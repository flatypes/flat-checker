package flat.checker.abs

import flat.checker.ast.Type
import flat.checker.{Issuer, Location}

class State(private val stack: Seq[Map[String, (AVal, AVal)]])(using issuer: Issuer):
  def apply(name: String): AVal =
    val i = stack.indexWhere(_.contains(name))
    stack(i)(name)._2

  def push: State = State(Map.empty +: stack)

  def pop: State = State(stack.tail)

  def added(name: String, typ: Type): State =
    val ub = AVal.fromType(typ)
    val m = stack.head + (name -> (ub, ub))
    State(m +: stack.tail)

  def updated(name: String, newVal: AVal, valLoc: Location): State =
    val i = stack.indexWhere(_.contains(name))
    val ub = stack(i)(name)._1
    if !(newVal :<: ub) then
      issuer.report(TypeMismatch(valLoc, ub.show, newVal.show))
    State(stack.updated(i, stack(i) + (name -> (ub, newVal))))

  def zipValues(other: State): Seq[(AVal, AVal)] =
    require(stack.length == other.stack.length,
      s"inconsistent stack: level different (${stack.length} != ${other.stack.length})")
    for
      (m1, m2) <- stack zip other.stack
      _ = require(m1.keySet == m2.keySet, s"inconsistent stack: keys different (${m1.keySet} != ${m2.keySet}")
      x <- m1.keySet
      (ub1, v1) = m1(x)
      (ub2, v2) = m2(x)
      _ = require(ub1 == ub2, s"inconsistent stack: ub of $x is different (${ub1.show} != ${ub2.show})")
    yield (v1, v2)

  def mergeValues(f: (AVal, AVal) => AVal)(other: State): State =
    require(stack.length == other.stack.length,
      s"inconsistent stack: level different (${stack.length} != ${other.stack.length})")
    val joined = for (m1, m2) <- stack zip other.stack yield
      require(m1.keySet == m2.keySet, s"inconsistent stack: keys different (${m1.keySet} != ${m2.keySet}")
      Map.from(
        for
          x <- m1.keySet
          (ub1, v1) = m1(x)
          (ub2, v2) = m2(x)
          _ = require(ub1 == ub2, s"inconsistent stack: ub of $x is different (${ub1.show} != ${ub2.show})")
        yield x -> (ub1, f(v1, v2))
      )
    State(joined)

object State:
  def from(locals: Seq[(String, Type)])(using issuer: Issuer): State =
    val m = Map.from(for (x, t) <- locals yield x -> (AVal.fromType(t), AVal.fromType(t)))
    State(Seq(m))

given AbsDom[State]:
  def top: State = throw UnsupportedOperationException("top of State")

  def bot: State = State(Seq.empty)(using issuer = new Issuer)

  def subElement(s1: State, s2: State): Boolean = s1.zipValues(s2).forall(_ :<: _)

  def join(s1: State, s2: State): State = s1.mergeValues(_ | _)(s2)

  def widen(s1: State, s2: State): State = s1.mergeValues(_ ∇ _)(s2)

