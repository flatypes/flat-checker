package flat.checker

import flat.checker.ast.Type

final case class FunInfo(paramTypes: Seq[Type], returnType: Type, loc: Location)

final class GlobalContext(private val data: Map[String, FunInfo]):
  def lookup(name: String): Option[FunInfo] = data.get(name)

  def apply(name: String): FunInfo = data(name)

  def declare(name: String, info: FunInfo): GlobalContext =
    if data.contains(name) then
      throw IllegalArgumentException(s"function '$name' is already defined")
    GlobalContext(data + (name -> info))

object GlobalContext:
  val empty = GlobalContext(Map.empty)

final case class VarInfo(declaredType: Type, latestType: Type)

final case class LocalContext(currentFun: String, private val stack: Seq[Map[String, VarInfo]]):
  private def find(name: String): Option[(Int, VarInfo)] =
    stack.indices.collectFirst { case i if stack(i).contains(name) => (i, stack(i)(name)) }

  def lookup(name: String): Option[VarInfo] = find(name).map(_._2)

  def push: LocalContext = copy(stack = Map.empty +: stack)

  def pop: LocalContext = copy(stack = stack.tail)

  def declare(name: String, typ: Type): LocalContext =
    if stack.contains(name) then
      throw IllegalArgumentException(s"variable '$name' is already defined")
    val m = stack.head + (name -> VarInfo(typ, typ))
    copy(stack = m +: stack.tail)

  def update(name: String, typ: Type): LocalContext =
    find(name) match
      case Some(i, info) =>
        val m = stack(i) + (name -> info.copy(latestType = typ))
        copy(stack = stack.updated(i, m))
      case None =>
        throw IllegalArgumentException(s"variable '$name' not found")

  def |(other: LocalContext): LocalContext =
    require(stack.length == other.stack.length,
      s"inconsistent stack: level different (${stack.length} != ${other.stack.length})")
    val joined = for (m1, m2) <- stack zip other.stack yield
      require(m1.keySet == m2.keySet, s"inconsistent stack: keys different (${m1.keySet} != ${m2.keySet}")
      Map.from(
        for
          x <- m1.keySet
          VarInfo(t1, v1) = m1(x)
          VarInfo(t2, v2) = m2(x)
          _ = require(t1 == t2, s"inconsistent stack: types of $x is different (${t1} != ${t2})")
        yield x -> VarInfo(t1, v1 | v2)
      )
    LocalContext(currentFun, joined)

object LocalContext:
  def apply(currentFun: String): LocalContext = LocalContext(currentFun, Seq(Map.empty))
