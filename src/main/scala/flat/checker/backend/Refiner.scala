package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker
import flat.checker.Bound.*
import flat.checker.ReLang.*
import flat.checker.backend.core.*
import flat.checker.backend.core.CmpOp.*
import flat.checker.{&, CharSet, Interval, ReLang, given}

object Refiner extends LazyLogging:
  def refine(base: ReLang, withRefine: ReLangRefine): ReLang = withRefine(base)

  def refine(name: String, declared: ReLang, premises: List[Expr]): ReLang =
    for p <- premises do logger.debug(p.toString)
    var r = declared
    if premises.flatMap(_.collect { case StrLen(Var(x)) if x == name => x }).nonEmpty then
      val (lb, ub) = LPSolver.solve(StrLen(name), premises)
      if lb > Fin(0) || ub < PosInf then
        val interval = Interval(lb, ub)
        logger.debug(s"|$name| in $interval")
        r = refine(declared, LenIn(interval))
    for
      e <- premises
      f <- collectRefine(name, e)
    do r = refine(r, f)
    r

  private def collectRefine(name: String, premise: Expr): Option[ReLangRefine] = premise match
    case Cmp(LT, StrFind(Var(x), Const(c: String), Const(0)), Const(0)) if x == name && c.length == 1 =>
      Some(NotContain(c.head))
    case Not(Cmp(LT, StrFind(Var(x), Const(c: String), Const(0)), Const(0))) if x == name && c.length == 1 =>
      Some(Contain(c.head))
    case _ => None

trait ReLangRefine:
  def apply(base: ReLang): ReLang

final case class LenIn(interval: Interval) extends ReLangRefine:
  require(interval.lb >= 0, interval.toString)

  def apply(base: ReLang): ReLang =
    if (base.length & interval).isEmpty then
      return ReNone
    if interval.ub == Fin(0) then
      return ReEmpty
    base match
      case ReUnion(r1, r2) =>
        ReUnion(apply(r1), apply(r2)).normalize
      case ReConcat(r1, r2) if r1.length.isInt && !r2.length.isInt =>
        val r21 = LenIn(safeSub(interval, r1.length))(r2)
        ReConcat(r1, r21)
      case ReConcat(r1, r2) if r2.length.isInt && !r1.length.isInt =>
        val r11 = LenIn(safeSub(interval, r2.length))(r1)
        ReConcat(r11, r2)
      case ReStar(r) =>
        val i = r.length.ub match
          case Fin(k) => Math.ceilDiv(interval.lb.asInt, k)
          case PosInf => 1
          case _ => assert(false)
        val j = (interval.ub, r.length.lb.asInt) match
          case (PosInf, _) => PosInf
          case (Fin(m), 0) => PosInf // better: (r \ {ε}) ^ m
          case (Fin(m), k) => Fin(Math.floorDiv(m, k))
          case _ => assert(false)
        r.loop(i, j).normalize
      case _ => base

  private def safeSub(i: Interval, j: Interval): Interval =
    val k = i - j
    if k.lb < 0 then Interval(0, k.ub) else k

final case class Contain(char: Char) extends ReLangRefine:
  def apply(base: ReLang): ReLang =
    base match
      case ReNone | ReEmpty => ReNone
      case ReChars(cs) =>
        if cs.contains(char) then ReChars(CharSet.of(char)) else ReNone
      case ReConcat(r1, r2) =>
        val r11 = apply(r1)
        val r21 = apply(r2)
        if r11.isEmpty && !r21.isEmpty then ReConcat(r1, r21)
        else if r21.isEmpty && !r11.isEmpty then ReConcat(r11, r2)
        else base
      case ReUnion(r1, r2) =>
        ReUnion(apply(r1), apply(r2)).normalize
      case ReStar(r) =>
        val r1 = apply(r)
        if r1.isEmpty then ReNone else ReConcat(r, base)

final case class NotContain(chars: Set[Char]) extends ReLangRefine:
  def apply(base: ReLang): ReLang =
    base match
      case ReNone => ReNone
      case ReEmpty => ReEmpty
      case ReChars(cs) =>
        val cs1 = cs -- chars
        if cs1.isEmpty then ReNone else ReChars(cs1)
      case ReConcat(r1, r2) => ReConcat(apply(r1), apply(r2))
      case ReUnion(r1, r2) => ReUnion(apply(r1), apply(r2))
      case ReStar(r) => ReStar(apply(r))

object NotContain:
  def apply(char: Char): NotContain = NotContain(Set(char))
