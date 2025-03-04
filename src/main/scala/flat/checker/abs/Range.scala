package flat.checker.abs

import flat.checker.Bound
import flat.checker.Bound.*

import scala.annotation.targetName

final case class Range(lb: Bound, ub: Bound):
  def isEmpty: Boolean = ub < lb

  def isInt: Boolean =
    (lb, ub) match
      case (Fin(k1), Fin(k2)) if k1 == k2 => true
      case _ => false

  def asInt: Int =
    require(isInt)
    lb.asInstanceOf[Fin].value

  def contains(value: Int): Boolean = (lb, ub) match
    case (NegInf, Fin(k)) => value <= k
    case (Fin(k1), Fin(k2)) => k1 <= value && value <= k2
    case (Fin(k), PosInf) => value >= k
    case (NegInf, PosInf) => true
    case _ => false

  extension (x: Bound)
    def +(y: Bound): Bound =
      (x, y) match
        case (Fin(k1), Fin(k2)) => Fin(k1 + k2)
        case (NegInf, PosInf) | (PosInf, NegInf) => throw ArithmeticException()
        case (NegInf, _) | (_, NegInf) => NegInf
        case (PosInf, _) | (_, PosInf) => PosInf

    def -(y: Bound): Bound =
      (x, y) match
        case (Fin(k1), Fin(k2)) => Fin(k1 - k2)
        case (NegInf, NegInf) | (PosInf, PosInf) => throw ArithmeticException()
        case (NegInf, _) | (_, PosInf) => NegInf
        case (PosInf, _) | (_, NegInf) => PosInf

  /** Interval addition: `[a, b] + [c, d] = [a + c, b + d]` */
  @targetName("add")
  def +(that: Range): Range =
    if isEmpty || that.isEmpty then Range.empty
    else Range(lb + that.lb, ub + that.ub)

  /** Interval subtraction: `[a, b] - [c, d] = [a - d, b - c]` */
  @targetName("sub")
  def -(that: Range): Range =
    if isEmpty || that.isEmpty then Range.empty
    else Range(lb - that.ub, ub - that.lb)

  override def toString: String = s"[$lb, $ub]"

object Range:
  val empty: Range = Range(PosInf, NegInf)

  val full: Range = Range(NegInf, PosInf)

  def fromInt(value: Int): Range = Range(Fin(value), Fin(value))

  @deprecated
  def from(constant: Int): Range = Range(Fin(constant), Fin(constant))

  @deprecated
  def from(begin: Option[Int], end: Option[Int]): Range =
    val lb = begin.map(Fin.apply).getOrElse(NegInf)
    val ub = end.map(Fin.apply).getOrElse(PosInf)
    Range(lb, ub)

  def fromScalaRange(range: scala.Range): Range = Range(Fin(range.start), Fin(range.end))

given AbsDom[Range]:
  def top: Range = Range.full
  
  def bot: Range = Range.empty

  def subElement(r1: Range, r2: Range): Boolean = r2.lb <= r1.lb && r1.ub <= r2.ub

  def join(r1: Range, r2: Range): Range = Range(r1.lb min r2.lb, r1.ub max r2.ub)

  def widen(r1: Range, r2: Range): Range =
    if r1.isEmpty then r2
    else if r2.isEmpty then r1
    else
      val lb = if r1.lb <= r2.lb then r1.lb else NegInf
      val ub = if r2.ub <= r1.ub then r1.ub else PosInf
      Range(lb, ub)
