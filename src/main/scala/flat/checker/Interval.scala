package flat.checker

import flat.checker.Bound.*

final case class Interval(lb: Bound, ub: Bound):
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
  def +(that: Interval): Interval =
    if isEmpty || that.isEmpty then Interval.empty
    else Interval(lb + that.lb, ub + that.ub)

  /** Interval subtraction: `[a, b] - [c, d] = [a - d, b - c]` */
  def -(that: Interval): Interval =
    if isEmpty || that.isEmpty then Interval.empty
    else Interval(lb - that.ub, ub - that.lb)

  def <(that: Interval): Ternary =
    if ub < that.lb then Ternary.True
    else if that.ub <= lb then Ternary.False
    else Ternary.Maybe

  infix def equiv(that: Interval): Ternary =
    val k1 = lb max that.lb
    val k2 = ub min that.ub
    if k1 == k2 then Ternary.True
    else if k1 > k2 then Ternary.False
    else Ternary.Maybe

  override def toString: String = s"[$lb, $ub]"

object Interval:
  val empty: Interval = Interval(PosInf, NegInf)

  val full: Interval = Interval(NegInf, PosInf)

  def fromInt(value: Int): Interval = Interval(value, value)

  def fromRange(range: scala.Range): Interval = Interval(range.start, range.end)

  given Conversion[Int, Interval] = fromInt

given AbsDom[Interval]:
  def top: Interval = Interval.full

  def bot: Interval = Interval.empty

  def subElement(r1: Interval, r2: Interval): Boolean = r2.lb <= r1.lb && r1.ub <= r2.ub

  def join(r1: Interval, r2: Interval): Interval = Interval(r1.lb min r2.lb, r1.ub max r2.ub)

  def meet(r1: Interval, r2: Interval): Interval = Interval(r1.lb max r2.lb, r1.ub min r2.ub)

  def widen(r1: Interval, r2: Interval): Interval =
    if r1.isEmpty then r2
    else if r2.isEmpty then r1
    else
      val lb = if r1.lb <= r2.lb then r1.lb else NegInf
      val ub = if r2.ub <= r1.ub then r1.ub else PosInf
      Interval(lb, ub)
