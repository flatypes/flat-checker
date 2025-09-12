package flat.regex

case object Inf:
  override def toString: String = "∞"

final case class Interval(lb: Int = 0, ub: Int | Inf.type = Inf):
  require(lb >= 0)
  require:
    ub match
      case n: Int => n >= 0
      case _ => true

  def isEmpty: Boolean = ub match
    case n: Int => lb > n
    case _ => false

  def isSingleton: Boolean = lb == ub

  def contains(elem: Int): Boolean = ub match
    case n: Int => lb <= elem && elem <= n
    case _ => lb <= elem

  def +(k: Int): Interval =
    require(k >= 0)
    Interval(
      lb + k,
      ub match
        case n: Int => n + k
        case _ => Inf
    )

  def -(k: Int): Interval =
    require(k >= 0)
    Interval(
      (lb - k) max 0,
      ub match
        case n: Int => (n - k) max 0
        case _ => Inf
    )

  def +(that: Interval): Interval = Interval(
    lb + that.lb,
    (ub, that.ub) match
      case (n1: Int, n2: Int) => n1 + n2
      case _ => Inf
  )

  def *(that: Interval): Interval = Interval(
    lb * that.lb,
    (ub, that.ub) match
      case (n1: Int, n2: Int) => n1 * n2
      case _ => Inf
  )

  def |(that: Interval): Interval = Interval(
    lb min that.lb,
    (ub, that.ub) match
      case (n1: Int, n2: Int) => n1 max n2
      case _ => Inf
  )

  def &(that: Interval): Interval = Interval(
    lb max that.lb,
    (ub, that.ub) match
      case (n1: Int, n2: Int) => n1 min n2
      case (n: Int, Inf) => n
      case (Inf, n: Int) => n
      case (Inf, Inf) => Inf
  )

  override def toString: String = ub match
    case n: Int if n == lb => s"{$n}"
    case _ => s"{$lb,$ub}"

object Interval:
  def at(k: Int): Interval = Interval(k, k)
