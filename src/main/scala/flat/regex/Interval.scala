package flat.regex

/** Infinity. */
case object Inf:
  override def toString: String = "∞"

/** Interval: a range of integers in between `lb` (can be -∞) and `ub` (can be +∞). */
final case class Interval(lb: Int | Inf.type = Inf, ub: Int | Inf.type = Inf) extends Domain:
  /** Tests if this interval is empty. */
  def isEmpty: Boolean = (lb, ub) match
    case (n1: Int, n2: Int) => n1 > n2
    case _ => false

  /** Tests if this interval is a singleton. */
  def isSingleton: Boolean = (lb, ub) match
    case (n1: Int, n2: Int) => n1 == n2
    case _ => false

  /** Tests if this interval contains the given `elem`. */
  def contains(elem: Int): Boolean =
    val b1 = lb match
      case n: Int => elem >= n
      case Inf => true
    val b2 = ub match
      case n: Int => elem <= n
      case Inf => true
    b1 && b2

  /** Union. */
  def |(that: Interval): Interval =
    val lb = (this.lb, that.lb) match
      case (n1: Int, n2: Int) => n1 min n2
      case _ => Inf
    val ub = (this.ub, that.ub) match
      case (n1: Int, n2: Int) => n1 max n2
      case _ => Inf
    Interval(lb, ub)

  /** Intersection. */
  def &(that: Interval): Interval =
    val lb = (this.lb, that.lb) match
      case (n1: Int, n2: Int) => n1 max n2
      case (Inf, n: Int) => n
      case (n: Int, Inf) => n
      case (Inf, Inf) => Inf
    val ub = (this.ub, that.ub) match
      case (n1: Int, n2: Int) => n1 min n2
      case (Inf, n: Int) => n
      case (n: Int, Inf) => n
      case (Inf, Inf) => Inf
    Interval(lb, ub)

  /** Addition. */
  def +(that: Interval): Interval =
    val lb = (this.lb, that.lb) match
      case (n1: Int, n2: Int) => n1 + n2
      case _ => Inf
    val ub = (this.ub, that.ub) match
      case (n1: Int, n2: Int) => n1 + n2
      case _ => Inf
    Interval(lb, ub)

  /** Addition with a point. */
  def +(point: Int): Interval = this + Interval.at(point)

  override def toString: String =
    val s1 = lb match
      case n: Int => n.toString
      case Inf => "-∞"
    val s2 = ub match
      case n: Int => n.toString
      case Inf => "+∞"
    s"[$s1, $s2]"

object Interval:
  /** Creates a singleton interval `k` to `k`. */
  def at(k: Int): Interval = Interval(k, k)
