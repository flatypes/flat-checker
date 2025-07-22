package flat.regex

final case class NatRange(lower: Int, upper: Option[Int]):
  require(lower >= 0 && upper.forall(_ >= 0))

  def isEmpty: Boolean = upper.exists(_ < lower)

  def isPoint: Boolean = upper.contains(lower)

  def +(k: Int): NatRange =
    require(k >= 0)
    NatRange(lower + k, upper.map(_ + k))

  def +(that: NatRange): NatRange =
    val m1 = this.lower + that.lower
    val m2 = (this.upper, that.upper) match
      case (Some(n1), Some(n2)) => Some(n1 + n2)
      case _ => None
    NatRange(m1, m2)

  def *(that: NatRange): NatRange =
    val m1 = this.lower * that.lower
    val m2 = (this.upper, that.upper) match
      case (Some(n1), Some(n2)) => Some(n1 * n2)
      case _ => None
    NatRange(m1, m2)

  def |(that: NatRange): NatRange =
    val m1 = this.lower min that.lower
    val m2 = (this.upper, that.upper) match
      case (Some(n1), Some(n2)) => Some(n1 max n2)
      case _ => None
    NatRange(m1, m2)

  def &(that: NatRange): NatRange =
    val m1 = this.lower max that.lower
    val m2 = (this.upper, that.upper) match
      case (Some(n1), Some(n2)) => Some(n1 min n2)
      case (Some(n), None) => Some(n)
      case (None, Some(n)) => Some(n)
      case (None, None) => None
    NatRange(m1, m2)

  def -(k: Int): NatRange =
    require(k >= 0 && upper.forall(k <= _))
    NatRange(0 max (lower - k), upper.map(_ - k))

  override def toString: String = upper match
    case Some(n) if n == lower => s"{$n}"
    case Some(m) => s"{$lower,$m}"
    case None => s"{$lower,}"

object NatRange:
  def apply(from: Int, to: Int): NatRange = NatRange(from, Some(to))

  def apply(from: Int): NatRange = NatRange(from, None)

  def at(k: Int): NatRange = NatRange(k, Some(k))
