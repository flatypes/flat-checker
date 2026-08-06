package flat.checker.domain

final case class NatRange(min: Int, max: Option[Int]):
  def +(that: NatRange): NatRange =
    val newMin = min + that.min
    val newMax = (max, that.max) match
      case (Some(m1), Some(m2)) => Some(m1 + m2)
      case _ => None
    NatRange(newMin, newMax)

  def |(that: NatRange): NatRange =
    val newMin = min min that.min
    val newMax = (max, that.max) match
      case (Some(m1), Some(m2)) => Some(Math.max(m1, m2))
      case _ => None
    NatRange(newMin, newMax)

object NatRange:
  def at(n: Int): NatRange = NatRange(n, Some(n))
