package flat.checker.domain

final case class NatSet private(nats: Set[Int], infinite: Boolean):
  def isEmpty: Boolean = nats.isEmpty

  def nonEmpty: Boolean = nats.nonEmpty

  def contains(n: Int): Boolean = nats.contains(n) || (infinite && n > nats.max)

  def |(that: NatSet): NatSet =
    (this.infinite, that.infinite) match
      case (true, true) =>
        val upper = this.nats.max min that.nats.max
        NatSet(this.nats.filter(_ <= upper) | that.nats.filter(_ <= upper), true)
      case (true, false) =>
        val upper = this.nats.max
        NatSet(this.nats | that.nats.filter(_ <= upper), true)
      case (false, true) =>
        val upper = that.nats.max
        NatSet(this.nats.filter(_ <= upper) | that.nats, true)
      case (false, false) =>
        NatSet(this.nats | that.nats, false)

  /** Lifted arithmetic addition. */
  def +(that: NatSet): NatSet =
    NatSet(for n1 <- this.nats; n2 <- that.nats yield n1 + n2, this.infinite || that.infinite)

  def eq(n: Int): Boolean = !infinite && nats == Set(n)

object NatSet:
  val empty: NatSet = new NatSet(Set.empty, false)

  /** Create a compressed `NatSet`. */
  def apply(nats: Set[Int], infinite: Boolean): NatSet =
    if infinite then
      var upper = nats.max
      while nats.contains(upper - 1) do
        upper -= 1
      new NatSet(nats.filter(_ <= upper), true)
    else
      new NatSet(nats, false)

  def apply(nats: Int*): NatSet = new NatSet(nats.toSet, false)

  def union(sets: List[NatSet]): NatSet =
    if sets.isEmpty then NatSet.empty else sets.reduce(_ | _)

final case class IndexSet(negative: Boolean, left: NatSet, right: NatSet)
