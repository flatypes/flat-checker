package flat.checker.domain

import flat.checker.domain.RegEx.*

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

final case class IdxSet(negative: Boolean, left: NatSet, right: NatSet)

/** A regular expression that encodes a (possibly infinite) set of natural numbers:
 *  - L(0) = ∅
 *  - L(1) = {0}
 *  - L(One) = {1}
 *  - L(r1 + r2) = L(r1) ∪ L(r2)
 *  - L(r1 * r2) = { n1 + n2 | n1 ∈ L(r1), n2 ∈ L(r2) }
 *  - L(r*) = {0} ∪ { n * k | n ∈ L(r), k ∈ ℕ }
 * */
type CountingRE = RegEx[Set[Int]]

extension (r: CountingRE)
  def isFinite: Boolean = r match
    case Zero() | One() | Lit(_) => true
    case Star(_) => false
    case Plus(r1, r2) => r1.isFinite && r2.isFinite
    case Comp(r1, r2) => r1.isFinite && r2.isFinite

  def toFinSet: Set[Int] = r match
    case Zero() => Set.empty
    case One() => Set(0)
    case Lit(a) => a
    case Plus(r1, r2) => r1.toFinSet ++ r2.toFinSet
    case Comp(r1, r2) => for n1 <- r1.toFinSet; n2 <- r2.toFinSet yield n1 + n2
    case Star(r1) => throw IllegalArgumentException("not a finite set")

  def min: Int = r match
    case Zero() => throw IllegalArgumentException("empty set has no minimum")
    case One() | Star(_) => 0
    case Lit(a) => a.min
    case Plus(r1, r2) => r1.min min r2.min
    case Comp(r1, r2) => r1.min + r2.min

  def toNatSet: NatSet = r match
    case Zero() => NatSet.empty
    case One() => NatSet(Set(0), false)
    case Lit(a) => NatSet(a, false)
    case Plus(r1, r2) => r1.toNatSet | r2.toNatSet
    case Comp(r1, r2) => r1.toNatSet + r2.toNatSet
    case Star(_) => NatSet(Set(0), true)

final case class IndexSet(neg: Set[Int], pos: CountingRE, right: CountingRE)