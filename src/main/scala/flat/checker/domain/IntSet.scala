package flat.checker.domain

import flat.checker.domain.RegEx.*

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

final case class IndexSet(neg: Set[Int], pos: CountingRE)