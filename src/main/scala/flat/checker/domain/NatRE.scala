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
type NatRE = RegEx[Int, Int]

extension (r: NatRE)
  def isFinite: Boolean = r match
    case REZero() | REOne() | RELit(_) => true
    case REStar(_) => false
    case REPlus(r1, r2) => r1.isFinite && r2.isFinite
    case REComp(r1, r2) => r1.isFinite && r2.isFinite

  def toSet(bound: Int = 0): Set[Int] = r match
    case REZero() => Set.empty
    case REOne() => Set(0)
    case RELit(n) => Set(n)
    case REPlus(r1, r2) => r1.toSet(bound) ++ r2.toSet(bound)
    case REComp(r1, r2) => for n1 <- r1.toSet(bound); n2 <- r2.toSet(bound) yield n1 + n2
    case REStar(r1) =>
      // regarding r1.star as r1 ^ bound
      if bound == 0 then Set(0)
      else Set.from(for n <- r1.toSet(bound); k <- 0 to bound yield n * k)
