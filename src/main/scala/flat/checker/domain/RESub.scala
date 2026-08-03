package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.RegEx.*

import scala.collection.mutable

object RESub extends LazyLogging:
  private val set = summon[SymbolSet[CharSet]]

  /** Checks if `r1` ⊆ `r2`. */
  def check(r1: StrRE, r2: StrRE): Boolean =
    val queue = mutable.Queue.empty[(StrRE, StrRE)]
    val visited = mutable.Set.empty[(StrRE, StrRE)]

    queue.enqueue((r1, r2))
    while queue.nonEmpty do
      if queue.size > 100 then
        throw RuntimeException("queue size exceeded 100")

      val (r1, r2) = queue.dequeue()
      if r1.nullable && !r2.nullable then
        logger.trace("disprove by null: {} ⊈ {}", r1.pp, r2.pp)
        return false

      val first1 = r1.first
      if !first1.subsetOf(r2.first) then
        logger.trace("disprove by first: {} ⊈ {}", r1.pp, r2.pp)
        return false

      if first1.isEmpty || r1 == r2 then
        logger.trace("prove by trivial: {} ⊆ {}", r1.pp, r2.pp)
      else if visited.contains((r1, r2)) then
        logger.trace("prove by cycle: {} ⊆ {}", r1.pp, r2.pp)
      else
        for a <- next(r1) <| next(r2) do
          val c: set.Symbol = a.representative
          queue.enqueue((r1.deriv(c), r2.deriv(c)))
        visited.add((r1, r2))

    true

  /** Equivalence class partitioning on the alphabet.
   * If the alphabet is empty, then the partition is empty.
   * Otherwise, for any equivalence class C in partition, the derivative of any c ∈ C is the same;
   * other characters form a special equivalence class where their derivatives are ∅. */
  private type Partition = List[CharSet]

  /** Computes the equivalence class partitioning on the first set of `r`. */
  private def next(r: StrRE): Partition = r match
    case Zero() | One() => Nil
    case Lit(a) => List(a)
    case Plus(r1, r2) => next(r1) | next(r2)
    case Comp(r1, r2) => if r1.nullable then next(r1) | next(r2) else next(r1)
    case Star(r1) => next(r1)

  extension (p1: Partition)
    /** Joins two partitions into one. */
    private def |(p2: Partition): Partition =
      if p1.isEmpty then p2
      else if p2.isEmpty then p1
      else
        val comp1 = ~p1.reduce(_ | _)
        val comp2 = ~p2.reduce(_ | _)
        (for x <- p1; y <- p2; z = x & y; if z.nonEmpty yield z) ++
          (for x <- p1; z = x & comp2; if z.nonEmpty yield z) ++
          (for y <- p2; z = y & comp1; if z.nonEmpty yield z)

    /** Left-biased join. Only include characters of `p1`. */
    private def <|(p2: Partition): Partition =
      require(p1.nonEmpty && p2.nonEmpty)
      val comp2 = ~p2.reduce(_ | _)
      (for x <- p1; y <- p2; z = x & y; if z.nonEmpty yield z) ++
        (for x <- p1; z = x & comp2; if z.nonEmpty yield z)