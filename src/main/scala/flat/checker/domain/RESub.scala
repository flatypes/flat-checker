package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.RegEx.*

import scala.collection.mutable

object RESub extends LazyLogging:
  /** Checks if `r1` ⊆ `r2`. */
  def check[T, D](r1: RegEx[T, D], r2: RegEx[T, D])(using domain: Domain[T, D]): Boolean =
    val queue = mutable.Queue.empty[(RegEx[T, D], RegEx[T, D])]
    val visited = mutable.Set.empty[(RegEx[T, D], RegEx[T, D])]

    queue.enqueue((r1, r2))
    while queue.nonEmpty do
      if queue.size > 100 then
        throw RuntimeException("queue size exceeded 100")

      val (r1, r2) = queue.dequeue()
      if r1.nullable && !r2.nullable then
        logger.debug("disprove by null: {} ⊈ {}", r1.pp, r2.pp)
        return false

      val first1 = r1.first
      if !first1.subsetOf(r2.first) then
        logger.debug("disprove by first: {} ⊈ {}", r1.pp, r2.pp)
        return false

      if first1.isEmpty || r1 == r2 then
        logger.debug("prove by trivial: {} ⊆ {}", r1.pp, r2.pp)
      else if visited.contains((r1, r2)) then
        logger.debug("prove by cycle: {} ⊆ {}", r1.pp, r2.pp)
      else
        for a <- next(r1) <| next(r2) do
          val c = a.representative
          queue.enqueue((r1.deriv(c), r2.deriv(c)))
        visited.add((r1, r2))

    true

  /** Equivalence class partitioning on the alphabet.
   * If the alphabet is empty, then the partition is empty.
   * Otherwise, for any equivalence class C in partition, the derivative of any c ∈ C is the same;
   * other characters form a special equivalence class where their derivatives are ∅. */
  private type Partition[D] = List[D]

  /** Computes the equivalence class partitioning on the first set of `r`. */
  private def next[T, D](r: RegEx[T, D])(using Lattice[D]): Partition[D] = r match
    case REZero() | REOne() => Nil
    case RELit(a) => List(a)
    case REPlus(r1, r2) => next(r1) | next(r2)
    case REComp(r1, r2) => if r1.nullable then next(r1) | next(r2) else next(r1)
    case REStar(r1) => next(r1)

  extension [D](p1: Partition[D])(using Lattice[D])
    /** Joins two partitions into one. */
    private def |(p2: Partition[D]): Partition[D] =
      if p1.isEmpty then p2
      else if p2.isEmpty then p1
      else
        val comp1 = ~p1.reduce(_ | _)
        val comp2 = ~p2.reduce(_ | _)
        (for x <- p1; y <- p2; z = x & y; if z.nonEmpty yield z) ++
          (for x <- p1; z = x & comp2; if z.nonEmpty yield z) ++
          (for y <- p2; z = y & comp1; if z.nonEmpty yield z)

    /** Left-biased join. Only include characters of `p1`. */
    private def <|(p2: Partition[D]): Partition[D] =
      require(p1.nonEmpty && p2.nonEmpty)
      val comp2 = ~p2.reduce(_ | _)
      (for x <- p1; y <- p2; z = x & y; if z.nonEmpty yield z) ++
        (for x <- p1; z = x & comp2; if z.nonEmpty yield z)