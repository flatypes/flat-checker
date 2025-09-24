package flat.regex

import flat.regex.RegEx.*

import scala.annotation.tailrec

/**
 * RE subset checking.
 *
 * Implementation is based on the following paper:
 * Matthias Keil and Peter Thiemann. 2014. "Symbolic Solving of Extended Regular Expression Inequalities."
 * https://arxiv.org/abs/1410.3227
 */
object RESub:
  /** Checks if `r1` is a subset of `r2`. */
  def check(r1: RegEx, r2: RegEx): Boolean = prove(List((r1, r2)), Nil)

  /** The main decision procedure, realized in a tail-recursive fashion. */
  @tailrec
  private def prove(goals: List[(RegEx, RegEx)], ctx: List[(RegEx, RegEx)]): Boolean = goals match
    case Nil => true
    case goal :: rest if ctx.contains(goal) => prove(rest, ctx)
    case (r1, r2) :: rest if r1 == r2 || r1 == RENull && r2.nullable => prove(rest, ctx :+ (r1, r2))
    case (r1, r2) :: rest if r1.nullable && !r2.nullable => false
    case (r1, r2) :: rest =>
      var disproved = false
      val subGoals =
        for
          cs <- (next(r1) <| next(r2)).toList
          if !cs.isEmpty
          c = cs.head
          r11 = r1.derivative(c)
          if !r11.isEmpty
          r21 = r2.derivative(c)
          _ = if r21.isEmpty then disproved = true
        yield (r11, r21)
      if disproved then false else prove(rest ++ subGoals, ctx :+ (r1, r2))

  /** A partition of Unicode characters: a nonempty set of mutually disjoint charsets. */
  private type Partition = Set[CharSet]

  extension (p1: Partition)
    /** Join: given two partitions, compute a refined partition that covers their union. */
    private def |(p2: Partition): Partition =
      val cs1 = p1.reduce(_ | _)
      val cs2 = p2.reduce(_ | _)
      for
        x <- p1
        y <- p2
        z <- Seq(x & y, x & !cs2, !cs1 & y)
      yield z

    /** Left join: a left-biased version of `join` that only covers the characters of `p1`. */
    private def <|(p2: Partition): Partition =
      val cs2 = p2.reduce(_ | _)
      for
        x <- p1
        y <- p2
        z <- Seq(x & y, x & !cs2)
      yield z

  /** Next literals: compute a partition of the alphabet of `re` such that:
   *  1. for any nonempty charset `cs` in the partition, all chars in `cs` give the same derivative,
   *     i.e., `cs` forms an equivalent class under derivation;
   *  1. for any other character (not presented), they give the same derivative ∅.
   */
  private def next(re: RegEx): Partition =
    re match
      case RENone => throw IllegalArgumentException(re.toString)
      case RENull => Set(CharSet.empty)
      case RELit(cs) => Set(cs)
      case REConcat(r1, r2) => if r1.nullable then next(r1) | next(r2) else next(r1)
      case REUnion(r1, r2) => next(r1) | next(r2)
      case REStar(r) => next(r)
