package flat.regex

import scala.annotation.tailrec

/** Check subtyping of two regular language types, i.e., if a regular language is a subset of another.
 * Ref: Matthias Keil and Peter Thiemann. 2014. "Symbolic Solving of Extended Regular Expression Inequalities."
 * https://arxiv.org/abs/1410.3227
 */
object RESub:

  import RegExpr.*

  /** A partition of the Unicode alphabet: a nonempty set of mutually disjoint char sets. */
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

    /** Left join: a left-biased version of `join` that only covers the chars of `p1`. */
    private def <|(p2: Partition): Partition =
      val cs2 = p2.reduce(_ | _)
      for
        x <- p1
        y <- p2
        z <- Seq(x & y, x & !cs2)
      yield z

  /** Next literals: given a regex `r`, compute a partition of the Unicode alphabet such that:
   * 1. for any nonempty char set `cs` in the partition, all chars in `cs` give the same derivative,
   * i.e., the char set `cs` forms an equivalent class under derivation;
   * 2. for any other char (not presented), they give the same derivative `∅`.
   */
  private def next(re: RegExpr): Partition =
    re match
      case RENone => throw IllegalArgumentException(re.toString)
      case RENull => Set(CharSet.empty)
      case REChar(cs) => Set(cs)
      case REConcat(r1, r2) => if r1.nullable then next(r1) | next(r2) else next(r1)
      case REUnion(r1, r2) => next(r1) | next(r2)
      case RELoop(NatRange(0, None), r) => next(r) // Kleene star
      case RELoop(NatRange(m1, m2), r) => next(desugarLoop(m1, m2, r))

  private def desugarLoop(lower: Int, upper: Option[Int], r: RegExpr): RegExpr =
    val rep = upper match
      case Some(m) => mkUnion(List.from(for k <- 0 to (m - lower) yield mkConcat(List.fill(k)(r))))
      case None => r.*
    mkConcat(List.fill(lower)(r) :+ rep)

  /** The decision procedure realized in a tail-recursive fashion. */
  @tailrec
  private def prove(goals: Seq[(RegExpr, RegExpr)], ctx: Seq[(RegExpr, RegExpr)]): Boolean = goals match
    case Seq() => true
    case goal +: rest if ctx.contains(goal) => prove(rest, ctx)
    case (r1, r2) +: rest if r1 == r2 || r1 == RENull && r2.nullable => prove(rest, ctx :+ (r1, r2))
    case (r1, r2) +: rest if r1.nullable && !r2.nullable => false
    case (r1, r2) +: rest =>
      var disproved = false
      val subGoals =
        for
          cs <- (next(r1) <| next(r2)).toSeq
          if !cs.isEmpty
          c = cs.getRepresentative
          r11 = REOps.derivative(r1, c).normalize
          if r11 != RENone
          r21 = REOps.derivative(r2, c).normalize
          _ = if r21 == RENone then disproved = true
        yield (r11, r21)
      if disproved then false else prove(rest ++ subGoals, ctx :+ (r1, r2))

  /** Entry of the decision procedure. */
  def check(r1: RegExpr, r2: RegExpr): Boolean = prove(Seq((r1, r2)), Seq.empty)