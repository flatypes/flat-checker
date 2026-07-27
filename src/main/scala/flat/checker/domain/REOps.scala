package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.RegEx.*

import scala.annotation.tailrec

object REOps extends LazyLogging:
  extension [T, D](r: RegEx[T, D])(using domain: Domain[T, D])
    /** Abstract operation for `s.length`. */
    def absLength: RegEx[Int, Int] = r match
      case REZero() => REZero()
      case REOne() => REOne()
      case RELit(d) => if d.isEmpty then REZero() else RELit(1)
      case REPlus(r1, r2) => r1.absLength + r2.absLength
      case REComp(r1, r2) => r1.absLength * r2.absLength
      case REStar(r1) => r1.absLength.star

    /** Abstract operation for `s.drop(n)`. */
    @tailrec
    def absDrop(n: Int): RegEx[T, D] =
      require(0 <= n)
      if n == 0 then r
      else
        val r1 = r.deriv(_.nonEmpty)
        if r1.isEmpty then REOne() else r1.absDrop(n - 1)

    /** Abstract operation for `s(i)`. */
    def absAt(i: Int): D =
      require(0 <= i)
      r.absDrop(i).first

    def absAtRight(i: Int): D =
      require(0 <= i)
      r.reverse.absDrop(i).first

    /** Abstract operation for `s.take(n)`. */
    def absTake(n: Int): RegEx[T, D] =
      require(n >= 0)
      if n == 0 then REOne()
      else
        val result1: RegEx[T, D] = if r.nullable then REOne() else REZero() // s is empty
        val result2 = sum(for (d, r1) <- r.absSplitAt1.toList yield RELit(d) * r1.absTake(n - 1)) // s is nonempty
        result1 + result2

    /** Abstract operation for `s.splitAt(1)`. */
    def absSplitAt1: List[(D, RegEx[T, D])] = r match
      case REZero() | REOne() => Nil
      case RELit(d) => if d.isEmpty then Nil else List(d -> REOne())
      case REPlus(r1, r2) => r1.absSplitAt1 ++ r2.absSplitAt1
      case REComp(r1, r2) =>
        (for (a, r1r) <- r1.absSplitAt1 yield (a, r1r * r2)) ++ (if r1.nullable then r2.absSplitAt1 else Nil)
      case REStar(r1) =>
        for (a, r1r) <- r1.absSplitAt1 yield (a, r1r * r)

    /** Abstract operation for `s.slice(i, j)`. */
    def absSlice(i: Int, j: Int): RegEx[T, D] =
      require(0 <= i && i <= j)
      r.absDrop(i).absTake(j - i)

    /** Abstract operation for `s.startsWith(t)`. */
    def absStartsWith(t: List[T]): BoolSet =
      if filterStartsWith(t).isEmpty then BoolSet.False
      else if filterNotStartWith(t).isEmpty then BoolSet.True
      else BoolSet.Full

    /** Abstract operation for `s.endsWith(t)`. */
    def absEndsWith(t: List[T]): BoolSet = r.reverse.absStartsWith(t.reverse)

    // Find
    /** Abstract operation for `s.contains(x)`. */
    def absContains(t: List[T]): BoolSet =
      if r.filterContains(t).isEmpty then BoolSet.False
      else if r.filterNotContain(t).isEmpty then BoolSet.True
      else BoolSet.Full

    private def findFirst(t: List[T]): List[(RegEx[T, D], RegEx[T, D])] =
      val forbiddenPrefixes =
        for n <- 1.until(t.length).toList; if t.endsWith(t.take(n)) yield t.dropRight(n)
      for
        (r1, r2) <- findAny(t.head); r2f = r2.filterStartsWith(t.tail); if r2f.nonEmpty
        r1f1 = forbiddenPrefixes.foldLeft(r1)(_.filterNotEndWith(_)); if r1f1.nonEmpty
        r1f = r1f1.filterNotContain(t); if !r1f.isEmpty
      yield (r1f, r2f.deriv(t.tail))

    def dropIndexOf(t: List[T]): RegEx[T, D] =
      RegEx(t) * sum(r.findFirst(t).map(_._2))

    def takeIndexOf(t: List[T]): RegEx[T, D] =
      sum(r.findFirst(t).map(_._1))

    /** Abstract operation for `s.indexOf(x)`. */
    def absIndexOf(t: List[T]): (Boolean, RegEx[Int, Int]) =
      val mustContain = r.filterNotContain(t).isEmpty
      (mustContain, takeIndexOf(t).absLength)

    /** Abstract operation for `s.count(t)`. */
    def absCount(t: List[T]): RegEx[Int, Int] = t match
      case Nil => throw IllegalArgumentException("absCount: t must be non-empty")
      case _ => CountSolver(t).solve(r)

    /** Abstract operation for `s.split(t)`. */
    def absSplit(t: List[T]): RegEx[List[T], RegEx[T, D]] = t match
      case Nil => RELit(r)
      case _ => SplitSolver(t).solve(r)

    /** Abstract operation for `s.map(f)`. */
    def absMap(f: D => D): RegEx[T, D] = r match
      case REZero() | REOne() => r
      case RELit(d) => RELit(f(d))
      case REPlus(r1, r2) => r1.absMap(f) + r2.absMap(f)
      case REComp(r1, r2) => r1.absMap(f) * r2.absMap(f)
      case REStar(r1) => r1.absMap(f).star

    // Filters
    /** Computes {s ∈ r | s starts with t}. */
    def filterStartsWith(t: List[T]): RegEx[T, D] = t match
      case Nil => r
      case _ =>
        val r1 = r.deriv(t)
        if r1.isEmpty then REZero() else RegEx(t) * r1

    /** Computes {s ∈ r | s not start with t}. */
    def filterNotStartWith(t: List[T]): RegEx[T, D] = t match
      case Nil => REZero()
      case x :: xs =>
        // case 1: empty
        val result1: RegEx[T, D] = if r.nullable then REOne() else REZero()
        // case 2: not start with x
        val result2 = sum(for (a, r1) <- r.absSplitAt1 yield lit(a - x) * r1)
        // case 3: start with x but the rest not start with xs
        val r1 = r.deriv(x)
        val result3 = if r1.isEmpty then REZero() else r1.filterNotStartWith(xs)
        result1 + result2 + result3

    /** Computes {s ∈ r | s ends with t}. */
    def filterEndsWith(t: List[T]): RegEx[T, D] = r.reverse.filterStartsWith(t.reverse).reverse

    /** Computes {s ∈ r | s not end with t}. */
    def filterNotEndWith(t: List[T]): RegEx[T, D] = r.reverse.filterNotStartWith(t.reverse).reverse

    /** Computes {s ∈ r | s = t}. */
    def filterEq(t: List[T]): RegEx[T, D] = if r.contains(t) then RegEx(t) else REZero()

    /** Computes {s ∈ r | s ≠ t}. */
    def filterNe(t: List[T]): RegEx[T, D] = RegEx(t) * r.deriv(t).filterNonEmpty + r.filterNotStartWith(t)

    /** Computes {s ∈ r | s ≠ []}. */
    private def filterNonEmpty: RegEx[T, D] = r match
      case REZero() | REOne() => REZero()
      case RELit(_) => r
      case REPlus(r1, r2) => r1.filterNonEmpty + r2.filterNonEmpty
      case REComp(r1, r2) => r1.filterNonEmpty * r2 + (if r1.nullable then r2.filterNonEmpty else REZero())
      case REStar(r1) => r1.filterNonEmpty + r

    private def findAny(x: T): List[(RegEx[T, D], RegEx[T, D])] = r match
      case REZero() | REOne() => Nil
      case RELit(d) => if d.contains(x) then List((REOne(), REOne())) else Nil
      case REPlus(r1, r2) => r1.findAny(x) ++ r2.findAny(x)
      case REComp(r1, r2) =>
        (for (r1l, r1r) <- r1.findAny(x) yield (r1l, r1r * r2)) ++
          (for (r2l, r2r) <- r2.findAny(x) yield (r1 * r2l, r2r))
      case REStar(r1) =>
        for (r1l, r1r) <- r1.findAny(x) yield (r * r1l, r1r * r)

    /** Computes { s ∈ r | s contains t }. */
    def filterContains(t: List[T]): RegEx[T, D] = t match
      case Nil => r
      case x :: xs =>
        sum(for (r1, r2) <- r.findAny(x) yield r1 * RegEx(x) * r2.filterStartsWith(xs))

    /** Computes { s ∈ r | s not contain t }. */
    def filterNotContain(t: List[T]): RegEx[T, D] = t match
      case Nil => REZero()
      case List(x) => r.filterNotContain1(x)
      case _ => r.filterNotContain2(t)

    private def filterNotContain1(x: T): RegEx[T, D] = r match
      case REZero() | REOne() => r
      case RELit(d) => lit(d - x)
      case REPlus(r1, r2) => r1.filterNotContain1(x) + r2.filterNotContain1(x)
      case REComp(r1, r2) => r1.filterNotContain1(x) * r2.filterNotContain1(x)
      case REStar(r1) => r1.filterNotContain1(x).star

    private def filterNotContain2(t: List[T]): RegEx[T, D] =
      require(t.length >= 2)
      r match
        case REZero() | REOne() | RELit(_) => r
        case REPlus(r1, r2) => r1.filterNotContain2(t) + r2.filterNotContain2(t)
        case REComp(r1, r2) if !r1.alphabet.contains(t.head) => r1 * r2.filterNotContain2(t)
        case _ =>
          val alphabet = r.alphabet
          if t.forall(alphabet.contains) then NotContainSolver(t).solve(r) else r

  private class CountSolver[T, D](t: List[T])(using Domain[T, D]) extends LinearSolver[T, D, Int, Int]:
    require(t.nonEmpty)

    override protected def build(input: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      // count(r) = {0} (if r may not contain t) ∪ {1 + n | (_, r2) ∈ r.find(t), n ∈ count(r2)}
      // In RegEx[Int, Int]: {0} = REOne, {1 + n | n ∈ count(r2)} = RELit(1) * count(r2)
      (if input.filterNotContain(t).nonEmpty then REOne() else REZero(),
        for (_, r2) <- input.findFirst(t) yield (RELit(1), r2))

  private class SplitSolver[T, D](t: List[T])(using Domain[T, D]) extends LinearSolver[T, D, List[T], RegEx[T, D]]:
    override protected def build(input: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      // split(r) = {[s] | s ∈ rf} (where rf = r ∩ {s | s not contain t})
      //          ∪ {[s1] ++ ss | (r1, r2) ∈ r.find(t), s1 ∈ r1, ss ∈ split(r2)}
      // In RE notation: {[s] | s ∈ rf} = RELit(rf), {[s1] ++ ss | s1 ∈ r1, ss ∈ split(r2)} = RELit(r1) * split(r2)
      val rf = input.filterNotContain(t)
      (if rf.nonEmpty then RELit(rf) else REZero(),
        for (r1, r2) <- input.findFirst(t) yield (RELit(r1), r2))

  private class NotContainSolver[T, D](t: List[T])(using Domain[T, D]) extends LinearSolver[T, D, T, D]:
    require(t.nonEmpty)

    override protected def build(r: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      val x = t.head
      val a = r.first
      val b = a - x
      // notContain(r) = REOne (if r is nullable)
      //               + x * notContain(r1) (where r1 = r.deriv(x) ∩ {s | s not start with t1})
      //               + (r.first - x) * notContain(r.deriv([^x]))
      (if r.nullable then REOne() else REZero(),
        (if a.contains(x) then List((RegEx(x), r.deriv(x).filterNotStartWith(t.tail))) else Nil) ++
          (if b.nonEmpty then List((RELit(b), r.deriv(d => (d - x).nonEmpty))) else Nil))

  given Conversion[String, List[Char]] = _.toList
