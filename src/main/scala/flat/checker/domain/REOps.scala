package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.RegEx.*

object REOps extends LazyLogging:
  extension [A](r: RegEx[A])(using set: SymbolSet[A])
    /** Abstract operation for `s.length`. */
    def absLength: CountingRE = ???

    /** Abstract operation for `s.drop(n)`. */
    def absDrop(n: Int): RegEx[A] = ???

    def absDropRight(n: Int): RegEx[A] = ???

    /** Abstract operation for `s(i)`. */
    def absAt(i: Int): A = ???

    def absAtRight(i: Int): A = ???

    def absAt(index: Index1): A = ???

    /** Abstract operation for `s.take(n)`. */
    def absTake(n: Int): RegEx[A] = ???

    def absTakeRight(n: Int): RegEx[A] = ???

    /** Abstract operation for `s.splitAt(1)`. */
    def absSplitAt1: List[(A, RegEx[A])] = ???

    def absSplitAt(n: Int): List[(RegEx[A], RegEx[A])] = ???

    def absSplitAtLast(n: Int): List[(RegEx[A], RegEx[A])] = ???

    /** Abstract operation for `s.startsWith(t)`. */
    def absStartsWith(t: List[set.Symbol]): BoolSet = ???

    /** Abstract operation for `s.endsWith(t)`. */
    def absEndsWith(t: List[set.Symbol]): BoolSet = ???

    // Find
    /** Abstract operation for `s.contains(x)`. */
    def absContainsElem(x: set.Symbol): BoolSet = ???

    def absContains(t: List[set.Symbol]): BoolSet = ???

    def findFirst(t: List[set.Symbol]): List[(RegEx[A], RegEx[A])] = ???

    def findFirst(t: List[set.Symbol], start: Int): List[(RegEx[A], RegEx[A])] = ???

    def dropIndexOf(t: List[set.Symbol], start: Int = 0): RegEx[A] = ???

    def takeIndexOf(t: List[set.Symbol], start: Int = 0): RegEx[A] = ???

    /** Abstract operation for `s.indexOf(x)`. */
    def absIndexOf(t: List[set.Symbol], start: Int = 0): IndexSet = ???

    def absIndexOfElem(x: set.Symbol, start: Int): IndexSet = ???

    def absSlice(from: Int): RegEx[A] = ???

    def absSlice(fromIndexOf: List[set.Symbol], start: Int, offset: Int): RegEx[A] = ???

    def absSlice(fromLeft: Int, until: Int): RegEx[A] = ???

    def absSlice(fromLeft: Int, untilIndexOf: List[set.Symbol], start: Int, offset: Int): RegEx[A] = ???

    def absSlice(fromIndexOf: List[set.Symbol], start: Int, offset: Int, untilLast: Int): RegEx[A] = ???

    def absSlice(start: Index1, end: Index1): RegEx[A] = ???

    /** Abstract operation for `s.count(t)`. */
    def absCount(t: List[set.Symbol]): CountingRE = ???

    def absSplit(t: List[set.Symbol], limit: Int): RegEx[RegEx[A]] = ???

    /** Abstract operation for `s.split(t)`. */
    def absSplit(t: List[set.Symbol]): RegEx[RegEx[A]] = ???

    /** Abstract operation for `s.map(f)`. */
    def absMap(f: A => A): RegEx[A] = ???

    // Filters
    /** Computes {s ∈ r | s starts with t}. */
    def filterStartsWith(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s not start with t}. */
    def filterNotStartWith(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s ends with t}. */
    def filterEndsWith(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s not end with t}. */
    def filterNotEndWith(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s = t}. */
    def filterEq(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s ≠ t}. */
    def filterNe(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes {s ∈ r | s ≠ []}. */
    def filterNonEmpty: RegEx[A] = ???

    private def findAny(x: set.Symbol): List[(RegEx[A], RegEx[A])] = ???

    /** Computes { s ∈ r | s contains t }. */
    def filterContains(t: List[set.Symbol]): RegEx[A] = ???

    /** Computes { s ∈ r | s not contain t }. */
    def filterNotContain(t: List[set.Symbol]): RegEx[A] = ???

    def filterNotContainElem(x: set.Symbol): RegEx[A] = ???

    private def filterNotContain1(x: set.Symbol): RegEx[A] = ???

    private def filterNotContain2(t: List[set.Symbol]): RegEx[A] = ???

    def filterIndexOfNeg(t: List[set.Symbol], start: Int): RegEx[A] =
      require(start >= 0)
      if start == 0 then filterNotContain(t)
      else r.absTake(start) * r.absDrop(start).filterNotContain(t)

    def filterIndexOfEq(t: List[set.Symbol], start: Int, n: Int): RegEx[A] =
      require(n >= 0)
      sum(for (r1, r2) <- findFirst(t, start); rf = r1.filterLengthEq(n); if rf.nonEmpty yield rf * word(t) * r2)

    def filterIndexOfLe(t: List[set.Symbol], start: Int, n: Int): RegEx[A] =
      require(n >= 0)
      sum(for (r1, r2) <- findFirst(t, start); rf = r1.filterLengthLe(n); if rf.nonEmpty yield rf * word(t) * r2)
        + filterIndexOfNeg(t, start)

    def filterIndexOfGe(t: List[set.Symbol], start: Int, n: Int): RegEx[A] =
      require(n >= 0)
      sum(for (r1, r2) <- findFirst(t, start); rf = r1.filterLengthGe(n); if rf.nonEmpty yield rf * word(t) * r2)

    def filterIndexOfRightGe(t: List[set.Symbol], start: Int, n: Int): RegEx[A] =
      require(n >= 0)
      sum(for (r1, r2) <- r.absSplitAt(t, start, 0); rf = r2.filterLengthGe(n); if rf.nonEmpty yield r1 * rf)
        + filterIndexOfNeg(t, start)

    def filterIndexOfLt(x1: set.Symbol, x2: set.Symbol, start: Int): RegEx[A] =
      // Assume both character exist
      sum:
        for
          (left, right) <- r.findFirst(List(x1), start)
          leftFiltered = left.filterNotContain1(x2)
          rightFiltered = right.filterContains(List(x2))
          if leftFiltered.nonEmpty && rightFiltered.nonEmpty
        yield leftFiltered * symbol(x1) * rightFiltered

    def absSplitAt(indexOf: List[set.Symbol], start: Int, offset: Int): List[(RegEx[A], RegEx[A])] = ???

    def filterElemWith(i: Int, f: A => A): RegEx[A] = ???

    def filterElemWith(indexOf: List[set.Symbol], start: Int, offset: Int, f: A => A): RegEx[A] = ???

    // Filter slice
    def filterSliceWith(from: Int, f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterSliceWith(fromIndexOf: List[set.Symbol], start: Int, offset: Int,
                        f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterSliceWith(fromLeft: Int, until: Int, f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterSliceWith(fromLeft: Int, untilIndexOf: List[set.Symbol], start: Int, offset: Int,
                        f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterSliceFromRightWith(fromRight: Int, untilRight: Int, f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterSliceWith(fromIndexOf: List[set.Symbol], start: Int, offset: Int, untilRight: Int,
                        f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    def filterForall(f: A => A): RegEx[A] = ???

    def filterExists(f: A => A): RegEx[A] = ???

    @deprecated
    def filterSliceWith2(fromIndexOf: (List[set.Symbol], Int, Int), untilLast: Int,
                         f: RegEx[A] => RegEx[A]): RegEx[A] = ???

    private def constLength: Option[Int] = ???

    def groupByLength(bound: Int): Map[Int, RegEx[A]] = ???

    def filterLengthEq(n: Int): RegEx[A] = ???
    def filterLengthLe(n: Int): RegEx[A] = ???
    def filterLengthGe(n: Int): RegEx[A] = ???

  private class CountSolver[A](using set: SymbolSet[A])(t: List[set.Symbol]) extends LinearSolver[A, Set[Int]]:
    require(t.nonEmpty)

    override protected def build(input: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      // count(r) = {0} (if r may not contain t) ∪ {1 + n | (_, r2) ∈ r.find(t), n ∈ count(r2)}
      // In RegEx[Int, Int]: {0} = REOne, {1 + n | n ∈ count(r2)} = RELit(1) * count(r2)
      (if input.filterNotContain(t).nonEmpty then One() else Zero(),
        for (_, r2) <- input.findFirst(t) yield (Lit(Set(1)), r2))

  private class SplitSolver[A](using set: SymbolSet[A])(t: List[set.Symbol]) extends LinearSolver[A, RegEx[A]]:
    override protected def build(input: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      // split(r) = {[s] | s ∈ rf} (where rf = r ∩ {s | s not contain t})
      //          ∪ {[s1] ++ ss | (r1, r2) ∈ r.find(t), s1 ∈ r1, ss ∈ split(r2)}
      // In RE notation: {[s] | s ∈ rf} = RELit(rf), {[s1] ++ ss | s1 ∈ r1, ss ∈ split(r2)} = RELit(r1) * split(r2)
      val rf = input.filterNotContain(t)
      (if rf.nonEmpty then Lit(rf) else Zero(),
        for (r1, r2) <- input.findFirst(t) yield (Lit(r1), r2))

  private class NotContainSolver[A](using set: SymbolSet[A])(t: List[set.Symbol]) extends LinearSolver[A, A]:
    require(t.nonEmpty)

    override protected def build(r: InputRE): (OutputRE, List[(OutputRE, InputRE)]) =
      val x = t.head
      val a = r.first
      // notContain(r) = REOne (if r is nullable)
      //               + x * notContain(r1) (where r1 = r.deriv(x) ∩ {s | s not start with t1})
      //               + (r.first - x) * notContain(r.deriv([^x]))
      (if r.nullable then One() else Zero(),
        (if a.contains(x) then List((symbol(x), r.deriv(x).filterNotStartWith(t.tail))) else Nil) ++
          (for (a, r1) <- r.absSplitAt1; if (a - x).nonEmpty yield (symbolSet(a - x), r1)))

  given Conversion[String, List[Char]] = _.toList
