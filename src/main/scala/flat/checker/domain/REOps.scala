package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Index.*
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.RegEx.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object REOps extends LazyLogging:
  extension [A](r: RegEx[A])(using set: SymbolSet[A])
    /** Abstract operation for `s.length`. */
    def absLength: CountingRE = r match
      case Zero() => Zero()
      case One() => One()
      case Lit(d) => if d.isEmpty then Zero() else Lit(Set(1))
      case Plus(r1, r2) => r1.absLength + r2.absLength
      case Comp(r1, r2) => r1.absLength * r2.absLength
      case Star(r1) => r1.absLength.star

    /** Abstract operation for `s.drop(n)`. */
    @tailrec
    def absDrop(n: Int): RegEx[A] =
      require(0 <= n)
      if n == 0 then r
      else
        val r1 = r.deriv(_.nonEmpty)
        if r1.isEmpty then One() else r1.absDrop(n - 1)

    def absDropRight(n: Int): RegEx[A] = r.reverse.absDrop(n).reverse

    /** Abstract operation for `s(i)`. */
    def absAt(i: Int): A =
      require(0 <= i)
      r.absDrop(i).first

    def absAtRight(i: Int): A =
      require(0 < i)
      r.reverse.absDrop(i - 1).first

    def absAt(index: Index): A = index match
      case Left(i) => r.absAt(i)
      case Right(i) =>
        require(0 < i)
        r.absAtRight(i - 1)
      case First(w, i, k) =>
        if 0 <= k && k < w.length then set.singleton(w(k).asInstanceOf[set.Symbol])
        else r.dropIndexOf(w.asInstanceOf[List[set.Symbol]], i).absAt(k)
      case Index.Range(from, until) =>
        r.absSlice(from, until).alphabet

    /** Abstract operation for `s.take(n)`. */
    def absTake(n: Int): RegEx[A] =
      require(n >= 0)
      if n == 0 then One()
      else
        val result1: RegEx[A] = if r.nullable then One() else Zero() // s is empty
        val result2 = sum(for (d, r1) <- r.absSplitAt1.toList yield Lit(d) * r1.absTake(n - 1)) // s is nonempty
        result1 + result2

    def absTakeRight(n: Int): RegEx[A] = r.reverse.absTake(n).reverse

    /** Abstract operation for `s.splitAt(1)`. */
    def absSplitAt1: List[(A, RegEx[A])] = r match
      case Zero() | One() => Nil
      case Lit(d) => if d.isEmpty then Nil else List(d -> One())
      case Plus(r1, r2) => r1.absSplitAt1 ++ r2.absSplitAt1
      case Comp(r1, r2) =>
        (for (a, r1r) <- r1.absSplitAt1 yield (a, r1r * r2)) ++ (if r1.nullable then r2.absSplitAt1 else Nil)
      case Star(r1) =>
        for (a, r1r) <- r1.absSplitAt1 yield (a, r1r * r)

    def absSplitAt(n: Int): List[(RegEx[A], RegEx[A])] =
      require(n >= 0)
      if n == 0 then List((One(), r))
      else
        for (a, r1) <- r.absSplitAt1; (r2, r3) <- r1.absSplitAt(n - 1) yield (Lit(a) * r2, r3)

    def absSplitAtLast(n: Int): List[(RegEx[A], RegEx[A])] =
      require(n < 0)
      for (r1, r2) <- r.reverse.absSplitAt(-n) yield (r2.reverse, r1.reverse)

    /** Abstract operation for `s.slice(i, j)`. */
//    def absSlice(i: Int, j: Int): RegEx[A] =
//      require(0 <= i && i <= j)
//      r.absDrop(i).absTake(j - i)

    /** Abstract operation for `s.startsWith(t)`. */
    def absStartsWith(t: List[set.Symbol]): BoolSet =
      if filterStartsWith(t).isEmpty then BoolSet.False
      else if filterNotStartWith(t).isEmpty then BoolSet.True
      else BoolSet.Full

    /** Abstract operation for `s.endsWith(t)`. */
    def absEndsWith(t: List[set.Symbol]): BoolSet = r.reverse.absStartsWith(t.reverse)

    // Find
    /** Abstract operation for `s.contains(x)`. */
    def absContainsElem(x: set.Symbol): BoolSet =
      if r.findAny(x).isEmpty then BoolSet.False
      else if r.filterNotContain(List(x)).isEmpty then BoolSet.True
      else BoolSet.Full

    def absContains(t: List[set.Symbol]): BoolSet =
      if r.filterContains(t).isEmpty then BoolSet.False
      else if r.filterNotContain(t).isEmpty then BoolSet.True
      else BoolSet.Full

    def findFirst(t: List[set.Symbol]): List[(RegEx[A], RegEx[A])] =
      val forbiddenPrefixes =
        for n <- 1.until(t.length).toList; if t.endsWith(t.take(n)) yield t.dropRight(n)
      for
        (r1, r2) <- findAny(t.head); r2f = r2.filterStartsWith(t.tail); if r2f.nonEmpty
        r1f1 = forbiddenPrefixes.foldLeft(r1)(_.filterNotEndWith(_)); if r1f1.nonEmpty
        r1f = r1f1.filterNotContain(t); if !r1f.isEmpty
      yield (r1f, r2f.deriv(t.tail))

    def findFirst(t: List[set.Symbol], start: Int): List[(RegEx[A], RegEx[A])] =
      require(start >= 0)
      if start == 0 then r.findFirst(t)
      else
        for
          (r1, r2) <- r.absSplitAt(start)
          (r2, r3) <- r2.findFirst(t)
        yield (r1 * r2, r3)

    def dropIndexOf(t: List[set.Symbol], start: Int = 0): RegEx[A] =
      word(t) * sum(r.findFirst(t, start).map(_._2))

    def takeIndexOf(t: List[set.Symbol], start: Int = 0): RegEx[A] =
      sum(r.findFirst(t, start).map(_._1))

    /** Abstract operation for `s.indexOf(x)`. */
    def absIndexOf(t: List[set.Symbol], start: Int = 0): IndexSet =
      val neg = if r.absDrop(start).filterNotContain(t).nonEmpty then Set(-1) else Set.empty
      val leftIndexSet = takeIndexOf(t, start).absLength
      val rightIndexSet = dropIndexOf(t, start).absLength
      IndexSet(neg, leftIndexSet, rightIndexSet)

    def absIndexOfElem(x: set.Symbol, start: Int): IndexSet = absIndexOf(List(x), start)

    def absSlice(from: Int): RegEx[A] =
      if from >= 0 then r.absDrop(from) else r.absDropRight(-from)

    def absSlice(fromIndexOf: List[set.Symbol], start: Int, offset: Int): RegEx[A] =
      require(start >= 0)
      sum(absSplitAt(fromIndexOf, start, offset).map(_._2))

    def absSlice(fromLeft: Int, until: Int): RegEx[A] =
      require(fromLeft >= 0)
      if until >= 0 then r.absTake(until).absDrop(fromLeft)
      else r.absDrop(fromLeft).absDropRight(-until)

    def absSlice(fromLeft: Int, untilIndexOf: List[set.Symbol], start: Int, offset: Int): RegEx[A] =
      require(fromLeft >= 0 && start >= 0)
      sum(for (left, _) <- r.absSplitAt(untilIndexOf, start, offset) yield left.absDrop(fromLeft))

    def absSlice(fromIndexOf: List[set.Symbol], start: Int, offset: Int, untilLast: Int): RegEx[A] =
      require(start >= 0 && untilLast < 0)
      sum(for (_, right) <- r.absSplitAt(fromIndexOf, start, offset) yield right.absDropRight(-untilLast))

    def absSlice(start: Index, end: Index): RegEx[A] = (start, end) match
      case (Left(i), Right(j)) => r.absDrop(i).absDropRight(j)
      case (Left(i), Left(j)) => r.absDrop(i).absTake(j - i)
      case (Right(i), Right(j)) => r.absDropRight(i).absTakeRight(j - i)
      case (First(w, i, k), Right(0)) => r.dropIndexOf(w.asInstanceOf[List[set.Symbol]], i).absDrop(k)
      case (Left(0), First(w, i, 0)) => r.takeIndexOf(w.asInstanceOf[List[set.Symbol]], i)
      case _ =>
        logger.warn(s"Cannot slice ${r.pp} with indices $start and $end")
        Zero()

    /** Abstract operation for `s.count(t)`. */
    def absCount(t: List[set.Symbol]): CountingRE = t match
      case Nil => throw IllegalArgumentException("absCount: t must be non-empty")
      case _ => CountSolver(t).solve(r)

    def absSplit(t: List[set.Symbol], limit: Int): RegEx[RegEx[A]] =
      require(limit >= 0)
      if t.isEmpty || limit == 0 then Lit(r)
      else
        val rs = for (r1, r2) <- r.findFirst(t) yield Lit(r1) * r2.absSplit(t, limit - 1)
        if rs.isEmpty then Lit(r) else sum(rs)

    /** Abstract operation for `s.split(t)`. */
    def absSplit(t: List[set.Symbol]): RegEx[RegEx[A]] = t match
      case Nil => Lit(r)
      case _ => SplitSolver(t).solve(r)

    /** Abstract operation for `s.map(f)`. */
    def absMap(f: A => A): RegEx[A] = r match
      case Zero() | One() => r
      case Lit(d) => Lit(f(d))
      case Plus(r1, r2) => r1.absMap(f) + r2.absMap(f)
      case Comp(r1, r2) => r1.absMap(f) * r2.absMap(f)
      case Star(r1) => r1.absMap(f).star

    // Filters
    /** Computes {s ∈ r | s starts with t}. */
    def filterStartsWith(t: List[set.Symbol]): RegEx[A] = t match
      case Nil => r
      case _ =>
        val r1 = r.deriv(t)
        if r1.isEmpty then Zero() else word(t) * r1

    /** Computes {s ∈ r | s not start with t}. */
    def filterNotStartWith(t: List[set.Symbol]): RegEx[A] = t match
      case Nil => Zero()
      case x :: xs =>
        // case 1: empty
        val result1: RegEx[A] = if r.nullable then One() else Zero()
        // case 2: not start with x
        val result2 = sum(for (a, r1) <- r.absSplitAt1 yield symbolSet(a - x) * r1)
        // case 3: start with x but the rest not start with xs
        val r1 = r.deriv(x)
        val result3 = if r1.isEmpty then Zero() else symbol(x) * r1.filterNotStartWith(xs)
        result1 + result2 + result3

    /** Computes {s ∈ r | s ends with t}. */
    def filterEndsWith(t: List[set.Symbol]): RegEx[A] = r.reverse.filterStartsWith(t.reverse).reverse

    /** Computes {s ∈ r | s not end with t}. */
    def filterNotEndWith(t: List[set.Symbol]): RegEx[A] = r.reverse.filterNotStartWith(t.reverse).reverse

    /** Computes {s ∈ r | s = t}. */
    def filterEq(t: List[set.Symbol]): RegEx[A] = if r.contains(t) then word(t) else Zero()

    /** Computes {s ∈ r | s ≠ t}. */
    def filterNe(t: List[set.Symbol]): RegEx[A] = word(t) * r.deriv(t).filterNonEmpty + r.filterNotStartWith(t)

    /** Computes {s ∈ r | s ≠ []}. */
    def filterNonEmpty: RegEx[A] = r match
      case Zero() | One() => Zero()
      case Lit(_) => r
      case Plus(r1, r2) => r1.filterNonEmpty + r2.filterNonEmpty
      case Comp(r1, r2) => r1.filterNonEmpty * r2 + (if r1.nullable then r2.filterNonEmpty else Zero())
      case Star(r1) => r1.filterNonEmpty * r

    private def findAny(x: set.Symbol): List[(RegEx[A], RegEx[A])] = r match
      case Zero() | One() => Nil
      case Lit(d) => if d.contains(x) then List((One(), One())) else Nil
      case Plus(r1, r2) => r1.findAny(x) ++ r2.findAny(x)
      case Comp(r1, r2) =>
        (for (r1l, r1r) <- r1.findAny(x) yield (r1l, r1r * r2)) ++
          (for (r2l, r2r) <- r2.findAny(x) yield (r1 * r2l, r2r))
      case Star(r1) =>
        for (r1l, r1r) <- r1.findAny(x) yield (r * r1l, r1r * r)

    /** Computes { s ∈ r | s contains t }. */
    def filterContains(t: List[set.Symbol]): RegEx[A] = t match
      case Nil => r
      case x :: xs =>
        sum(for (r1, r2) <- r.findAny(x) yield r1 * symbol(x) * r2.filterStartsWith(xs))

    /** Computes { s ∈ r | s not contain t }. */
    def filterNotContain(t: List[set.Symbol]): RegEx[A] = t match
      case Nil => Zero()
      case List(x) => r.filterNotContain1(x)
      case _ => r.filterNotContain2(t)

    def filterNotContainElem(x: set.Symbol): RegEx[A] = r.filterNotContain1(x)

    private def filterNotContain1(x: set.Symbol): RegEx[A] =
      val a = r.alphabet
      if !a.contains(x) then r
      else r match
        case Zero() | One() => r
        case Lit(d) => symbolSet(d - x)
        case Plus(r1, r2) => r1.filterNotContain1(x) + r2.filterNotContain1(x)
        case Comp(r1, r2) => r1.filterNotContain1(x) * r2.filterNotContain1(x)
        case Star(r1) => r1.filterNotContain1(x).star

    private def filterNotContain2(t: List[set.Symbol]): RegEx[A] =
      require(t.length >= 2)
      val a = r.alphabet
      if t.exists(!a.contains(_)) then r
      else if r.findAny(t.head).forall(_._2.deriv(t.tail).isEmpty) then r
      else r match
        case Plus(r1, r2) => r1.filterNotContain2(t) + r2.filterNotContain2(t)
        case Comp(r1, r2) if !r1.alphabet.contains(t.head) => r1 * r2.filterNotContain2(t)
        case _ => NotContainSolver(t).solve(r)

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

    def filterIndexOfLt(x1: set.Symbol, x2: set.Symbol, start: Int): RegEx[A] =
      // Assume both character exist
      sum:
        for
          (left, right) <- r.findFirst(List(x1), start)
          leftFiltered = left.filterNotContain1(x2)
          rightFiltered = right.filterContains(List(x2))
          if leftFiltered.nonEmpty && rightFiltered.nonEmpty
        yield leftFiltered * symbol(x1) * rightFiltered

    def absSplitAt(indexOf: List[set.Symbol], start: Int, offset: Int): List[(RegEx[A], RegEx[A])] =
      if offset == 0 then
        val pat = word(indexOf)
        for (before, after) <- r.findFirst(indexOf, start) yield (before, pat * after)
      else if offset == indexOf.length then
        val pat = word(indexOf)
        for (before, after) <- r.findFirst(indexOf, start) yield (before * pat, after)
      else if 0 < offset && offset < indexOf.length then
        val pat1 = word(indexOf.take(offset))
        val pat2 = word(indexOf.drop(offset))
        for (before, after) <- r.findFirst(indexOf, start) yield (before * pat1, pat2 * after)
      else if offset > indexOf.length then
        val pat = word(indexOf)
        for
          (before, after) <- r.findFirst(indexOf, start)
          (r1, r2) <- after.absSplitAt(offset - indexOf.length)
        yield (before * pat * r1, r2)
      else // offset < 0
        val pat = word(indexOf)
        for
          (before, after) <- r.findFirst(indexOf, start)
          (r1, r2) <- before.absSplitAtLast(offset)
        yield (r1, r2 * pat * after)

    def filterElemWith(i: Int, f: A => A): RegEx[A] =
      sum:
        for
          (left, right) <- if i >= 0 then r.absSplitAt(i) else r.absSplitAtLast(i)
          (target, skipped) <- right.absSplitAt1
          filtered = f(target)
          if filtered.nonEmpty
        yield left * Lit(filtered) * skipped

    // Filter slice
    def filterSliceWith(from: Int, f: RegEx[A] => RegEx[A]): RegEx[A] =
      sum:
        for
          (left, right) <- if from >= 0 then r.absSplitAt(from) else r.absSplitAtLast(from)
          filtered = f(right)
          if filtered.nonEmpty
        yield left * filtered

    def filterSliceWith(fromIndexOf: List[set.Symbol], start: Int, offset: Int, f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(start >= 0)
      sum:
        for
          (left, right) <- r.absSplitAt(fromIndexOf, start, offset)
          filtered = f(right)
          if filtered.nonEmpty
        yield left * filtered

    def filterSliceWith(fromLeft: Int, until: Int, f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(fromLeft >= 0)
      sum:
        for
          (left, right) <- if until >= 0 then r.absSplitAt(until) else r.absSplitAtLast(until)
          (skipped, target) <- left.absSplitAt(fromLeft)
          filtered = f(target)
          if filtered.nonEmpty
        yield skipped * filtered * right

    def filterSliceWith(fromLeft: Int, untilIndexOf: List[set.Symbol], start: Int, offset: Int,
                        f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(fromLeft >= 0 && start >= 0)
      sum:
        for
          (left, right) <- r.absSplitAt(untilIndexOf, start, offset)
          (skipped, target) <- left.absSplitAt(fromLeft)
          filtered = f(target)
          if filtered.nonEmpty
        yield skipped * filtered * right

    def filterSliceFromRightWith(fromRight: Int, untilRight: Int, f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(fromRight >= 0 && untilRight < 0)
      sum:
        for
          (left, right) <- r.absSplitAtLast(fromRight)
          (target, skipped) <- right.absSplitAtLast(untilRight)
          filtered = f(target)
          if filtered.nonEmpty
        yield left * filtered * skipped

    def filterSliceWith(fromIndexOf: List[set.Symbol], start: Int, offset: Int, untilRight: Int,
                        f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(start >= 0 && untilRight < 0, s"start = $start, untilRight = $untilRight")
      sum:
        for
          (left, right) <- r.absSplitAt(fromIndexOf, start, offset)
          (target, skipped) <- right.absSplitAtLast(untilRight)
          filtered = f(target)
          if filtered.nonEmpty
        yield left * filtered * skipped

    def filterForall(f: A => A): RegEx[A] = r match
      case Zero() | One() => r
      case Lit(d) => symbolSet(f(d))
      case Plus(r1, r2) => r1.filterForall(f) + r2.filterForall(f)
      case Comp(r1, r2) => r1.filterForall(f) * r2.filterForall(f)
      case Star(r1) => r1.filterForall(f).star

    def filterExists(f: A => A): RegEx[A] = r match
      case Zero() | One() => Zero()
      case Lit(d) => symbolSet(f(d))
      case Plus(r1, r2) => r1.filterExists(f) + r2.filterExists(f)
      case Comp(r1, r2) => r1.filterExists(f) * r2 + r1 * r2.filterExists(f)
      case Star(r1) => r * r1.filterExists(f) * r

    @deprecated
    def filterSliceWith2(fromIndexOf: (List[set.Symbol], Int, Int), untilLast: Int,
                         f: RegEx[A] => RegEx[A]): RegEx[A] =
      require(untilLast <= 0)
      sum:
        for
          (r1, r2) <- r.findFirst(fromIndexOf._1, fromIndexOf._2)
          (r2, r3) <- (word(fromIndexOf._1) * r2).absDrop(fromIndexOf._3).absSplitAtLast(untilLast)
          rf = f(r2)
          if rf.nonEmpty
        yield r1 * rf * r3

    private def constLength: Option[Int] =
      val r1 = r.absLength
      if r1.isFinite then
        val ns = r1.toFinSet
        if ns.size == 1 then Some(ns.head) else None
      else None

    def groupByLength(bound: Int): Map[Int, RegEx[A]] =
      r.constLength match
        case Some(k) => Map(k -> r)
        case None => r match
          case Zero() => Map.empty
          case One() => Map(0 -> r)
          case Lit(d) => if d.isEmpty then Map.empty else Map(1 -> r)
          case Plus(r1, r2) =>
            val m = mutable.Map.from(r1.groupByLength(bound))
            for (k, rk) <- r2.groupByLength(bound) do
              m(k) = m.getOrElse(k, Zero()) + rk
            m.toMap
          case Comp(r1, r2) =>
            val m1 = r1.groupByLength(bound)
            val m2 = r2.groupByLength(bound)
            val m = mutable.Map.empty[Int, RegEx[A]]
            // first part within bound
            for (k1, r1) <- m1; if k1 <= bound do
              for (k2, r2) <- m2 do
                val k = (k1 + k2) min (bound + 1)
                m(k) = m.getOrElse(k, Zero()) + (r1 * r2)
            // first part out of bound
            for r1 <- m1.get(bound + 1) do
              m(bound + 1) = m.getOrElse(bound + 1, Zero()) + (r1 * r2)
            m.toMap
          case Star(r1) =>
            val m1 = r1.groupByLength(bound)
            val ms = ListBuffer.empty[mutable.Map[Int, RegEx[A]]]
            for i <- 0 to bound do
              val m = mutable.Map.empty[Int, RegEx[A]]
              m(0) = One()
              // first part within bound
              for (k1, r1) <- m1; if k1 <= i do
                for (k2, r2) <- ms(i - k1) do
                  val k = (k1 + k2) min (i + 1)
                  m(k) = m.getOrElse(k, Zero()) + (r1 * r2)
              // first part out of bound
              val rs = (for (k1, r1) <- m1; if k1 > i yield r1).toList
              if rs.nonEmpty then
                m(i + 1) = m.getOrElse(i + 1, Zero()) + (sum(rs) * r)
              ms += m
            ms(bound).toMap

    def filterLengthEq(n: Int): RegEx[A] =
      require(n >= 0)
      val m = r.groupByLength(n)
      m.get(n) match
        case Some(rn) => rn
        case None => Zero()

    def filterLengthLe(n: Int): RegEx[A] =
      require(n >= 0)
      val m = r.groupByLength(n)
      sum((for (k, rk) <- m; if k <= n yield rk).toList)

    def filterLengthGe(n: Int): RegEx[A] =
      require(n >= 0)
      if n == 0 then r
      else
        val m = r.groupByLength(n)
        sum((for (k, rk) <- m; if k >= n yield rk).toList)

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
