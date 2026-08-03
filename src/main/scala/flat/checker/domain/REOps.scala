package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Index.*
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.RegEx.*

import scala.annotation.tailrec

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
      require(0 <= i)
      r.reverse.absDrop(i).first

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

    /** Abstract operation for `s.slice(i, j)`. */
    def absSlice(i: Int, j: Int): RegEx[A] =
      require(0 <= i && i <= j)
      r.absDrop(i).absTake(j - i)

    /** Abstract operation for `s.startsWith(t)`. */
    def absStartsWith(t: List[set.Symbol]): BoolSet =
      if filterStartsWith(t).isEmpty then BoolSet.False
      else if filterNotStartWith(t).isEmpty then BoolSet.True
      else BoolSet.Full

    /** Abstract operation for `s.endsWith(t)`. */
    def absEndsWith(t: List[set.Symbol]): BoolSet = r.reverse.absStartsWith(t.reverse)

    // Find
    /** Abstract operation for `s.contains(x)`. */
    def absContains(t: List[set.Symbol]): BoolSet =
      if r.filterContains(t).isEmpty then BoolSet.False
      else if r.filterNotContain(t).isEmpty then BoolSet.True
      else BoolSet.Full

    private def findFirst(t: List[set.Symbol]): List[(RegEx[A], RegEx[A])] =
      val forbiddenPrefixes =
        for n <- 1.until(t.length).toList; if t.endsWith(t.take(n)) yield t.dropRight(n)
      for
        (r1, r2) <- findAny(t.head); r2f = r2.filterStartsWith(t.tail); if r2f.nonEmpty
        r1f1 = forbiddenPrefixes.foldLeft(r1)(_.filterNotEndWith(_)); if r1f1.nonEmpty
        r1f = r1f1.filterNotContain(t); if !r1f.isEmpty
      yield (r1f, r2f.deriv(t.tail))

    def dropIndexOf(t: List[set.Symbol]): RegEx[A] =
      word(t) * sum(r.findFirst(t).map(_._2))

    def takeIndexOf(t: List[set.Symbol]): RegEx[A] =
      sum(r.findFirst(t).map(_._1))

    /** Abstract operation for `s.indexOf(x)`. */
    def absIndexOf(t: List[set.Symbol]): IndexSet =
      val neg = if r.filterNotContain(t).nonEmpty then Set(-1) else Set.empty
      IndexSet(neg, takeIndexOf(t).absLength)

    def absSlice(start: Index, end: Index): RegEx[A] = (start, end) match
      case (Left(i), Right(j)) => r.absDrop(i).absDropRight(j)
      case (Left(i), Left(j)) => r.absDrop(i).absTake(j - i)
      case (Right(i), Right(j)) => r.absDropRight(i).absTakeRight(j - i)
      case (First(w, k), Right(0)) => r.dropIndexOf(w.asInstanceOf[List[set.Symbol]]).absDrop(k)
      case (Left(0), First(w, 0)) => r.takeIndexOf(w.asInstanceOf[List[set.Symbol]])
      case _ =>
        logger.warn(s"Cannot slice ${r.pp} with indices $start and $end")
        Zero()

    /** Abstract operation for `s.count(t)`. */
    def absCount(t: List[set.Symbol]): CountingRE = t match
      case Nil => throw IllegalArgumentException("absCount: t must be non-empty")
      case _ => CountSolver(t).solve(r)

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

    private def filterNotContain1(x: set.Symbol): RegEx[A] = r match
      case Zero() | One() => r
      case Lit(d) => symbolSet(d - x)
      case Plus(r1, r2) => r1.filterNotContain1(x) + r2.filterNotContain1(x)
      case Comp(r1, r2) => r1.filterNotContain1(x) * r2.filterNotContain1(x)
      case Star(r1) => r1.filterNotContain1(x).star

    private def filterNotContain2(t: List[set.Symbol]): RegEx[A] =
      require(t.length >= 2)
      r match
        case Zero() | One() | Lit(_) => r
        case Plus(r1, r2) => r1.filterNotContain2(t) + r2.filterNotContain2(t)
        case Comp(r1, r2) if !r1.alphabet.contains(t.head) => r1 * r2.filterNotContain2(t)
        case _ =>
          val alphabet = r.alphabet
          if t.forall(alphabet.contains) then NotContainSolver(t).solve(r) else r

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
      val b = a - x
      // notContain(r) = REOne (if r is nullable)
      //               + x * notContain(r1) (where r1 = r.deriv(x) ∩ {s | s not start with t1})
      //               + (r.first - x) * notContain(r.deriv([^x]))
      (if r.nullable then One() else Zero(),
        (if a.contains(x) then List((symbol(x), r.deriv(x).filterNotStartWith(t.tail))) else Nil) ++
          (if b.nonEmpty then List((Lit(b), r.deriv(a => (a - x).nonEmpty))) else Nil))

  given Conversion[String, List[Char]] = _.toList
