package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.regex.RegEx.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object REOps:
  extension (r: RegEx)
    def absLength: NatSet = r match
      case RENone => Set.empty
      case RENull => Set(0)
      case RELit(_) => Set(1)
      case REUnion(r1, r2) => r1.absLength | r2.absLength
      case REConcat(r1, r2) => r1.absLength + r2.absLength
      case REStar(_) => NatSet.From(0)

    /* Prefix Operations */
    /** Abstract operation for `s.startsWith(t)`. */
    def absStartWith(t: String): BoolSet =
      if !existsStartWith(t) then BoolSet.False
      else if forallStartWith(t) then BoolSet.True
      else BoolSet.Top

    /** Tests if there exists a string s ∈ r such that s starts with t. */
    private def existsStartWith(t: String): Boolean = !r.deriv(t).isEmpty

    /** Tests if forall string s ∈ r, s starts with c. */
    private def forallStartWith(c: Char): Boolean = !r.nullable && r.first.isSingleton && r.first.contains(c)

    /** Tests if forall string s ∈ r, s starts with t. */
    @tailrec
    private def forallStartWith(t: String): Boolean =
      if t.isEmpty then true
      else forallStartWith(t.head) && r.deriv(t.head).forallStartWith(t.tail)

    /** Computes {s ∈ r | s starts with t}. */
    def filterStartWith(t: String): RegEx = fromString(t) ++ r.deriv(t)

    /** Computes {s ∈ r | s not start with c}. */
    def filterNotStartWith(c: Char): RegEx =
      val splits = r.splitAt0
      (if r.nullable then RENull else RENone) | union(for (a, r) <- splits yield fromCharSet(a - c) ++ r)

    private def splitAt0: List[(CharSet, RegEx)] = r match
      case RENone | RENull => Nil
      case RELit(cs) => List((cs, RENull))
      case REUnion(r1, r2) => r1.splitAt0 ++ r2.splitAt0
      case REConcat(r1, r2) =>
        (for (a, r1r) <- r1.splitAt0 yield (a, r1r ++ r2)) ++ (if r1.nullable then r2.splitAt0 else Nil)
      case REStar(r1) =>
        for (a, r1r) <- r1.splitAt0 yield (a, r1r ++ r)

    /** Computes {s ∈ r | s not start with t}. */
    def filterNotStartWith(t: String): RegEx =
      if t.isEmpty then RENone
      else filterNotStartWith(t.head) | (fromChar(t.head) ++ r.deriv(t.head).filterNotStartWith(t.tail))

    /** Abstract operation for `s.endsWith(t)`. */
    def absEndWith(t: String): BoolSet = r.reverse.absStartWith(t.reverse)

    /** Computes {s ∈ r | s ends with t}. */
    def filterEndWith(t: String): RegEx = r.reverse.filterStartWith(t.reverse).reverse

    /** Computes {s ∈ r | s not end with t}. */
    def filterNotEndWith(t: String): RegEx = r.reverse.filterNotStartWith(t.reverse).reverse

    /** Computes {s ∈ r | s = t}. */
    def filterEq(t: String): RegEx = if r.contains(t) then fromString(t) else RENone

    /** Computes {s ∈ r | s ≠ []}. */
    def filterNonEmpty: RegEx = r match
      case RENone | RENull => RENone
      case RELit(_) => r
      case REUnion(r1, r2) => r1.filterNonEmpty | r2.filterNonEmpty
      case REConcat(r1, r2) => (r1.filterNonEmpty ++ r2) | (if r1.nullable then r2.filterNonEmpty else RENone)
      case REStar(r1) => r1.filterNonEmpty ++ r

    /** Computes {s ∈ r | s ≠ t}. */
    def filterNe(t: String): RegEx =
      filterNotStartWith(t) | (fromString(t) ++ r.deriv(t).filterNonEmpty)

    /* Infix Operations */
    /** Abstract operation for `s.contains(c)`. */
    def absContain(c: Char): BoolSet =
      if !existsContain(c) then BoolSet.False
      else if forallContain(c) then BoolSet.True
      else BoolSet.Top

    /** Tests if there exists a string s ∈ r such that c ∈ s. */
    def existsContain(c: Char): Boolean = r match
      case RENone | RENull => false
      case RELit(a) => a.contains(c)
      case REUnion(r1, r2) => r1.existsContain(c) || r2.existsContain(c)
      case REConcat(r1, r2) => r1.existsContain(c) || r2.existsContain(c)
      case REStar(r1) => r1.existsContain(c)

    /** Tests if there exists a string s ∈ r such that s contains t. */
    def existsContain(t: String): Boolean = t.length match
      case 0 => true
      case 1 => existsContain(t.head)
      case _ =>
        val splits = splitBy(t.head)
        splits.exists(_._2.existsStartWith(t.tail))

    def splitBy(c: Char): List[(RegEx, RegEx)] = r match
      case RENone | RENull => Nil
      case REUnion(r1, r2) => r1.splitBy(c) ++ r2.splitBy(c)
      case RELit(a) => if a.contains(c) then List((RENull, RENull)) else Nil
      case REConcat(r1, r2) =>
        (for (r1l, r1r) <- r1.splitBy(c) yield (r1l, r1r ++ r2)) ++
          (for (r2l, r2r) <- r2.splitBy(c) yield (r1 ++ r2l, r2r))
      case REStar(r1) =>
        for (r1l, r1r) <- r1.splitBy(c) yield (r ++ r1l, r1r ++ r)

    /** Tests if forall string s ∈ r, we have c ∈ s. */
    def forallContain(c: Char): Boolean = r match
      case RENone | RENull | REStar(_) => false
      case RELit(a) => a.isSingleton && a.contains(c)
      case REUnion(r1, r2) => r1.forallContain(c) && r2.forallContain(c)
      case REConcat(r1, r2) => r1.forallContain(c) || r2.forallContain(c)

    /** Tests if forall string s ∈ r, we have s contains t. */
    def forallContain(t: String): Boolean = t.length match
      case 0 => true
      case 1 => forallContain(t.head)
      case _ => filterNotContainSlice(t).isEmpty

    /** Computes {s ∈ r | c ∈ s}. */
    def filterContain(c: Char): RegEx = r match
      case RENone | RENull => RENone
      case RELit(s) => if s.contains(c) then fromChar(c) else RENone
      case REConcat(r1, r2) => (r1.filterContain(c) ++ r2) | (r1 ++ r2.filterContain(c))
      case REUnion(r1, r2) => r1.filterContain(c) | r2.filterContain(c)
      case REStar(r) => r ++ r.filterContain(c) ++ r

    /** Computes {s ∈ r | s contains t}. */
    def filterContain(t: String): RegEx = t.length match
      case 0 => r
      case 1 => filterContain(t.head)
      case _ =>
        val splits = r.splitBy(t.head)
        union(for (r1, r2) <- splits yield r1 ++ fromChar(t.head) ++ r2.filterStartWith(t.tail))

    /** Computes {s ∈ r | c ∉ s} */
    def filterNotContain(c: Char): RegEx = r match
      case RENone | RENull => r
      case RELit(a) => fromCharSet(a - c)
      case REUnion(r1, r2) => r1.filterNotContain(c) | r2.filterNotContain(c)
      case REConcat(r1, r2) => r1.filterNotContain(c) ++ r2.filterNotContain(c)
      case REStar(r) => r.filterNotContain(c).*

    /** Computes {s ∈ r | s not contain t} */
    def filterNotContain(t: String): RegEx = t.length match
      case 0 => RENone
      case 1 => filterNotContain(t.head)
      case _ => filterNotContainSlice(t)

    private def filterNotContainSlice(t: String): RegEx =
      if !CharSet.from(t).subsetOf(r.alphabet) then r
      else r match
        case REUnion(r1, r2) => r1.filterNotContainSlice(t) | r2.filterNotContainSlice(t)
        case REConcat(r1, r2) if !r1.existsContain(t.head) => r1 ++ r2.filterNotContainSlice(t)
        case _ => FilterNotContainSolver(r, t).solve

    /** Abstract operation for `s.indexOf(c)`. */
    def absIndexOf(c: Char): IndexSet =
      val lengths = splitAtIndexOf(c).map(_._1.absLength)
      IndexSet(!forallContain(c), NatSet.union(lengths))

    private def splitAtIndexOf(c: Char): List[(RegEx, RegEx)] =
      for (r1, r2) <- splitBy(c); r1f = r1.filterNotContain(c); if !r1f.isEmpty
        yield (r1f, fromChar(c) ++ r2)

    /** Abstract operation for `s.indexOf(t)`. */
    def absIndexOf(t: String): IndexSet = t.length match
      case 0 => IndexSet(false, Set(0))
      case 1 => absIndexOf(t.head)
      case _ =>
        val lengths = splitAtIndexOf(t).map(_._1.absLength)
        IndexSet(!forallContain(t), NatSet.union(lengths))

    private def splitAtIndexOf(t: String): List[(RegEx, RegEx)] =
      val forbiddenPrefixes = for n <- (1 until t.length).toList; if t.endsWith(t.take(n)) yield t.dropRight(n)
      for
        (r1, r2) <- splitBy(t.head); r2f = r2.filterStartWith(t.tail); if !r2f.isEmpty
        r1f = r1.filterNotContain(t).filterNotEndWith(forbiddenPrefixes); if !r1f.isEmpty
      yield (r1f, fromChar(t.head) ++ r2f)

    private def filterNotEndWith(ts: List[String]): RegEx = ts.foldLeft(r)(_.filterNotStartWith(_))

    /** Abstract operation for `s.count(c)`. */
    def absCount(c: Char): NatSet = r match
      case RENone | RENull => Set(0)
      case RELit(a) => if a.contains(c) then Set(1) else Set(0)
      case REUnion(r1, r2) => r1.absCount(c) | r2.absCount(c)
      case REConcat(r1, r2) => r1.absCount(c) + r2.absCount(c)
      case REStar(r1) => if r1.existsContain(c) then NatSet.From(0) else Set(0)

    /* Slice Operations */
    /** Abstract operation for `s.drop(1)`. */
    private def absDrop1: RegEx =
      val r1 = r.derivativeAny
      if r1.isEmpty then RENull else r1

    /** Abstract operation for `s.drop(n)`. */
    @tailrec
    def absDrop(n: Int): RegEx =
      require(n >= 0)
      if n == 0 then r else r.absDrop1.absDrop(n - 1)

    /** Abstract operation for `s.take(n)`. */
    def absTake(n: Int): RegEx =
      require(n >= 0)
      if n == 0 then RENull
      else
        val rs = for (a, r1) <- r.splitAt0 yield RELit(a) ++ r1.absTake(n - 1)
        (if r.nullable then RENull else RENone) | union(rs)

    /** Abstract operation for `s.charAt(i)`. */
    def absCharAt(i: Int): CharSet =
      require(i >= 0)
      r.absDrop(i).first

    /** Abstract operation for `s.slice(i, j)`. */
    def absSlice(i: Int, j: Int): RegEx =
      require(0 <= i && i <= j)
      r.absDrop(i).absTake(j - i)

    /** Abstract operation for `s.dropRight(n)`. */
    def absDropRight(n: Int): RegEx =
      require(n >= 0)
      r.reverse.absDrop(n).reverse

    /** Abstract operation for `s.takeRight(n)`. */
    def absTakeRight(n: Int): RegEx =
      require(n >= 0)
      r.reverse.absTake(n).reverse

    /** Abstract operation for `s.drop(s.indexOf(t))`. */
    def absDropIndexOf(t: String): RegEx =
      union(r.splitAtIndexOf(t).map(_._2))

    /** Abstract operation for `s.take(s.indexOf(t))`. */
    def absTakeIndexOf(t: String): RegEx =
      union(r.splitAtIndexOf(t).map(_._1))

    /* Conversion */
    /** Abstract operation for `s.map(f)`. */
    def absMap(f: CharSet => CharSet): RegEx = r match
      case RENone | RENull => r
      case RELit(a) => RELit(f(a))
      case REUnion(r1, r2) => r1.absMap(f) | r2.absMap(f)
      case REConcat(r1, r2) => r1.absMap(f) ++ r2.absMap(f)
      case REStar(r1) => r1.absMap(f).*

    /** Abstract operation for `s.toLower`. */
    def absToLower: RegEx = r.absMap(CharSetAbs.toLower)

    /** Abstract operation for `s.toUpper`. */
    def absToUpper: RegEx = r.absMap(CharSetAbs.toUpper)


  private class FilterNotContainSolver(r: RegEx, t: String) extends LazyLogging:
    require(t.nonEmpty)

    // Variable X_i denotes the language {s ∈ tasks(i) | s not contain t}
    private val tasks = ListBuffer(r)

    private def getOrCreate(regEx: RegEx): Int =
      val i = tasks.indexOf(regEx)
      if i >= 0 then i
      else
        tasks += regEx
        tasks.length - 1

    private class Eq(var base: RegEx, val coef: mutable.Map[Int, RegEx])

    // Linear equation system: X_i = B_i + r_0 * X_0 + ... + r_i * X_i + ...
    private val eqs = ListBuffer.empty[Eq]

    private def showEqs: List[String] =
      for (eq, i) <- eqs.toList.zipWithIndex yield
        if eq.coef.isEmpty then
          s"X_$i = ${eq.base.show}"
        else
          val coefStr = (for (j, rj) <- eq.coef yield s"${rj.show} * X_$j").mkString(" + ")
          s"X_$i = ${eq.base.show} + $coefStr"

    // Substitute eqs(i) for X_i in equations X_0, ..., X_{i - 1}
    private def subst(i: Int): Unit =
      for k <- 0 until i; if eqs(k).coef.contains(i) do
        val coef = eqs(k).coef
        // X_k = ... + r_j * X_j + r_i * X_i + ...
        //     = ... + r_j * X_j + r_i * (... + r_{ij} * X_j + B) + ...
        //     = ... + (r_j + r_i * r_{ij}) * X_j + ... + r_i * B + ...
        for (j, rij) <- eqs(i).coef do
          coef(j) = coef.getOrElse(j, RENone) | (coef(i) ++ rij)
        eqs(k).base |= coef(i) ++ eqs(i).base
        coef.remove(i)

    def solve: RegEx =
      // Step 1: collect equations
      var i = 0
      while i < tasks.length do
        val r = tasks(i)
        // the language of `r` can be split into three subsets:
        // (base) [] if `r` is nullable
        // (coef0) c :: s where `c != t.head`, `s` not contain `t`
        // (coef1) c :: s where `c == t.head`, `s` not start with `t.tail`, `s` not contain `t`
        val base = if r.nullable then RENull else RENone
        val coef = mutable.Map.empty[Int, RegEx]
        val unmatchedChars = r.first - t.head
        val r1 = r.deriv(unmatchedChars)
        if !r1.isEmpty then
          coef(getOrCreate(r1)) = fromCharSet(unmatchedChars)
        val r2 = r.deriv(t.head).filterNotStartWith(t.tail)
        if !r2.isEmpty then
          coef(getOrCreate(r2)) = fromChar(t.head)
        eqs += Eq(base, coef)
        i += 1
//      logger.trace("Tasks:\n{}", tasks.map(_.show).mkString("\n"))
//      logger.debug("Equations:\n{}", showEqs.mkString("\n"))

      // Step 2: solve equations via Arden's lemma: X = A * X + B => X = A.star * B
      i = eqs.length - 1
      while i >= 0 do
        val coef = eqs(i).coef
        // X_i = r_i * X_i + r_j * X_j + ... + B
        // => X_i = r_i^* * (r_j * X_j + ... + B)
        //        = r_i^* * r_j * X_j + ... + r_i^* * B
        val star = coef.getOrElse(i, RENone).*
        for (j, rj) <- coef; if j != i do
          coef(j) = star ++ rj
        eqs(i).base = star ++ eqs(i).base
        coef.remove(i)
        subst(i)
        i -= 1

      assert(eqs.head.coef.isEmpty)
      eqs.head.base
