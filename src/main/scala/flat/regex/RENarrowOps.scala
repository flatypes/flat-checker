package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.regex.REAbsOps.splitBy
import flat.regex.RegEx.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object RENarrowOps:
  extension (regEx: RegEx)
    // prefix, suffix
    def filterStartWith(t: String): RegEx =
      val r1 = regEx.deriv(t)
      if r1.isEmpty then RENone else fromString(t) ++ r1

    def filterNonEmpty: RegEx = regEx match
      case RENone | RENull => RENone
      case RELit(_) => regEx
      case REConcat(r1, r2) => if r1.nullable then r2.filterNonEmpty | (r1.filterNonEmpty ++ r2) else regEx
      case REUnion(r1, r2) => r1.filterNonEmpty | r2.filterNonEmpty
      case REStar(r) => r.filterNonEmpty.+

    def filterNotStartWith(c: Char): RegEx = regEx match
      case RENone | RENull => regEx
      case RELit(s) => fromCharSet(s - c)
      case REConcat(r1, r2) =>
        if r1.nullable then r2.filterNotStartWith(c) | (r1.filterNonEmpty.filterNotStartWith(c) ++ r2)
        else r1.filterNotStartWith(c) ++ r2
      case REUnion(r1, r2) => r1.filterNotStartWith(c) | r2.filterNotStartWith(c)
      case REStar(r) => RENull | (r.filterNonEmpty.filterNotStartWith(c) ++ regEx)

    def filterNotStartWith(t: String): RegEx =
      if t.isEmpty then RENone
      else filterNotStartWith(t.head) | (fromChar(t.head) ++ regEx.deriv(t.head).filterNotStartWith(t.tail))

    def filterEndWith(t: String): RegEx = regEx.reverse.filterStartWith(t.reverse).reverse

    def filterNotEndWith(t: String): RegEx = regEx.reverse.filterNotStartWith(t.reverse).reverse

    // equality
    def filterEq(t: String): RegEx =
      if regEx.contains(t) then fromString(t) else RENone

    def filterNe(t: String): RegEx =
      filterNotStartWith(t) | (fromString(t) ++ regEx.deriv(t).filterNonEmpty)

    // infix
    def filterContain(c: Char): RegEx = regEx match
      case RENone | RENull => RENone
      case RELit(s) => if s.contains(c) then fromChar(c) else RENone
      case REConcat(r1, r2) => (r1.filterContain(c) ++ r2) | (r1 ++ r2.filterContain(c))
      case REUnion(r1, r2) => r1.filterContain(c) | r2.filterContain(c)
      case REStar(r) => regEx ++ r.filterContain(c) ++ regEx

    def filterContain(t: String): RegEx =
      if t.isEmpty then regEx
      else
        val rs =
          for
            (r1, r2) <- regEx.splitBy(t.head)
            r2f = r2.filterStartWith(t.tail)
            if !r2f.isEmpty
          yield r1 ++ fromChar(t.head) ++ r2f
        union(rs)

    def filterNotContain(c: Char): RegEx = regEx match
      case RENone | RENull => regEx
      case RELit(s) => fromCharSet(s - c)
      case REConcat(r1, r2) => r1.filterNotContain(c) ++ r2.filterNotContain(c)
      case REUnion(r1, r2) => r1.filterNotContain(c) | r2.filterNotContain(c)
      case REStar(r) => r.filterNotContain(c).*

    /** Compute the subset of `r` such that `t` is not contained in any string in it. */
    def filterNotContain(t: String): RegEx = t.length match
      case 0 => RENone
      case 1 => filterNotContain(t.head)
      case _ =>
        regEx match
          case RENone | RENull | RELit(_) => regEx
          case REConcat(r1, r2) =>
            if !r1.alphabet.contains(t.head) then r1 ++ r2.filterNotContain(t)
            else FilterNotContainSolver(regEx, t).solve
          case REUnion(r1, r2) => r1.filterNotContain(t) | r2.filterNotContain(t)
          case REStar(r) =>
            if !r.alphabet.contains(t.head) then regEx
            else FilterNotContainSolver(regEx, t).solve

private class FilterNotContainSolver(regEx: RegEx, t: String) extends LazyLogging:
  // Matching state: the first `n` characters of `t` have been matched
  final case class State(n: Int):
    def isError: Boolean = n == t.length

    def next(c: Char): State =
      if isError then this
      else
        val s = t.take(n) + c
        val longestSuffix = s.indices.map(s.drop).find(t.startsWith).getOrElse("")
        State(longestSuffix.length)

  private type Variable = (RegEx, State)

  // Term in equation system: sum_k(factors(k) * X_k) + base
  final case class Term(base: RegEx, factors: Map[Int, RegEx]):
    def subst(k: Int, term: Term): Term =
      if factors.contains(k) then
        val r = factors(k)
        val m = for (i, ri) <- term.factors yield i -> ((r ++ ri) | factors.getOrElse(i, RENone))
        Term((r ++ term.base) | base, factors - k ++ m)
      else
        this

  def solve: RegEx =
    val alphabet = regEx.alphabet
    var sets = t.toSet.filter(alphabet.contains).toList.map(CharSet(_))
    val others = alphabet -- t.toSet
    if !others.isEmpty then sets = sets :+ others
    val variables = ListBuffer((regEx, State(0)))
    val terms = ListBuffer.empty[Term]

    // Step 1: collect equations
    var i = 0
    while i < variables.length do
      val (r, st) = variables(i)
      val base = if r.nullable && !st.isError then RENull else RENone
      val factors = Map.from:
        for
          s <- sets
          r1 = r.deriv(s)
          st1 = if s.isSingleton then st.next(s.head) else State(0)
          if !r1.isEmpty && !st1.isError
        yield
          val variable = (r1, st1)
          var j = variables.indexOf(variable)
          if j == -1 then
            j = variables.length
            variables += variable
          j -> RegEx.fromCharSet(s)
      terms.append(Term(base, factors))
      i += 1

    // Step 2: solve equations via Arden's lemma: X = AX + B => X = A^* B
    for k <- terms.indices.reverse do
      val term = terms(k)
      val star = term.factors.getOrElse(k, RENone).*
      terms(k) = Term(star ++ term.base, for (i, r) <- term.factors; if i != k yield i -> (star ++ r))
      for i <- 0 until k do
        terms(i) = terms(i).subst(k, terms(k))
    assert(terms.head.factors.isEmpty)
    terms.head.base
