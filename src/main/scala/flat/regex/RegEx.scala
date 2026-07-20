package flat.regex

/**
 * Regular Expressions (REs):
 *   - `RENone`: the empty set of strings.
 *   - `RENull`: the singleton set of the empty string.
 *   - `RELit(cs)`: the set of singleton strings taken from the charset `cs`.
 *   - `REConcat(r1, r2)`: concatenation of `r1` and `r2`.
 *   - `REUnion(r1, r2)`: union of `r1` and `r2`.
 *   - `REStar(r)`: Kleene star of `r`, i.e., repeat `r` zero or multiple times.
 */
enum RegEx extends Domain:
  case RENone
  case RENull
  case RELit(chars: CharSet)
  case REConcat(left: RegEx, right: RegEx)
  case REUnion(left: RegEx, right: RegEx)
  case REStar(part: RegEx)

  /** Concatenation. */
  def ++(that: RegEx): RegEx = (this, that) match
    case (RENone, _) | (_, RENone) => RENone // ∅ ++ r = r ++ ∅ = ∅
    case (RENull, r) => r // ε ++ r = r
    case (r, RENull) => r // r ++ ε = r
    case (r1, r2) => REConcat(r1, r2)

  /** Union. */
  def |(that: RegEx): RegEx = (this, that) match
    case (RENone, r) => r // ∅ | r = r
    case (r, RENone) => r // r | ∅ = r
    case (r, RENull) if r.nullable => r // r | ε = r if r is nullable
    case (RENull, r) if r.nullable => r // r | ε = r if r is nullable
    case (r1, r2) if r1 == r2 => r1 // r | r = r
    case (r1, r2) => REUnion(r1, r2)

  /** Kleene star. */
  def * : RegEx = this match
    case RENone => RENull // ∅* = ε
    case RENull => RENull // ε* = ε
    case r => REStar(r)

  /** Kleene plus: repeat ''at least'' once. */
  inline def + : RegEx = this ++ this.*

  /** Optional: repeat ''at most'' once. */
  inline def ? : RegEx = RENull | this

  /** Power: repeat ''exactly'' `n` times.
   * Require: `n` is nonnegative. */
  def ^(n: Int): RegEx = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => RENull
    case 1 => this
    case _ => this ++ (this ^ (n - 1))

  /** Loop: repeat a number of times as specified in the `interval`.
   * Require: the lower bound of the interval is nonnegative. */
  def loop(interval: Interval): RegEx = interval match
    case Interval(m: Int, n: Int) => (this ^ m) ++ RegEx.union((for k <- 0 to (n - m) yield this ^ k).toList)
    case Interval(m: Int, Inf) => (this ^ m) ++ this.*
    case _ => assert(false)

  /** Tests if this RE is semantically equivalent to the empty set. */
  def isEmpty: Boolean = this match
    case RENone => true
    case RENull => false
    case RELit(cs) => cs.isEmpty
    case REConcat(r1, r2) => r1.isEmpty || r2.isEmpty
    case REUnion(r1, r2) => r1.isEmpty && r2.isEmpty
    case REStar(_) => false

  /** Tests if the empty string is a member. */
  lazy val nullable: Boolean = this match
    case RENone => false
    case RENull => true
    case RELit(_) => false
    case REConcat(r1, r2) => r1.nullable && r2.nullable
    case REUnion(r1, r2) => r1.nullable || r2.nullable
    case REStar(_) => true

  def minusNull: RegEx = this match
    case RENull => RENone
    case REConcat(RENull, r) => r.minusNull
    case REConcat(r, RENull) => r.minusNull
    case REConcat(r1, r2) => r1.minusNull ++ r2.minusNull
    case REUnion(RENull, r) => r
    case REUnion(r, RENull) => r
    case REUnion(r1, r2) => r1.minusNull | r2.minusNull
    case REStar(r) => r.+
    case _ => this

  /** Tests if this language is a singleton. */
  def isSingleton: Boolean = this match
    case RENone => false
    case RENull => true
    case RELit(cs) => cs.isSingleton
    case REConcat(r1, r2) => r1.isSingleton && r2.isSingleton
    case REUnion(_, _) => false
    case REStar(_) => false

  /** Returns the ''first set'': all possible leading characters. */
  def first: CharSet = this match
    case RENone => CharSet.empty
    case RENull => CharSet.empty
    case RELit(cs) => cs
    case REConcat(r1, r2) => r1.first | (if r1.nullable then r2.first else CharSet.empty)
    case REUnion(r1, r2) => r1.first | r2.first
    case REStar(r) => r.first

  /** Returns ''all'' possible characters that any member of this RE contains. */
  def alphabet: CharSet = this match
    case RENone => CharSet.empty
    case RENull => CharSet.empty
    case RELit(cs) => cs
    case REConcat(r1, r2) => r1.alphabet | r2.alphabet
    case REUnion(r1, r2) => r1.alphabet | r2.alphabet
    case REStar(r) => r.alphabet

  /** Returns the reverse RE. */
  def reverse: RegEx = this match
    case REConcat(r1, r2) => r2.reverse ++ r1.reverse
    case REUnion(r1, r2) => r1.reverse | r2.reverse
    case REStar(r) => r.reverse.*
    case r => r

  /** Returns the Brzozowski derivative at the character `c`. */
  def deriv(c: Char): RegEx = this match
    case RENone => RENone
    case RENull => RENone
    case RELit(cs) => if cs.contains(c) then RENull else RENone
    case REConcat(r1, r2) =>
      if r1.nullable then (r1.deriv(c) ++ r2) | r2.deriv(c)
      else r1.deriv(c) ++ r2
    case REUnion(r1, r2) => r1.deriv(c) | r2.deriv(c)
    case REStar(r) => r.deriv(c) ++ this

  /** Returns the Brzozowski derivative at the string `t`. */
  def deriv(t: String): RegEx =
    var r = this
    for c <- t do
      r = r.deriv(c)
    r

  def deriv(set: CharSet): RegEx = this match
    case RENone => RENone
    case RENull => RENone
    case RELit(s) => if (s & set).isEmpty then RENone else RENull
    case REConcat(r1, r2) =>
      if r1.nullable then (r1.deriv(set) ++ r2) | r2.deriv(set)
      else r1.deriv(set) ++ r2
    case REUnion(r1, r2) => r1.deriv(set) | r2.deriv(set)
    case REStar(r) => r.deriv(set) ++ this

  /** Returns the Brzozowski derivative at any char. */
  def derivativeAny: RegEx = this match
    case RENone => RENone
    case RENull => RENone
    case RELit(cs) => if !cs.isEmpty then RENull else RENone
    case REConcat(r1, r2) =>
      if r1.nullable then (r1.derivativeAny ++ r2) | r2.derivativeAny
      else r1.derivativeAny ++ r2
    case REUnion(r1, r2) => r1.derivativeAny | r2.derivativeAny
    case REStar(r) => r.derivativeAny ++ this

  /** Tests if the given string `s` is a member. */
  def contains(s: String): Boolean =
    if s.isEmpty then nullable else deriv(s).nullable

  /** Tests if this RE is subset of `that`. */
  infix def subsetOf(that: RegEx): Boolean = RESub.check(this, that)

  /** Tests if this RE is semantically equivalent to `that`. */
  infix def equiv(that: RegEx): Boolean = this.subsetOf(that) && that.subsetOf(this)

  def toCNF: List[RegEx] = this match
    case REConcat(r1, r2) => r1.toCNF ++ r2.toCNF
    case r => List(r)

  def toDNF: List[RegEx] = this match
    case REUnion(r1, r2) => r1.toDNF ++ r2.toDNF
    case r => List(r)

  def toJavaRegex: String = this match
    case RENone => "[]"
    case RENull => ""
    case RELit(cs) => cs.toJavaRegex
    case REConcat(r1, r2) => r1.toJavaRegex + r2.toJavaRegex
    case REUnion(r1, r2) => "(" + r1.toJavaRegex + "|" + r2.toJavaRegex + ")"
    case REStar(r) => "(" + r.toJavaRegex + ")*"

  override def toString: String = this match
    case RENone => "∅"
    case RENull => "ε"
    case RELit(cs) =>
      if cs.isEmpty then "∅"
      else if cs.isFull then "."
      else if cs.isSingleton then cs.head.toString
      else cs.toString
    case REConcat(r1, r2) => s"$r1$r2"
    case REUnion(r1, r2) => s"($r1|$r2)"
    case REStar(r) =>
      val s = r.toString
      val s1 = if s.startsWith("(") || s.length == 1 then s else "(" + s + ")"
      s1 + "*"

object RegEx:
  /** RE `.`: the set of all strings of length 1. */
  val allChar: RegEx = RELit(CharSet.full)

  /** RE `.*`: the full set of strings. */
  val all: RegEx = allChar.*

  /** Big union. */
  def union(cases: List[RegEx]): RegEx = if cases.isEmpty then RENone else cases.reduceRight(_ | _)

  /** Big union. */
  def union(cases: RegEx*): RegEx = union(cases.toList)

  /** Big concatenation. */
  def concat(parts: List[RegEx]): RegEx = if parts.isEmpty then RENull else parts.reduceRight(_ ++ _)

  /** Big concatenation. */
  def concat(parts: RegEx*): RegEx = concat(parts.toList)

  /** Creates a singleton RE of the singleton string `c`. */
  def fromChar(c: Char): RegEx = RELit(CharSet(c))

  /** Creates a singleton RE of the given string `s`. */
  def fromString(s: String): RegEx = concat(s.map(fromChar).toList)

  /** Creates an RE that matches the given charset `cs`. */
  def fromCharSet(cs: CharSet): RegEx = if cs.isEmpty then RENone else RELit(cs)

  /** Parse an RE from the given `regex` literal. */
  def parse(regex: String): RegEx = REParser.tryParse(regex) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r
