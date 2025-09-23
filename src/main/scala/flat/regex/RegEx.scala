package flat.regex

/**
 * Regular expressions (regexes):
 *   - `RENone`: the empty set of strings.
 *   - `RENull`: the singleton set of the empty string.
 *   - `RELit(cs)`: the set of singleton strings taken from the charset `cs`.
 *   - `REConcat(r1, r2)`: concatenation of `r1` and `r2`.
 *   - `REUnion(r1, r2)`: union of `r1` and `r2`.
 *   - `REStar(r)`: Kleene star of `r`, i.e., repeat `r` zero or multiple times.
 */
enum RegEx:
  case RENone
  case RENull
  case RELit(chars: CharSet)
  case REConcat(left: RegEx, right: RegEx)
  case REUnion(left: RegEx, right: RegEx)
  case REStar(part: RegEx)

  // Constructors

  def ++(that: RegEx): RegEx = (this, that) match
    case (RENone, _) | (_, RENone) => RENone // ∅ ++ r = r ++ ∅ = ∅
    case (RENull, r) => r // ε ++ r = r
    case (r, RENull) => r // r ++ ε = r
    case (r1, r2) => REConcat(r1, r2)

  def |(that: RegEx): RegEx = (this, that) match
    case (RENone, r) => r // ∅ | r = r
    case (r, RENone) => r // r | ∅ = r
    case (r, RENull) if r.nullable => r // r | ε = r if r is nullable
    case (RENull, r) if r.nullable => r // r | ε = r if r is nullable
    case (r1, r2) if r1 == r2 => r1 // r | r = r
    case (r1, r2) => REUnion(r1, r2)

  def * : RegEx = this match
    case RENone => RENull // ∅* = ε
    case RENull => RENull // ε* = ε
    case r => REStar(r)

  /** Kleene plus: repeat this regex at least once. */
  inline def + : RegEx = this ++ this.*

  /** Option: repeat this regex at most once. */
  inline def ? : RegEx = this | RENull

  /** The `n`-th power of this regex: repeat it exactly `n` times. */
  def ^(n: Int): RegEx = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => RENull
    case 1 => this
    case _ => this ++ (this ^ (n - 1))

  /** Repeats this regex at least `m` times and at most `n` times,
   * where `m` and `n` are the lower and upper bounds of the given `interval`. */
  def loop(interval: Interval): RegEx = interval match
    case Interval(m, n: Int) => (this ^ m) ++ RegEx.union((for k <- 0 to (n - m) yield this ^ k).toList)
    case Interval(m, Inf) => (this ^ m) ++ this.*

  /** Tests if this regex denotes the empty set of strings. */
  def isEmpty: Boolean = this match
    case RENone => true
    case RENull => false
    case RELit(cs) => cs.isEmpty
    case REConcat(r1, r2) => r1.isEmpty || r2.isEmpty
    case REUnion(r1, r2) => r1.isEmpty && r2.isEmpty
    case REStar(_) => false

  /** Tests if the empty string is a member of this regex. */
  def nullable: Boolean = this match
    case RENone => false
    case RENull => true
    case RELit(_) => false
    case REConcat(r1, r2) => r1.nullable && r2.nullable
    case REUnion(r1, r2) => r1.nullable || r2.nullable
    case REStar(_) => true

  def minusNull: RegEx = this match
    case REConcat(r1, r2) => r1.minusNull ++ r2.minusNull
    case REUnion(RENull, r) => r
    case REUnion(r, RENull) => r
    case REUnion(r1, r2) => r1.minusNull | r2.minusNull
    case REStar(r) => r.+
    case _ => this

  /** Returns the ''first set'' of this regex. */
  def first: CharSet = this match
    case RENone => CharSet.empty
    case RENull => CharSet.empty
    case RELit(cs) => cs
    case REConcat(r1, r2) => r1.first | (if r1.nullable then r2.first else CharSet.empty)
    case REUnion(r1, r2) => r1.first | r2.first
    case REStar(r) => r.first

  /** Returns ''all'' chars that any member of this regex may contain. */
  def alphabet: CharSet = this match
    case RENone => CharSet.empty
    case RENull => CharSet.empty
    case RELit(cs) => cs
    case REConcat(r1, r2) => r1.alphabet | r2.alphabet
    case REUnion(r1, r2) => r1.alphabet | r2.alphabet
    case REStar(r) => r.alphabet

  /** Returns the reverse language of this regex. */
  def reverse: RegEx = this match
    case REConcat(r1, r2) => r2.reverse ++ r1.reverse
    case REUnion(r1, r2) => r1.reverse | r2.reverse
    case REStar(r) => r.reverse.*
    case _ => this

  /** Returns the Brzozowski derivative of this regex at the char `c`. */
  def derivative(c: Char): RegEx = this match
    case RENone => RENone
    case RENull => RENone
    case RELit(cs) => if cs.contains(c) then RENull else RENone
    case REConcat(r1, r2) =>
      if r1.nullable then (r1.derivative(c) ++ r2) | r2.derivative(c)
      else r1.derivative(c) ++ r2
    case REUnion(r1, r2) => r1.derivative(c) | r2.derivative(c)
    case REStar(r) => r.derivative(c) ++ this

  /** Returns the Brzozowski derivative of this regex at the string `t`. */
  def derivative(s: String): RegEx = if s.isEmpty then this else derivative(s.head).derivative(s.tail)

  /** Tests if `s` is a member of this language. */
  def contains(s: String): Boolean = derivative(s).nullable

  /** Tests if this language is a subset of `that` language. */
  infix def subsetOf(that: RegEx): Boolean = RESub.check(this, that)

  /** Tests if this RE is ''semantically'' equivalent to `that`. */
  infix def equiv(that: RegEx): Boolean = this.subsetOf(that) && that.subsetOf(this)

  override def toString: String = this match
    case RENone => "∅"
    case RENull => "ε"
    case RELit(cs) =>
      if cs.isEmpty then "∅"
      else if cs.isFull then "."
      else if cs.isSingleton then cs.chars.head.toString
      else "[" + cs.toString + "]"
    case REConcat(r1, r2) => s"$r1$r2"
    case REUnion(r1, r2) => s"($r1|$r2)"
    case REStar(r) => paren(r.toString) + "*"

  private def paren(s: String): String = if s.startsWith("(") || s.length == 1 then s else s"($s)"

object RegEx:
  /** The set of all strings of length 1 (POSIX `.`). */
  val allChar: RegEx = RELit(CharSet.full)

  /** The set of all strings (POSIX `.*`). */
  val all: RegEx = allChar.*

  /** Big union. */
  def union(cases: List[RegEx]): RegEx = if cases.isEmpty then RENone else cases.reduceRight(_ | _)

  def union(cases: RegEx*): RegEx = union(cases.toList)

  /** Big concatenation. */
  def concat(parts: List[RegEx]): RegEx = if parts.isEmpty then RENull else parts.reduceRight(_ ++ _)

  def concat(parts: RegEx*): RegEx = concat(parts.toList)

  def fromChar(c: Char): RegEx = RELit(CharSet(c))

  def fromString(s: String): RegEx = concat(s.map(fromChar).toList)

  def fromCharSet(cs: CharSet): RegEx = if cs.isEmpty then RENone else RELit(cs)

  def parse(regex: String): RegEx = REParser.tryParse(regex) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r
