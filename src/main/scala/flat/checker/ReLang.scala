package flat.checker

import flat.checker
import flat.checker.Bound.PosInf
import flat.checker.py.RegexParser

enum ReLang:
  /** Empty language. */
  case ReNone
  /** Empty string (ε). */
  case ReEmpty
  /** Set of characters in `cs`. */
  case ReChars(cs: CharSet)
  /** Concatenation (r₁ ⋅ r₂). */
  case ReConcat(r1: ReLang, r2: ReLang)
  /** Union (r₁ | r₂). */
  case ReUnion(r1: ReLang, r2: ReLang)
  /** Kleene closure (r*): repeating the element zero or multiple times. */
  case ReStar(r: ReLang)

  def * : ReLang = ReStar(this)

  def + : ReLang = ReConcat(this, ReStar(this))

  def ? : ReLang = ReUnion(ReEmpty, this)

  def ^(k: Int): ReLang = k match
    case 0 => ReEmpty
    case 1 => this
    case k if k >= 2 => ReConcat(this, this ^ (k - 1))
    case _ => throw IllegalArgumentException(k.toString)

  def loop(lb: Bound, ub: Bound): ReLang =
    val rep =
      if ub == PosInf then ReStar(this)
      else ReLang.mkUnion((for i <- 0 to (ub.asInt - lb.asInt) yield this ^ i) *)
    if lb.asInt == 0 then rep else ReLang.ReConcat(this ^ lb.asInt, rep)

  /** Convert to concatenation normal form. */
  def toCNF: List[ReLang] =
    this match
      case ReEmpty => Nil
      case ReConcat(r1, r2) => r1.toCNF ++ r2.toCNF
      case r => r :: Nil

  /** Convert to union normal form. */
  def toUNF: List[ReLang] =
    this match
      case ReUnion(r1, r2) => r1.toUNF ++ r2.toUNF
      case r => r :: Nil

  def isEmpty: Boolean =
    this match
      case ReNone => true
      case ReEmpty => false
      case ReChars(cs) => cs.isEmpty
      case ReConcat(r1, r2) => r1.isEmpty || r2.isEmpty
      case ReUnion(r1, r2) => r1.isEmpty && r2.isEmpty
      case ReStar(r) => false

  /** Basic normalization that removes redundant `ε` (in concatenation) and `∅` (in union). */
  def normalize: ReLang =
    this match
      case ReNone | ReEmpty | ReChars(_) => this
      case ReConcat(_, _) =>
        val terms = toCNF.map(_.normalize).filter(_ != ReEmpty)
        if terms.contains(ReNone) then ReNone
        else if terms.isEmpty then ReEmpty
        else terms.reduce(ReConcat.apply)
      case ReUnion(_, _) =>
        val terms = toUNF.map(_.normalize).filter(_ != ReNone)
        if terms.isEmpty then ReNone else terms.reduce(ReUnion.apply)
      case ReStar(r) =>
        r.normalize match
          case ReNone | ReEmpty => ReEmpty
          case ReStar(r1) => ReStar(r1)
          case r1 => ReStar(r1)

  /** Language equivalence. For now, simply check the syntactic equality of their normalizations. */
  infix def equiv(other: ReLang): Boolean = normalize == other.normalize

  def isChar: Boolean =
    this match
      case ReChars(cs) => cs.isSingleton
      case _ => false

  def asChar: Char =
    require(isChar)
    this match
      case ReChars(cs) => cs.asChar
      case _ => assert(false)

  def isString: Boolean =
    toCNF.forall(_.isChar)

  def asString: String =
    require(isString)
    toCNF.map(_.asChar).mkString

  def nullable: Boolean =
    this match
      case ReNone | ReEmpty => true
      case ReChars(_) => false
      case ReConcat(r1, r2) => r1.nullable && r2.nullable
      case ReUnion(r1, r2) => r1.nullable || r2.nullable
      case ReStar(_) => true

  /** First set: a set of possible first characters. */
  def first: CharSet =
    this match
      case ReNone | ReEmpty => CharSet.empty
      case ReChars(cs) => cs
      case ReConcat(r1, r2) => if r1.nullable then r1.first | r2.first else r1.first
      case ReUnion(r1, r2) => r1.first | r2.first
      case ReStar(r) => r.first

  /** Brzozowski derivative (to a char). Return `None` for the empty language `∅`. */
  def derivative(x: Char): Option[ReLang] = derivative(CharSet.of(x))

  /** Positive derivative to a char set `x`: a superset of `{derivative(c) | c in x}`.
   * Return `None` for `∅`. */
  def derivative(x: CharSet): Option[ReLang] =
    this match
      case ReNone | ReEmpty => None
      case ReChars(cs) => if cs ** x then None else Some(ReEmpty)
      case ReConcat(r1, r2) =>
        ReLang.langUnion(
          for r <- r1.derivative(x) yield ReLang.mkConcat(r, r2),
          if r1.nullable then r2.derivative(x) else None
        )
      case ReUnion(r1, r2) =>
        ReLang.langUnion(r1.derivative(x), r2.derivative(x))
      case ReStar(r) =>
        for r1 <- r.derivative(x) yield ReConcat(r1, this)

  def reverse: ReLang = this match
    case ReNone | ReEmpty | ReChars(_) => this
    case ReConcat(r1, r2) => ReConcat(r2.reverse, r1.reverse)
    case ReUnion(r1, r2) => ReUnion(r1.reverse, r2.reverse)
    case ReStar(r) => ReStar(r.reverse)

  def length: Interval = this match
    case ReNone | ReEmpty => 0
    case ReChars(_) => 1
    case ReConcat(r1, r2) => r1.length + r2.length
    case ReUnion(r1, r2) => r1.length | r2.length
    case ReStar(_) => Interval(0, PosInf)

  def alphabet: CharSet = this match
    case ReNone | ReEmpty => CharSet.empty
    case ReChars(cs) => cs
    case ReConcat(r1, r2) => r1.alphabet | r2.alphabet
    case ReUnion(r1, r2) => r1.alphabet | r2.alphabet
    case ReStar(r) => r.alphabet

  def contains(c: Char): Ternary = this match
    case ReNone | ReEmpty => Ternary.False
    case ReChars(cs) if cs.isSingleton => if cs.contains(c) then Ternary.True else Ternary.False
    case ReChars(cs) => if cs.contains(c) then Ternary.Maybe else Ternary.False
    case ReConcat(r1, r2) => r1.contains(c) || r2.contains(c)
    case ReUnion(r1, r2) => r1.contains(c) | r2.contains(c)
    case ReStar(r) => r.contains(c) match
      case Ternary.True => Ternary.Maybe
      case b => b

  def neverContain(c: Char): Boolean = contains(c) == Ternary.False

  def isNumber: Boolean = alphabet.subsetOf(CharSet.NUM)

  private def size: Option[Int] = this match
    case ReNone => Some(0)
    case ReEmpty => Some(1)
    case ReChars(cs) => Some(cs.size)
    case ReConcat(r1, r2) => for n1 <- r1.size; n2 <- r2.size yield n1 * n2
    case ReUnion(r1, r2) => for n1 <- r1.size; n2 <- r2.size yield n1 + n2
    case ReStar(r) => None

  def isSmall: Boolean = normalize.size.exists(_ < 20)

  def getLang: Set[String] =
    require(isSmall)
    normalize match
      case ReNone => Set.empty
      case ReEmpty => Set("")
      case ReChars(cs) => cs.getChars.map(_.toString)
      case ReConcat(r1, r2) => for s1 <- r1.getLang; s2 <- r2.getLang yield s1 + s2
      case ReUnion(r1, r2) => r1.getLang | r2.getLang
      case ReStar(r) => assert(false)

  override def toString: String =
    this match
      case ReNone => "None"
      case ReEmpty => "ε"
      case ReChars(cs) => cs.prettyString
      case ReConcat(r1, r2) => s"$r1$r2"
      case ReUnion(r1, r2) => "(" + toUNF.mkString("|") + ")"
      case ReStar(r) =>
        val s = r.toString
        if s.length == 1 || s.startsWith("(") then s"$s*" else s"($s)*"

object ReLang:
  /** Bottom: empty language (∅). */
  val empty: ReLang = ReChars(CharSet.empty)

  /** Top: full language (Σ*). */
  val full: ReLang = ReStar(ReChars(CharSet.full))

  /** Set of all characters (Σ). */
  val allChar: ReLang = ReChars(CharSet.full)

  val digit: ReLang = ReChars(CharSet.NUM)

  val number: ReLang = ReChars(CharSet.NUM).+

  def mkConcat(regexes: ReLang*): ReLang =
    regexes.toList.filterNot(_ == ReEmpty) match
      case Nil => ReEmpty
      case r :: Nil => r
      case rs => rs.reduce(ReConcat.apply)

  def mkUnion(regexes: ReLang*): ReLang =
    regexes.toList match
      case Nil => assert(false)
      case r :: Nil => r
      case rs => rs.reduce(ReUnion.apply)

  def mkRange(lb: Char, ub: Char): ReLang = ReChars(CharSet.from(lb to ub))

  def langUnion(lang1: Option[ReLang], lang2: Option[ReLang]): Option[ReLang] =
    (lang1, lang2) match
      case (None, _) => lang2
      case (_, None) => lang1
      case (Some(r1), Some(r2)) => Some(ReUnion(r1, r2))

  def fromChar(value: Char): ReLang = ReChars(CharSet.of(value))

  given Conversion[Char, ReLang] = fromChar

  def fromString(value: String): ReLang = mkConcat(value.map(c => ReChars(CharSet.of(c))) *)

  given Conversion[String, ReLang] = fromString

  def fromCNF(cnf: List[ReLang]): ReLang = mkConcat(cnf *)

  def fromPython(regex: String): ReLang =
    RegexParser(regex) match
      case Left(msg) => throw IllegalArgumentException(msg)
      case Right(r) => r

given Lattice[ReLang]:
  import ReLang.*

  def top: ReLang = full

  def bot: ReLang = empty

  def subElement(r1: ReLang, r2: ReLang): Boolean =
    if r2 == full then true else ReLangSub.check(r1, r2)

  def join(r1: ReLang, r2: ReLang): ReLang =
    if subElement(r1, r2) then r2
    else if subElement(r2, r1) then r1
    else ReUnion(r1, r2)

  def meet(r1: ReLang, r2: ReLang): ReLang = ???