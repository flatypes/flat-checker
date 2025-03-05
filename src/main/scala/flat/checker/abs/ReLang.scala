package flat.checker.abs

import flat.checker.Bound.PosInf

import scala.annotation.targetName

enum ReLang:
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

  @targetName("star")
  def * : ReLang = ReStar(this)

  @targetName("plus")
  def + : ReLang = ReConcat(this, ReStar(this))

  @targetName("opt")
  def ? : ReLang = ReUnion(ReEmpty, this)

  @targetName("rep")
  def ^(k: Int): ReLang = k match
    case 0 => ReEmpty
    case 1 => this
    case k if k >= 2 => ReConcat(this, this ^ (k - 1))
    case _ => throw IllegalArgumentException()

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
      case ReEmpty => true
      case ReChars(_) => false
      case ReConcat(r1, r2) => r1.nullable && r2.nullable
      case ReUnion(r1, r2) => r1.nullable || r2.nullable
      case ReStar(_) => true

  /** First set: a set of possible first characters. */
  def first: CharSet =
    this match
      case ReEmpty => CharSet.empty
      case ReChars(cs) => cs
      case ReConcat(r1, r2) => if r1.nullable then r1.first | r2.first else r1.first
      case ReUnion(r1, r2) => r1.first | r2.first
      case ReStar(r) => r.first

  /** Brzozowski derivative. Return `None` for the none language ∅. */
  def derivative(x: CharSet): Option[ReLang] =
    this match
      case ReEmpty => None
      case ReChars(cs) =>
        if cs ** x then None else Some(ReEmpty)
      case ReConcat(r1, r2) =>
        ReLang.langUnion(
          for r <- r1.derivative(x) yield ReConcat(r, r2),
          if r1.nullable then r2.derivative(x) else None
        )
      case ReUnion(r1, r2) =>
        ReLang.langUnion(r1.derivative(x), r2.derivative(x))
      case ReStar(r) =>
        for r1 <- r.derivative(x) yield ReConcat(r1, this)

  def reverse: ReLang = this match
    case ReEmpty | ReChars(_) => this
    case ReConcat(r1, r2) => ReConcat(r2.reverse, r1.reverse)
    case ReUnion(r1, r2) => ReUnion(r1.reverse, r2.reverse)
    case ReStar(r) => ReStar(r.reverse)

  def length: Range = this match
    case ReEmpty => 0
    case ReChars(_) => 1
    case ReConcat(r1, r2) => r1.length + r2.length
    case ReUnion(r1, r2) => r1.length | r2.length
    case ReStar(_) => Range(0, PosInf)

  def alphabet: CharSet = this match
    case ReEmpty => CharSet.empty
    case ReChars(cs) => cs
    case ReConcat(r1, r2) => r1.alphabet | r2.alphabet
    case ReUnion(r1, r2) => r1.alphabet | r2.alphabet
    case ReStar(r) => r.alphabet

  def contains(c: Char): ABool = this match
    case ReEmpty => ABool.False
    case ReChars(cs) if cs.isSingleton => if cs.contains(c) then ABool.True else ABool.False
    case ReChars(cs) => if cs.contains(c) then ABool.Top else ABool.False
    case ReConcat(r1, r2) => r1.contains(c) || r2.contains(c)
    case ReUnion(r1, r2) => r1.contains(c) | r2.contains(c)
    case ReStar(r) => r.contains(c) match
      case ABool.True => ABool.Top
      case b => b

  def neverContain(c: Char): Boolean = contains(c) == ABool.False

  def isNumber: Boolean = alphabet.subsetOf(CharSet.NUM)

  override def toString: String =
    this match
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
    regexes.toList match
      case Nil => assert(false)
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

  def fromString(value: String): ReLang = mkConcat(value.map(c => ReChars(CharSet.of(c))) *)

  def fromCNF(cnf: List[ReLang]): ReLang = mkConcat(cnf *)

given AbsDom[ReLang]:
  import ReLang.*

  def top: ReLang = full

  def bot: ReLang = empty

  def subElement(r1: ReLang, r2: ReLang): Boolean =
    if r2 == full || r1 == r2 then true else false // TODO: subtype check

  def join(r1: ReLang, r2: ReLang): ReLang =
    if subElement(r1, r2) then r2
    else if subElement(r2, r1) then r1
    else ReUnion(r1, r2)

  def widen(r1: ReLang, r2: ReLang): ReLang =
    if subElement(r1, r2) then r2
    else if subElement(r2, r1) then r1
    else ReUnion(r1, r2)
