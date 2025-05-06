package flat.regex

import flat.regex.RegExpr.mkConcat

enum RegExpr:
  case RENone
  case RENull
  case REChar(charSet: CharSet)
  case REConcat(left: RegExpr, right: RegExpr)
  case REUnion(left: RegExpr, right: RegExpr)
  case RELoop(quantity: NatRange, elem: RegExpr)

  require:
    this match
      case REChar(cs) => cs.nonEmpty
      case REConcat(r1, r2) => r1 != RENone && r2 != RENone
      case REUnion(r1, r2) => r1 != RENone && r2 != RENone
      case RELoop(range, r) => r != RENone && range != NatRange.at(0) && range != NatRange.at(1)
      case _ => true

  def * : RegExpr = RELoop(NatRange(0), this)

  def + : RegExpr = RELoop(NatRange(1), this)

  def ? : RegExpr = RELoop(NatRange(0, 1), this)

  def ^(k: Int): RegExpr =
    require(k >= 0)
    k match
      case 0 => RENull
      case 1 => this
      case _ => RELoop(NatRange.at(k), this)

  def tryAsString: Option[String] = this match
    case RENull => Some("")
    case REChar(cs) if cs.isSingleton => Some(cs.chars.head.toString)
    case REConcat(r1, r2) =>
      for s1 <- r1.tryAsString; s2 <- r2.tryAsString yield s1 + s2
    case REUnion(r1, r2) =>
      for s1 <- r1.tryAsString; s2 <- r2.tryAsString; if s1 == s2 yield s1
    case _ => None

  def tryAsChar: Option[Char] = this match
    case REChar(cs) if cs.isSingleton => Some(cs.chars.head)
    case REUnion(r1, r2) =>
      for c1 <- r1.tryAsChar; c2 <- r2.tryAsChar; if c1 == c2 yield c1
    case _ => None

  def nullable: Boolean = this match
    case RENone => false
    case RENull => true
    case REChar(_) => false
    case REConcat(r1, r2) => r1.nullable && r2.nullable
    case REUnion(r1, r2) => r1.nullable || r2.nullable
    case RELoop(range, r) => range.lower == 0 || r.nullable

  def reverse: RegExpr = this match
    case RENone | RENull | REChar(_) => this
    case REConcat(r1, r2) => mkConcat(r2.reverse.toCNF ++ r1.reverse.toCNF)
    case REUnion(r1, r2) => REUnion(r1.reverse, r2.reverse)
    case RELoop(range, r) => RELoop(range, r.reverse)

  def normalize: RegExpr = this match
    case REConcat(r1, r2) => RegExpr.mkConcat(r1.normalize.toCNF ++ r2.normalize.toCNF)
    case REUnion(r1, r2) =>
      val r11 = r1.normalize
      val r21 = r2.normalize
      if r11 == RENull || r21 == RENull then RegExpr.mkUnion(RENull :: List(r11, r21).filter(_ != RENull))
      else RegExpr.mkUnion(r11, r21)
    case RELoop(q, r) =>
      r.normalize match
        case RENone if q.lower == 0 => RENull
        case RENull => RENull
        case r1 => RELoop(q, r1)
    case _ => this

  /** Language equivalence. For now, simply check the syntactic equality of their normalizations. */
  infix def equiv(other: RegExpr): Boolean = normalize == other.normalize

  def subsetOf(that: RegExpr): Boolean = RESub.check(this, that)

  def toCNF: List[RegExpr] = this match
    case RENone => throw IllegalArgumentException("∅")
    case RENull => Nil
    case REConcat(r1, r2) => r1.toCNF ++ r2.toCNF
    case _ => List(this)

  override def toString: String = this match
    case RENone => "∅"
    case RENull => "ε"
    case REChar(cs) =>
      if cs.isEmpty then "∅"
      else if cs.isFull then "."
      else if cs.isSingleton then cs.chars.head.toString
      else "[" + cs.toString + "]"
    case REConcat(r1, r2) => s"$r1$r2"
    case REUnion(r1, r2) => s"($r1|$r2)"
    case RELoop(NatRange(0, None), r) => paren(r.toString) + "*"
    case RELoop(NatRange(1, None), r) => paren(r.toString) + "+"
    case RELoop(NatRange(0, Some(1)), r) => paren(r.toString) + "?"
    case RELoop(range, r) => paren(r.toString) + range.toString

  private def paren(s: String): String = if s.startsWith("(") || s.length == 1 then s else s"($s)"

object RegExpr:
  val allChar: RegExpr = REChar(CharSet.full)

  val all: RegExpr = allChar.*

  val number: RegExpr = REChar(CharSet.NUM).+

  def fromChar(c: Char): RegExpr = REChar(CharSet.of(c))

  def fromString(s: String): RegExpr = mkConcat(s.map(fromChar).toList)

  def mkChar(cs: CharSet): RegExpr = if cs.isEmpty then RENone else REChar(cs)

  def mkConcat(elems: List[RegExpr]): RegExpr =
    if elems.contains(RENone) then RENone
    else if elems.forall(_ == RENull) then RENull
    else elems.filterNot(_ == RENull).reduce(REConcat(_, _))

  def mkConcat(elems: RegExpr*): RegExpr = mkConcat(elems.toList)

  def mkUnion(cases: List[RegExpr]): RegExpr =
    if cases.forall(_ == RENone) then RENone
    else
      val rs1 = cases.filterNot(_ == RENone)
      val rs2 = if rs1.exists(r => r != RENull && r.nullable) then rs1.filterNot(_ == RENull) else rs1
      rs2.reduce(REUnion(_, _))

  def mkUnion(cases: RegExpr*): RegExpr = mkUnion(cases.toList)

  def mkLoop(lower: Int, upper: Option[Int], elem: RegExpr): RegExpr =
    require(lower >= 0)
    require(upper.forall(_ >= lower))
    if lower == 0 && upper.contains(0) then RENull
    else if lower == 1 && upper.contains(1) then elem
    else RELoop(NatRange(lower, upper), elem)

  def mkLoop(range: NatRange, elem: RegExpr): RegExpr = mkLoop(range.lower, range.upper, elem)

  def mkNullable(r: RegExpr): RegExpr = if r.nullable then r else REUnion(RENull, r)

  def parse(regex: String): RegExpr = REParser.tryParse(regex) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r