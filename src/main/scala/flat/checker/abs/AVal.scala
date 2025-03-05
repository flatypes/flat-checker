package flat.checker.abs

import flat.checker.Bound.PosInf
import flat.checker.ast.*

case object ATop

final case class Index(cnf: List[ReLang], pos: Int):
  override def toString: String = s"index($pos)"

type AInt = Range | Index

extension (i: AInt)
  def asRange: Range = i match
    case range: Range => range
    case Index(cnf, pos) => ReLang.fromCNF(cnf.take(pos)).length

given AbsDom[AInt]:
  def top: AInt = Range.full

  def bot: AInt = Range.empty

  def subElement(i1: AInt, i2: AInt): Boolean =
    if i1 == i2 then true else i1.asRange :<: i2.asRange

  def join(i1: AInt, i2: AInt): AInt =
    if i1 == i2 then i1 else i1.asRange | i2.asRange

  def widen(i1: AInt, i2: AInt): AInt =
    if i1 == i2 then i1 else i1.asRange ∇ i2.asRange

type AString = ReLang

final case class AFun(argUbs: Seq[AVal], returnUb: AVal)

type AVal = ATop.type | AInt | ABool | AString | AList | AFun

object AVal:
  def fromType(typ: Type): AVal = computeUb(typ)

  private def computeUb(typ: Type): AVal =
    typ match
      case AnyType => ATop
      case NoType => throw IllegalStateException("ill-typed")
      case IntType => Range.full
      case BoolType => ABool.Top
      case StringType => ReLang.full
      case LiteralType(n: Int) => Range.fromInt(n)
      case LiteralType(b: Boolean) => ABool.fromBoolean(b)
      case LiteralType(s: String) => ReLang.fromString(s)
      case ArrayType(t) => AList.top(computeUb(t))
      case FunType(ts, t) =>
        val us = for t <- ts yield computeUb(t)
        val u = computeUb(t)
        AFun(us, u)
      case RangeType(lb, ub) => Range(lb, ub)
      case LangType(e) => reExprToLang(e)

  private def reExprToLang(expr: ReExpr): ReLang =
    expr match
      case ReEmpty => ReLang.ReEmpty
      case ReAllChar => ReLang.allChar
      case ReChar(c) => ReLang.fromChar(c)
      case ReRange(c1, c2) => ReLang.mkRange(c1, c2)
      case ReConcat(e1, e2) => ReLang.ReConcat(reExprToLang(e1), reExprToLang(e2))
      case ReUnion(e1, e2) => ReLang.ReUnion(reExprToLang(e1), reExprToLang(e2))
      case ReRepeat(lb, ub, e) =>
        val r = reExprToLang(e)
        val rep =
          if ub == PosInf then ReLang.ReStar(r)
          else ReLang.mkUnion((for i <- 0 to ub.asInt yield r ^ i) *)
        if lb.asInt == 0 then rep else ReLang.ReConcat(r ^ lb.asInt, rep)
      case ReComp(e) => ReLang.ReChars(!reExprToCharSet(e))

  private def reExprToCharSet(expr: ReExpr): CharSet =
    expr match
      case ReAllChar => CharSet.complementFrom()
      case ReChar(c) => CharSet.from(c)
      case ReRange(c1, c2) => CharSet.from(c1 to c2)
      case ReUnion(e1, e2) => reExprToCharSet(e1) | reExprToCharSet(e2)
      case ReComp(e) => !reExprToCharSet(e)
      case _ => throw UnsupportedOperationException(s"convert to char set: $expr")

given AbsDom[AVal]:
  def top: AVal = ATop

  def bot: AVal = throw NotImplementedError()

  def subElement(x: AVal, y: AVal): Boolean =
    (x, y) match
      case (_, ATop) => true
      case (i1: AInt, i2: AInt) => i1 :<: i2
      case (b1: ABool, b2: ABool) => b1 :<: b2
      case (s1: AString, s2: AString) => s1 :<: s2
      case _ => false

  def join(x: AVal, y: AVal): AVal =
    (x, y) match
      case (i1: AInt, i2: AInt) => i1 | i2
      case (b1: ABool, b2: ABool) => b1 | b2
      case (s1: AString, s2: AString) => s1 | s2
      case _ => ATop

  def widen(x: AVal, y: AVal): AVal =
    (x, y) match
      case (i1: AInt, i2: AInt) => i1 ∇ i2
      case (b1: ABool, b2: ABool) => b1 ∇ b2
      case (s1: AString, s2: AString) => s1 ∇ s2
      case _ => ATop

extension (v: AVal)
  def show: String = v match
    case ATop => "⊤"
    case range: Range => if range == Range.full then "Int" else range.toString
    case Index(_, k) => s"index($k)"
    case b: ABool => b.toString
    case r: ReLang => if r == ReLang.full then "String" else "/" + r.toString + "/"
    case l: AList => "list"
    case AFun(us, u) => "(" + us.map(_.show).mkString(", ") + ") → " + u.show
