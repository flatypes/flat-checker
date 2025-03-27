package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker
import flat.checker.*
import flat.checker.Bound.PosInf
import flat.checker.ReLang.ReChars
import flat.checker.backend.core.*
import flat.checker.backend.core.ArithOp.SUB
import flat.checker.backend.core.CmpOp.{EQ, NE}

import scala.collection.mutable

extension (typ: Type)
  private def show: String = typ match
    case AnyType => "⊤"
    case NoType => "?"
    case IntervalType(i) => if i == Interval.full then "Int" else i.toString
    case TernaryType(b) => b.toString
    case LangType(r) => if r == ReLang.full then "String" else "/" + r.toString + "/"
    case TupleType(ts) => "(" + ts.map(_.show).mkString(", ") + ")"
    case ArrayType(t) => s"Array[${t.show}]"
    case FunType(ts, t) => "(" + ts.map(_.show).mkString(", ") + ") → " + t.show
    case HintType(h) => h.toType.show + s"(with hint: $h)"

class Inferer(types: Types, premises: List[Expr])(using issuer: Issuer) extends ExprVisitor[Unit, Type], LazyLogging:
  override def visitConst(node: Const, ctx: Unit): Type =
    node.value match
      case i: Int => IntervalType(i)
      case b: Boolean => TernaryType(b)
      case s: String => LangType(s)

  private val cache = mutable.Map.empty[String, Type]

  override def visitVar(node: Var, ctx: Unit): Type =
    val x = node.name
    if !cache.contains(x) then
      val t = types(x).toSort match
        case Sort.Int =>
          val (lb, ub) = LPSolver.solve(node, premises)
          IntervalType(Interval(lb, ub))
        case Sort.Bool => boolType
        case Sort.String =>
          val r = Refiner.refine(x, types(x).asInstanceOf[LangType].reLang, premises)
          LangType(r)
        case _ => throw UnsupportedOperationException()
      cache(x) = t

    cache(x)

  override def visitCmp(node: Cmp, ctx: Unit): Type =
    node match
      case Cmp(op@(EQ | NE), e, Const("")) =>
        val r = e.accept(this, ctx).asInstanceOf[LangType].reLang
        TernaryType(if op == EQ then r.nullable else !r.nullable)
      case _ => boolType

  // Int
  override def visitArith(node: Arith, ctx: Unit): Type =
    val t1 = node.left.accept(this, ctx)
    val t2 = node.right.accept(this, ctx)
    node.op match
      case ArithOp.ADD => add(t1, t2)
      case ArithOp.SUB =>
        (t1.ignoreHint, t2.ignoreHint) match
          case (IntervalType(i1), IntervalType(i2)) => IntervalType(i1 - i2)
          case _ => intType

  private def add(t1: Type, t2: Type): Type =
    (t1, t2) match
      case (HintType(index: Index), IntervalType(i)) if i.isInt =>
        CNFOps.shiftIndex(index, i.asInt) match
          case Right(newIndex) => return HintType(newIndex)
          case _ =>
      case (IntervalType(i), HintType(index: Index)) if i.isInt =>
        CNFOps.shiftIndex(index, i.asInt) match
          case Right(newIndex) => return HintType(newIndex)
          case _ =>
      case _ =>
    (t1.ignoreHint, t2.ignoreHint) match
      case (IntervalType(i1), IntervalType(i2)) => IntervalType(i1 + i2)
      case _ => intType

  // Char
  override def visitCharToCode(node: CharToCode, ctx: Unit): Type =
    val t = node.char.accept(this, ctx)
    t match
      case LangType(r) if r.isChar => IntervalType(r.asChar.toInt)
      case LangType(r) =>
        val k = r.length
        if !(k.isInt && k.asInt == 1) then
          issuer.report(TypeMayMismatch("String of length 1", s"String of length $k", node.char.loc))
        intType
      case _ => intType

  override def visitCharFromCode(node: CharFromCode, ctx: Unit): Type =
    val t = node.code.accept(this, ctx)
    t.ignoreHint match
      case IntervalType(i) if i.isInt => LangType(ReLang.fromChar(i.asInt.toChar))
      case _ => LangType(ReLang.allChar)

  // String
  override def visitStrConcat(node: StrConcat, ctx: Unit): Type =
    val t1 = node.left.accept(this, ctx)
    val t2 = node.right.accept(this, ctx)
    (t1, t2) match
      case (LangType(r1), LangType(r2)) => LangType(ReLangOps.concat(r1, r2))
      case _ => strType

  override def visitStrRev(node: StrRev, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    t match
      case LangType(r) => LangType(r.reverse)
      case _ => strType

  override def visitStrLen(node: StrLen, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    t match
      case LangType(r) => IntervalType(r.length)
      case _ => IntervalType(Interval(0, PosInf))

  override def visitStrAt(node: StrAt, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val r = t.asInstanceOf[LangType].reLang
    val (k1, k2) = getStrIndex(node.str, node.index)
    val cs =
      if k1 == k2 then ReLangOps.charAt(r, k1).getOrElse(CharSet.empty)
      else if k1 >= 0 then ReLangOps.charAt(r, k1, k2)
      else if k1 < 0 && k2 < 0 then ReLangOps.charAt(r.reverse, -k1 - 1, -k2 - 1)
      else r.alphabet
    LangType(ReChars(cs))
  //    val t1 = node.index.accept(this, ctx)
  //    (t.ignoreHint, t1.ignoreHint) match
  //      case (LangType(r), IntervalType(i)) =>
  //        if i.isInt then
  //          ReLangOps.charAt(r, i.asInt) match
  //            case Some(cs) => LangType(ReLang.ReChars(cs))
  //            case None =>
  //              issuer.report(IndexMayOutOfBounds(node.index.loc))
  //              LangType(ReLang.allChar)
  //        else
  //          LangType(ReChars(r.alphabet))
  //      case _ => LangType(ReLang.allChar)

  private def getStrIndex(str: Expr, index: Expr): (Int, Int) =
    index match
      case Const(k: Int) => (k, k)
      case Arith(SUB, StrLen(s), Const(k: Int)) if s == str => (-k, -k)
      case _ =>
        val (lb1, ub1) = LPSolver.solve(index, premises)
        if ub1.isFin && lb1 == ub1 then return (lb1.asInt, ub1.asInt)
        val (lb2, ub2) = LPSolver.solve(SUB(index, StrLen(str)), premises)
        if ub2.isFin && lb2 == ub2 then return (lb2.asInt, ub2.asInt)
        val k1 = if lb2.isFin && -lb2.asInt < 1000 then lb2.asInt else if lb1.isFin then lb1.asInt else 0
        val k2 = if ub1 < 1000 then ub1.asInt else if ub2.isFin then ub2.asInt else -1
        logger.debug(s"merge [$lb1, $ub1] with [$lb2, $ub2] as [$k1, $k2]")
        (k1, k2)

  private def getRelPos(cnf: List[ReLang], intType: Type): Either[String, Int] = intType match
    case HintType(Index(cnf1, pos)) =>
      if cnf1 == cnf then Right(pos) else Left("cnf mismatch")
    case IntervalType(r) if r.isInt => CNFOps.convertToRelative(cnf, r.asInt)
    case IntervalType(r) => Left("index not constant")
    case _ => throw IllegalArgumentException()

  override def visitStrSlice(node: StrSlice, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.fromIndex.accept(this, ctx)
    val t2 = node.untilIndex.accept(this, ctx)
    (t, t1, t2) match
      case (LangType(r), from, until) =>
        val cnf = r.toCNF
        getRelPos(cnf, from) match
          case Right(fromPos) =>
            getRelPos(cnf, until) match
              case Right(untilPos) => LangType(CNFOps.substring(cnf, fromPos, untilPos))
              case Left(reason) =>
                issuer.report(OverApprox(reason, node.loc))
                strType
          case Left(reason) =>
            issuer.report(OverApprox(reason, node.loc))
            strType
      case _ => strType

  override def visitStrFind(node: StrFind, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.target.accept(this, ctx)
    val t2 = node.fromIndex.accept(this, ctx)
    (t, t1, t2) match
      case (LangType(r), LangType(rt), from) =>
        if rt.isChar then
          val cnf = r.toCNF
          getRelPos(cnf, from) match
            case Left(reason) =>
              issuer.report(OverApprox(reason, node.fromIndex.loc))
              intType
            case Right(fromPos) =>
              CNFOps.indexOf(cnf, rt.asChar, fromPos) match
                case Right(pos) => HintType(Index(cnf, pos))
                case Left(reason) =>
                  issuer.report(OverApprox(reason, node.loc))
                  intType
        else
          issuer.report(OverApprox("pattern is not a constant char", node.loc))
          intType
      case _ => intType

  override def visitStrSplit(node: StrSplit, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.sep.accept(this, ctx)
    (t, t1) match
      case (LangType(r), LangType(rt)) =>
        if rt.isChar then
          val cnf = r.toCNF
          CNFOps.split(cnf, rt.asChar) match
            case Right(split) => HintType(split)
            case Left(reason) =>
              issuer.report(OverApprox(reason, node.loc))
              ArrayType(strType)
        else
          issuer.report(OverApprox("seperator is not a constant char", node.loc))
          ArrayType(strType)
      case _ => ArrayType(strType)

  override def visitStrStartsWith(node: StrStartsWith, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.prefix.accept(this, ctx)
    (t, t1) match
      case (LangType(r), LangType(rt)) =>
        if rt.isString then TernaryType(ReLangOps.startsWith(r, rt.asString))
        else
          issuer.report(OverApprox("prefix is not a constant string", node.loc))
          boolType
      case _ => boolType

  override def visitStrEndsWith(node: StrEndsWith, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.suffix.accept(this, ctx)
    (t, t1) match
      case (LangType(r), LangType(rt)) =>
        if rt.isString then TernaryType(ReLangOps.endsWith(r, rt.asString))
        else
          issuer.report(OverApprox("prefix is not a constant string", node.loc))
          boolType
      case _ => boolType

  override def visitStrContains(node: StrContains, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    val t1 = node.infix.accept(this, ctx)
    (t, t1) match
      case (LangType(r), LangType(rt)) =>
        if rt.isChar then TernaryType(r.contains(rt.asChar))
        else
          issuer.report(OverApprox("prefix is not a constant char", node.loc))
          boolType
      case _ => boolType

  override def visitStrToInt(node: StrToInt, ctx: Unit): Type =
    val t = node.str.accept(this, ctx)
    t match
      case LangType(r) if r.isNumber && r.isString => IntervalType(r.asString.toInt)
      case LangType(r) if !r.isNumber =>
        issuer.report(TypeMayMismatch("String of digits", t.show, node.str.loc))
        intType
      case _ => intType

  override def visitStrFromInt(node: StrFromInt, ctx: Unit): Type =
    val t = node.int.accept(this, ctx)
    t.ignoreHint match
      case IntervalType(i) if i.isInt => LangType(i.asInt.toString)
      case _ => LangType(ReLang.number)

  override def visitArraySelect(node: ArraySelect, ctx: Unit): Type =
    val t1 = node.array.accept(this, ctx)
    val t2 = node.index.accept(this, ctx)
    (t1, t2.ignoreHint) match
      case (HintType(split: Split), IntervalType(i)) if i.isInt =>
        return split.get(i.asInt) match
          case Some(r) => LangType(r)
          case None => strType
      case _ =>
    t1.ignoreHint match
      case ArrayType(elemType) => elemType
      case _ => NoType