package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker
import flat.checker.*
import flat.checker.Bound.{NegInf, PosInf}
import flat.checker.CNFOps.BiIndex
import flat.checker.CNFOps.BiIndex.{FromLeft, FromRight}
import flat.checker.ReLang.ReChars
import flat.checker.backend.core.*
import flat.checker.backend.core.ArithOp.{ADD, SUB}
import flat.checker.backend.core.CmpOp.*

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

class Inferer(types: Types, premises: List[Expr])(using issuer: Issuer):
  def infer(expr: Expr): Type =
    cache.get(expr) match
      case Some(t) => t
      case None =>
        val t = expr.accept(Visitor, ())
        cache(expr) = t
        t

  private val cache = mutable.Map.empty[Expr, Type]

  private object Visitor extends ExprVisitor[Unit, Type], LazyLogging:
    override def visitConst(node: Const, ctx: Unit): Type =
      node.value match
        case i: Int => IntervalType(i)
        case b: Boolean => TernaryType(b)
        case s: String => LangType(s)

    private def narrowIndexRangeByCharTest(str: Expr, r: ReLang, char: Char, isEQ: Boolean): Pred =
      val k = r.reverse.toCNF.takeWhile(_.alphabet.contains(char) ^ isEQ).map(_.length.lb.asInt).sum
      Pred(Sort.Int, i => LT(i, SUB(StrLen(str), Const(k))))

    override def visitVar(node: Var, ctx: Unit): Type =
      val x = node.name
      val t = types(x).toSort match
        case Sort.Int =>
          premises.collectFirst {
            case Cmp(op@(EQ | NE), StrAt(s, i), Const(s1: String)) if i == node && s1.length == 1 =>
              val r = infer(s).asInstanceOf[LangType].reLang
              narrowIndexRangeByCharTest(s, r, s1.head, op == EQ)
            case Not(Cmp(op@(EQ | NE), StrAt(s, i), Const(s1: String))) if i == node && s1.length == 1 =>
              val r = infer(s).asInstanceOf[LangType].reLang
              narrowIndexRangeByCharTest(s, r, s1.head, op == NE)
          } match
            case Some(pred) => HintType(pred)
            case None =>
              val (lb, ub) = RangeSolver.solve(node, premises)(using types)
              IntervalType(Interval(lb, ub))
        case Sort.Bool => boolType
        case Sort.String =>
          val r = Refiner.refine(node, types(x).asInstanceOf[LangType].reLang, premises)(using types)
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
      premises.collectFirst {
        case TypeTest(e@StrSlice(s, i, StrLen(s1)), LangType(r)) if s == node.str && s1 == s =>
          visitStrLen(StrLen(e), ctx) match
            case IntervalType(Interval(lb, ub)) =>
              HintType(Pred(Sort.Int, len => And(
                LE(ADD(lb.asInt, i), len),
                if ub.isFin then LE(len, ADD(ub.asInt, i)) else true
              )))
            case _ => assert(false)
      } match
        case Some(t) => t
        case None =>
          val t = node.str.accept(this, ctx)
          t match
            case LangType(r) => IntervalType(r.length)
            case _ => IntervalType(Interval(0, PosInf))

    override def visitStrAt(node: StrAt, ctx: Unit): Type =
      val slice = StrSlice(node.str, node.index, StrLen(node.str))
      val re = visitStrSlice(slice, ctx).asInstanceOf[LangType].reLang
      val cs0 = re.first

      // usual approach
      val t = node.str.accept(this, ctx)
      val r = t.asInstanceOf[LangType].reLang
      val cs1 =
        tryGetRelIndex(node.str, node.index) match
          case Some(op, Index(cnf, i)) =>
            val (from, until) = op match
              case EQ => (i, i + 1)
              case NE => (0, cnf.length)
              case LT => (0, i)
              case LE => (0, i + 1)
              case GT => (i + 1, cnf.length)
              case GE => (i, cnf.length)
            ReLang.fromCNF(cnf.slice(from, until)).alphabet
          case None =>
            inferBiIndex(node.str, node.index) match
              case FromLeft(k) => ReLangOps.charAt(r, k).getOrElse(CharSet.empty)
              case FromRight(k) => ReLangOps.charAt(r.reverse, k - 1).getOrElse(CharSet.empty)
              case (FromLeft(k1), FromLeft(k2)) => ReLangOps.charAt(r, k1, k2)
              case (FromRight(k1), FromRight(k2)) => ReLangOps.charAt(r, -k1, -k2)
              case (FromLeft(k1), FromRight(k2)) => ReLangOps.charAt(r, k1, -k2)
              case _ => assert(false)
      val cs = if cs0.subsetOf(cs1) then cs0 else cs1
      logger.debug(s"infer $node : ${ReChars(cs)}")
      LangType(ReChars(cs))

    private def tryGetRelIndex(str: Expr, index: Expr): Option[(CmpOp, Index)] =
      for
        (op, pos) <- premises.collectFirst {
          case Cmp(op, pos@StrFind(s, _), i) if s == str && i == index => Analyzer.reverseCmpOp(op) -> pos
          case Cmp(op, i, pos@StrFind(s, _)) if s == str && i == index => op -> pos
        }
        t = pos.accept(this, ())
        index <- t match
          case HintType(index@Index(_, _)) => Some(index)
          case _ => None
      yield (op, index)

    private def validateLPSolverResult(result: Expr): Boolean =
      SMTSolver.prove(result, premises)(using types) match
        case SolverResult.Valid => true
        case SolverResult.Invalid(_) =>
          logger.debug(s"LP solver unsound: cannot prove $result")
          false

    private def getRelPos(cnf: List[ReLang], intType: Type): Either[String, Int] = intType match
      case HintType(Index(cnf1, pos)) =>
        if cnf1 == cnf then Right(pos) else Left("cnf mismatch")
      case IntervalType(r) if r.isInt =>
        CNFOps.convertToRelative(cnf, r.asInt) match
          case Some(i) => Right(i)
          case _ => Left("index ambiguous")
      case IntervalType(r) => Left("index not constant")
      case _ => throw IllegalArgumentException()

    private def inferBiIndex(str: Expr, index: Expr): BiIndex | (BiIndex, BiIndex) =
      index match
        case Const(k: Int) if k >= 0 => BiIndex.FromLeft(k)
        case Const(k: Int) if k < 0 => BiIndex.FromRight(-k)
        case StrLen(s) if s == str => BiIndex.FromRight(0)
        case Arith(SUB, StrLen(s), Const(k: Int)) if s == str && k >= 0 => BiIndex.FromRight(k)
        case _ =>
          val (lb1, ub1) = RangeSolver.solve(index, premises)(using types)
          if lb1 == ub1 then return BiIndex.FromLeft(lb1.asInt)
          val (lb2, ub2) = RangeSolver.solve(SUB(index, StrLen(str)), premises)(using types)
          if lb2 == ub2 then return BiIndex.FromRight(-ub2.asInt)
          if lb1.isFin && ub1.isFin && lb2 == NegInf then
            return (BiIndex.FromLeft(lb1.asInt), BiIndex.FromLeft(ub1.asInt))
          if lb2.isFin && ub2.isFin && ub1 == PosInf then
            return (BiIndex.FromRight(-lb2.asInt), BiIndex.FromRight(-ub2.asInt))
          if lb1.isFin && ub1 == PosInf && lb2 == NegInf && ub2.isFin then
            return (BiIndex.FromLeft(lb1.asInt), BiIndex.FromRight(-ub2.asInt))
          throw new NotImplementedError(s"index of [$lb1, $ub1], [$lb2, $ub2]")

    private def inferCNFIndex(str: Expr, cnf: List[ReLang], index: Expr): Option[Int] =
      index.accept(this, ()) match
        case HintType(Index(cnf1, pos)) if cnf1 == cnf => Some(pos)
        case _ =>
          inferBiIndex(str, index) match
            case idx: BiIndex =>
              idx.toCNFIndex(cnf)
            case (idx1, idx2: BiIndex) =>
              (idx1.toCNFIndex(cnf), idx2.toCNFIndex(cnf)) match
                case (Some(k1), Some(k2)) if k1 == k2 => Some(k1)
                case (Some(k1), None) =>
                  idx2.next.toCNFIndex(cnf) match
                    case Some(k2) if k2 == k1 + 2 &&
                      cnf(k1 + 1) == ReLang.ReStar(cnf(k1)) && cnf(k1).length == Interval(1, 1) => // aa*
                      Some(k1)
                    case _ =>
                      logger.warn(s"cannot infer CNF index ($idx1, $idx2)")
                      None
                case _ =>
                  logger.warn(s"cannot infer CNF index ($idx1, $idx2)")
                  None

    private def tryRebaseSlice(node: StrSlice): Option[(ReLang, Int, Int)] =
      val (b1, k1) = node.fromIndex match
        case Arith(ADD, e, Const(k: Int)) if k >= 0 => (e, k)
        case e => (e, 0)
      val (b2, k2) = node.untilIndex match
        case StrLen(s) if s == node.str => (b1, -1)
        case Arith(ADD, e, Const(k: Int)) if k >= 0 => (e, k)
        case e => (e, 0)
      if b1 != b2 then return None
      for r <- premises.collectFirst {
        case TypeTest(e@StrSlice(s, i, StrLen(s1)), LangType(r)) if s == node.str && i == b1 && s1 == s =>
          Refiner.refine(e, r, premises)(using types)
      } yield (r, k1, k2)

    override def visitStrSlice(node: StrSlice, ctx: Unit): Type =
      tryRebaseSlice(node) match
        case Some((r1, k1, k2)) =>
          val cnf = r1.toCNF
          val result =
            for
              i <- CNFOps.convertToRelative(cnf, k1)
              j <- if k2 == -1 then Some(cnf.length) else CNFOps.convertToRelative(cnf, k2)
            yield
              val r = CNFOps.substring(cnf, i, j)
              logger.debug(s"infer $node : $r")
              LangType(r)
          result.getOrElse {
            LangType(ReLangOps.substring(r1, k1, k2))
          }
        case None =>
          val t = node.str.accept(this, ctx)
          val r = t.asInstanceOf[LangType].reLang
          val cnf = r.toCNF
          val r1 =
            for
              i <- inferCNFIndex(node.str, cnf, node.fromIndex)
              j <- inferCNFIndex(node.str, cnf, node.untilIndex)
            yield
              logger.debug(s"slice $i:$j from $cnf")
              val r = CNFOps.substring(cnf, i, j)
              logger.debug(s"infer $node : $r")
              LangType(r)
          r1.getOrElse(strType)

    private def extractCharAt(str: Expr, index: Expr): Option[CharSet] =
      premises.collectFirst {
        case Cmp(EQ, StrAt(s, i), Const(c: String)) if s == str && i == index && c.length == 1 =>
          CharSet.of(c.head)
        case Not(Cmp(NE, StrAt(s, i), Const(c: String))) if s == str && i == index && c.length == 1 =>
          CharSet.of(c.head)
        case Not(Cmp(EQ, StrAt(s, i), Const(c: String))) if s == str && i == index && c.length == 1 =>
          CharSet.complementOf(c.head)
        case Cmp(NE, StrAt(s, i), Const(c: String)) if s == str && i == index && c.length == 1 =>
          CharSet.complementOf(c.head)
      }

    override def visitStrFind(node: StrFind, ctx: Unit): Type =
      val t = node.str.accept(this, ctx)
      val t1 = node.target.accept(this, ctx)
      (t, t1) match
        case (LangType(r), LangType(rt)) =>
          if rt.isChar then
            val cnf = r.toCNF
            CNFOps.indexOf(cnf, rt.asChar) match
              case Right(pos) => HintType(Index(cnf, pos))
              case Left(reason) =>
                r.contains(rt.asChar) match
                  case Ternary.True => HintType(Pred(Sort.Int, x => And(LE(0, x), LT(x, StrLen(node.str)))))
                  case Ternary.False => IntervalType(-1)
                  case Ternary.Maybe => HintType(Pred(Sort.Int, x => And(LE(-1, x), LT(x, StrLen(node.str)))))
                  case _ => assert(false)
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