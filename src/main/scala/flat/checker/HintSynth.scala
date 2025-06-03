package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.core.*
import flat.regex.*
import flat.regex.RegExpr.*
import flat.util.tryAll

import scala.collection.mutable.ListBuffer

class HintSynth(using ctx: PrfCtx, config: Config) extends LazyLogging:

  import ArithOp.*
  import CmpOp.*
  import Direction.*

  private val hints = ListBuffer.empty[Expr]

  private val types = ctx.types

  private val indexSolver = new IndexSolver

  private val smtSolver = new SMTSolver

  def collectHints(seed: Expr): List[Expr] =
    val temps = seed.collect {
      case e@StrLen(_) => Len(e)
      case e@StrAt(_, _) => CharAt(e)
      case e@StrFind(_, _) => Find(e)
      case e@StrContains(_, _) => StrTest(e)
      case Cmp(EQ | NE, e, Const(s: String)) => StrEq(e, s)
      case e@Var(_) if ctx.exists {
        case Cmp(_, StrAt(_, ei), _) => ei == e
        case Cmp(_, ei, StrFind(_, _)) => ei == e
      } => IndexVar(e)
    }.distinct
    hints.clear()
    for temp <- temps do temp()
    hints.toList.distinct

  sealed trait HintTemp extends LazyLogging:
    def apply(): Unit

  final case class Len(expr: StrLen) extends HintTemp:
    def apply(): Unit =
      tryAll(
        () => for
          (ei, r) <- extractSuffixLang(expr.str)
          if smtSolver.canProve(mkAnd(GE(ei, 0), LT(ei, expr)))
        yield
          val r1 = RERefiner.refineByLen(r, (GT, 0))
          val NatRange(lb, ub) = REOps.length(r1)
          hints += GE(expr, ADD(ei, lb))
          for k <- ub do hints += LE(expr, ADD(ei, k))
      ).getOrElse {
        val r = inferLang(expr.str)
        val NatRange(lb, ub) = REOps.length(r)
        if ub.contains(lb) then
          hints += EQ(expr, lb)
        else
          if lb > 0 then hints += GE(expr, lb)
          for k <- ub do hints += LE(expr, k)
      }

  final case class IndexVar(expr: Var) extends HintTemp:
    def apply(): Unit =
      ctx.foreach {
        case Cmp(EQ, StrAt(es, ei@Var(_)), Const(s: String)) if s.length == 1 =>
          val r = inferLang(es)
          val c = s.head
          val ((r1, _), _) = REOps.splitAtIndexOf(r, c)
          if r1 != RENone then
            val k1 = REOps.length(r1).lower
            hints += GE(ei, k1)
            val ((_, r2), _) = REOps.splitAtIndexOf(r.reverse, c)
            val k2 = REOps.length(r2).lower
            hints += LT(ei, SUB(StrLen(es), k2))
        case Cmp(LE, e1, StrFind(es, Const(s: String))) if e1 == expr && s.length == 1 =>
          val r = inferLang(es)
          val c = s.head
          val ((r1, _), _) = REOps.splitAtIndexOf(r, c)
          val cs = REOps.alphabet(r) -- REOps.alphabet(r1) - c
          require(cs.polarity)
          for
            c <- cs.chars
            if ctx.exists { case StrContains(e1, Const(s1: String)) => e1 == es && s1 == c.toString }
          do hints += LT(e1, StrFind(es, Const(c.toString)))
        case _ =>
      }

  final case class CharAt(expr: StrAt) extends HintTemp:
    def apply(): Unit =
      inferLang(expr) match
        case REChar(cs) if cs.polarity =>
          val cases = for c <- cs.chars yield EQ(expr, c.toString)
          hints += mkOr(cases.toList)
        case _ => throw InternalError()

  final case class Find(expr: StrFind) extends HintTemp:
    def apply(): Unit =
      inferIndexOf(expr) match
        case Some(IndexAt(_, L, k)) => hints += EQ(expr, k)
        case Some(IndexAt(_, R, 0)) => hints += EQ(expr, -1)
        case Some(IndexAt(_, R, k)) => hints += EQ(expr, SUB(StrLen(expr.str), k))
        case Some((IndexAt(_, L, k1), IndexAt(_, L, k2))) =>
          hints += GE(expr, k1)
          hints += LE(expr, k2)
        case Some((IndexAt(_, L, k1), IndexAt(_, R, k2))) =>
          hints += GE(expr, k1)
          hints += LE(expr, SUB(StrLen(expr.str), k2))
        case _ =>

  final case class StrEq(str: Expr, target: String) extends HintTemp:
    def apply(): Unit =
      val r = inferLang(str)
      for ss <- REOps.tryEnumerate(r) do
        val cases = for s <- ss yield EQ(str, s)
        hints += mkOr(cases.toList)
      r match
        case REChar(cs) if !cs.polarity =>
          for c <- cs.chars do
            hints += NE(str, c.toString)
        case _ =>

  final case class StrTest(expr: StrContains) extends HintTemp:
    def apply(): Unit = expr match
      case StrContains(e, Const(s: String)) if s.length == 1 =>
        val r = inferLang(e)
        REOps.contains(r, s.charAt(0)) match
          case Some(true) => hints += StrContains(e, s)
          case Some(false) => hints += Not(StrContains(e, s))
          case None =>
      case _ =>

  def inferLang(expr: Expr): RegExpr = expr match
    case Const(s: String) => fromString(s)
    case Var(_) => ctx.getLang(expr)
    case StrConcat(es1, es2) =>
      val r1 = inferLang(es1)
      val r2 = inferLang(es2)
      mkConcat(r1, r2)
    case StrAt(es, ei) =>
      val attempt = for
        (from, r) <- extractSuffixLang(es)
        k <- Rewriter.tryGetConstDiff(ei, from)
        if k >= 0
      // r = langRefiner.refineSuffix(es, from, r0)
      yield REOps.firstSet(slice(r, IndexAt(null, L, k)))
      // normal attempt
      val r = inferLang(es)
      var cs = indexSolver.solve(ei, es) match
        case IndexAt(_, L, k) => REOps.charAt(r, k)
        case IndexAt(_, R, k) => REOps.charAt(r.reverse, k - 1)
        case i: Index => REOps.firstSet(slice(r, i))
        case IndexRange(i1: Index, i2: Index) =>
          logger.debug(s"slice $r from $i1 to $i2")
          REOps.alphabet(slice(r, i1, i2 + 1))
      if cs.isEmpty then cs = REOps.alphabet(r)
      ctx.foreach {
        case Cmp(NE, StrAt(e1, e2), Const(s: String)) if e1 == es && e2 == ei && s.length == 1 =>
          // TODO: is it possible to direct report inconsistency at this phase?
          if !cs.isSingleton then cs = cs - s.head
        case _ =>
      }
      // pick the more precise one
      attempt match
        case Some(cs1) if cs1.subsetOf(cs) =>
          logger.debug(s"  infer $expr ∈ $cs1 by suffix")
          REChar(cs1)
        case _ => REChar(cs)
    case StrSlice(es, ei, ej) =>
      logger.debug(s"infer $es[$ei:$ej]")
      tryAll(
        () =>
          if smtSolver.canProve(GE(ei, StrLen(es))) then Some(RENull)
          else None,
        () => for
          (from, r) <- extractSuffixLang(es)
          k <- Rewriter.tryGetConstDiff(ei, from)
          if k >= 0 && ej == StrLen(es)
        // r = langRefiner.refineSuffix(es, from, r0)
        yield
          val r1 = slice(r, IndexAt(null, L, k))
          r1,
        () => for
          (from, r) <- extractSuffixLang(es)
          ki <- Rewriter.tryGetConstDiff(ei, from)
          if ki >= 0
          kj <- Rewriter.tryGetConstDiff(ej, from)
          if kj >= 0
        yield
          val r1 = slice(r, IndexAt(null, L, ki), IndexAt(null, L, kj))
          logger.debug(s"$es[$ei:$ej] : $r1")
          r1,
      ).getOrElse {
        val r = inferLang(es)
        (indexSolver.solve(ei, es), indexSolver.solve(ej, es)) match
          case (i1: Index, i2: Index) =>
            val r1 = slice(r, i1, i2)
            logger.debug(s"$es[$ei:$ej] : $r1")
            r1
          case (IndexRange(i1, i2), IndexAt(_, R, 0)) =>
            val cs = REOps.alphabet(slice(r, i1, i2 + 1))
            val r2 = slice(r, i2 + 1, IndexAt(es, R, 0))
            val r1 = i1 match
              case IndexAt(_, L, _) => REChar(cs).+
              case _ => throw UnsupportedOperationException(i1.toString)
            logger.debug(s"$es[$ei:$ej] : $r1$r2")
            mkConcat(r1, r2)
          case (i1, i2) => throw UnsupportedOperationException(s"slice $i1 until $i2")
      }
    case _ => throw IllegalArgumentException(s"not a str-sorted expression: $expr")

  private def extractSuffixLang(str: Expr): Option[(Expr, RegExpr)] =
    ctx.collectFirst {
      case TypeTest(suffix@StrSlice(e1, ei, StrLen(e2)), LangType(_)) if e1 == str && e2 == str =>
        (ei, ctx.getLang(suffix))
    }

  private def slice(re: RegExpr, fromIndex: Index): RegExpr = slice(re, fromIndex, IndexAt(null, R, 0))

  private def slice(re: RegExpr, fromIndex: Index, untilIndex: Index): RegExpr =
    // TODO: check empty slice first?
    fromIndex match
      case IndexAt(_, d1, i1) =>
        val ra = d1 match
          case L => REOps.drop(re, i1)
          case R => REOps.takeRight(re, i1)
        untilIndex match
          case IndexAt(_, L, i2) =>
            require(d1 == L)
            REOps.take(ra, (i2 - i1) max 0)
          case IndexAt(_, R, i2) =>
            REOps.dropRight(ra, i2)
          case IndexOf(e, c, k) =>
            val r0 = inferLang(e)
            require(r0 == re || r0 == ra)
            val ((ral, rar), raNot) = REOps.splitAtIndexOf(ra, c)
            // assert(raNot == ReNone)
            val (r, _) = REOps.shift(ral, rar, k)
            val dropped =
              if r0 == re then d1 match
                case L => REOps.alphabet(REOps.take(re, i1))
                case R => REOps.alphabet(REOps.dropRight(re, i1))
              else CharSet.empty
            if dropped.contains(c) then mkNullable(r) else r
      case IndexOf(es1, c1, k1) =>
        val r1 = inferLang(es1)
        require(r1 == re)
        untilIndex match
          case IndexAt(_, d2, i2) =>
            val ra = d2 match
              case L => REOps.take(re, i2)
              case R => REOps.dropRight(re, i2)
            val ((ral, rar), raNot) = REOps.splitAtIndexOf(ra, c1)
            // assert(raNot == ReNone, raNot.toString)
            val (_, r) = REOps.shift(ral, rar, k1)
            val dropped = d2 match
              case L => REOps.alphabet(REOps.dropRight(re, i2))
              case R => REOps.alphabet(REOps.take(re, i2))
            if dropped.contains(c1) then mkNullable(r) else r
          case IndexOf(es2, c2, k2) =>
            val r2 = inferLang(es2)
            val ((rel, rer), reNot) = REOps.splitAtIndexOf(re, c1)
            // assert(reNot == RENone, reNot.toString)
            val (rDrop, ra) = REOps.shift(rel, rer, k1)
            require(r2 == ra || r2 == re)
            val ((ral, rar), raNot) = REOps.splitAtIndexOf(ra, c2)
            // assert(raNot == RENone, raNot.toString)
            val (r, _) = REOps.shift(ral, rar, k2)
            val dropped = if r2 == re then REOps.alphabet(rDrop) else CharSet.empty
            if dropped.contains(c2) then mkNullable(r) else r

  def inferIndexOf(expr: StrFind): Option[Index | (Index, Index)] = expr match
    case StrFind(es, Const(s: String)) if s.length == 1 =>
      val r = inferLang(es)
      val ((r1, r2), rNot) = REOps.splitAtIndexOf(r, s.head)
      if rNot != RENone && r1 == RENone && r2 == RENone then
        return Some(IndexAt(expr.str, R, 0))
      val k1 = REOps.length(r1).lower
      if REOps.length(r1).upper.contains(k1) && rNot == RENone then
        return Some(IndexAt(expr.str, L, k1))
      val k2 = REOps.length(r2).lower
      if REOps.length(r2).upper.contains(k2) && rNot == RENone then
        return Some(IndexAt(expr.str, R, k2))
      REOps.length(r1).upper match
        case Some(k3) => Some((IndexAt(expr.str, L, k1), IndexAt(expr.str, L, k3)))
        case None => Some((IndexAt(expr.str, L, k1), IndexAt(expr.str, R, 1)))
    case _ => None
