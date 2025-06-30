package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.core.*
import flat.regex.{CharSet, REOps, RERefiner, RegExpr}

import scala.collection.mutable

object Refiner extends LazyLogging:

  import CmpOp.*
  import Direction.*
  import RegExpr.*

  private val m = mutable.HashMap.empty[Expr, RegExpr]

  def refine(ctx: PrfCtx)(using config: Config): PrfCtx =
    m.clear()
    ctx.foreach { cond => for e -> r <- refine(cond)(using ctx) do m(e) = r }
    if m.nonEmpty then
      for e -> r <- m do logger.debug(s"refine $e : $r")
    ctx ++ List.from(for e -> r <- m yield TypeTest(e, LangType(r)))

  private def refine(assumption: Expr)(using ctx: PrfCtx, config: Config): Option[(Expr, RegExpr)] =
    assumption match
      case Cmp(op, StrLen(Var(x)), Const(n: Int)) =>
        for r <- getNonEmptyLang(x) yield (Var(x) -> RERefiner.refineByLen(r, (op, n)))
      case Cmp(op@(EQ | NE), StrAt(Var(x), ei), Const(s: String)) =>
        val c = ensureChar(s)
        for
          r <- getNonEmptyLang(x)
          reft <- refineByCharAt(Var(x), r, ei, op, c)
        yield reft
      case Cmp(op, StrFind(Var(x), Const(s: String)), Const(n: Int)) if s.length == 1 =>
        val c = ensureChar(s)
        for r <- getNonEmptyLang(x) yield Var(x) -> RERefiner.refineByIndexOf(r, c, (op, n))
      case Cmp(EQ, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield Var(x) -> refineByEqual(r, s)
      case Cmp(NE, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield Var(x) -> refineByNotEqual(r, s)
      case _ => None

  private def getNonEmptyLang(varName: String)(using ctx: PrfCtx): Option[RegExpr] =
    val r = m.get(varName) match
      case Some(r) => r
      case None => ctx.getLang(Var(varName))
    if r == RENone then None else Some(r)

  private def ensureChar(s: String): Char =
    require(s.length == 1)
    s.head

  private def refineByCharAt(str: Expr, lang: RegExpr, idx: Expr, op: CmpOp, c: Char)
                            (using ctx: PrfCtx, config: Config): Option[(Expr, RegExpr)] =
    val solution = for
      (base, r) <- ctx.collectFirst {
        case TypeTest(StrSlice(e1, i, StrLen(e2)), LangType(r)) if e1 == str && e2 == str => (i, r)
      }
      k <- Rewriter.tryGetConstDiff(idx, base)
      if k >= 0
    yield StrSlice(str, base, StrLen(str)) -> RERefiner.refineByCharAt(r, k, (op, c))
    solution.orElse {
      val indexSolver = new IndexSolver
      indexSolver.solve(idx, str) match
        case IndexAt(_, L, k) =>
          Some(str -> RERefiner.refineByCharAt(lang, k, (op, c)))
        case IndexAt(_, R, k) if k > 0 =>
          Some(str -> RERefiner.refineByCharAt(lang.reverse, k - 1, (op, c)).reverse)
        case IndexRange(_, _) if op == NE =>
          Some(str -> refineBySomeCharNotEqual(lang, c))
        case _ => None
    }

  private def refineBySomeCharNotEqual(re: RegExpr, c: Char): RegExpr = re match
    case REUnion(r1, r2) => mkUnion(List(r1, r2).filter(r => (REOps.alphabet(r) & CharSet(false, Set(c))).nonEmpty))
    case _ => re

  private def refineByIsNull(re: RegExpr): RegExpr =
    if re.nullable then RENull else RENone

  private def refineByEqual(re: RegExpr, s: String): RegExpr =
    if REOps.canParse(re, s) then RegExpr.fromString(s) else RENone

  private def refineByNotEqual(re: RegExpr, s: String): RegExpr = s.length match
    case 0 => RERefiner.refineByLen(re, (GT, 0))
    case 1 => RERefiner.refineByCharAt(re, 0, (NE, s.head))
    case _ if REOps.firstSet(re) == CharSet.of(s.head) =>
      mkConcat(fromChar(s.head), refineByNotEqual(REOps.drop(re, 1), s.tail))
    case _ => REOps.tryEnumerate(re) match
      case Some(words) => mkUnion(List.from(words - s).map(fromString))
      case None => re