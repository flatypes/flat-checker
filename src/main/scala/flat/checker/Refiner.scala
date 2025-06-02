package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.RegExpr.RENone
import flat.regex.{RERefiner, RegExpr}

import scala.collection.mutable

object Refiner extends LazyLogging:

  import CmpOp.*
  import Direction.*

  private val m = mutable.HashMap.empty[String, RegExpr]

  def refine(ctx: PrfCtx): PrfCtx =
    m.clear()
    ctx.foreach { e => for x -> r <- refine(e)(using ctx) do m(x) = r }
    if m.nonEmpty then
      for x -> r <- m do logger.debug(s"refine $x : $r")
    ctx ++ List.from(for x -> r <- m yield TypeTest(Var(x), LangType(r)))

  private def refine(assumption: Expr)(using ctx: PrfCtx): Option[(String, RegExpr)] =
    val refiner = new SuffixRefiner
    assumption match
      case Cmp(op, StrLen(Var(x)), Const(n: Int)) =>
        for r <- getNonEmptyLang(x) yield (x -> RERefiner.refineByLen(r, (op, n)))
      case Cmp(op@(EQ | NE), StrAt(Var(x), ei), Const(s: String)) =>
        val c = ensureChar(s)
        for
          r <- getNonEmptyLang(x)
          indexSolver = new IndexSolver
          r1 <- indexSolver.solve(ei, Var(x)) match
            case IndexAt(_, L, k) => Some(x -> RERefiner.refineByCharAt(r, k, (op, c)))
            case IndexAt(_, R, k) if k > 0 => Some(x -> RERefiner.refineByCharAt(r.reverse, k - 1, (op, c)).reverse)
            case IndexRange(_, _) if op == NE =>
              val c = ensureChar(s)
              val r = ctx.getLang(x)
              Some(x -> refiner.refineBySomeCharNotEqual(r, c))
            case _ => None
        yield r1
      case Cmp(op, StrFind(Var(x), Const(s: String)), Const(n: Int)) if s.length == 1 =>
        val c = ensureChar(s)
        for r <- getNonEmptyLang(x) yield x -> RERefiner.refineByIndexOf(r, c, (op, n))
      case Cmp(EQ, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield x -> refiner.refineByEqual(r, s)
      case Cmp(NE, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield x -> refiner.refineByNotEqual(r, s)
      case _ => None

  private def getNonEmptyLang(varName: String)(using ctx: PrfCtx): Option[RegExpr] =
    val r = m.get(varName) match
      case Some(r) => r
      case None => ctx.getLang(varName)
    if r == RENone then None else Some(r)

  private def ensureChar(s: String): Char =
    require(s.length == 1)
    s.head