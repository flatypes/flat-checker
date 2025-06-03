package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.SolverResult.Valid
import flat.checker.core.*
import flat.util.tryAll

import scala.Function.unlift

class IndexSolver(using ctx: PrfCtx, config: Config) extends LazyLogging:

  import ArithOp.*
  import CmpOp.*
  import Direction.*

  private val types = ctx.types
  private val smtSolver = new SMTSolver

  def solve(expr: Expr, str: Expr): AbsIndex =
    evalIndex(expr, str) match
      case Some(i) => i
      case None => expr match
        case x@Var(_) => inferIndexVar(x, str)
        case Arith(ADD, x@Var(_), Const(k: Int)) => inferIndexVar(x, str) + k
        case Arith(SUB, x@Var(_), Const(k: Int)) => inferIndexVar(x, str) - k
        case _ => throw UnsupportedOperationException()

  private def evalIndex(expr: Expr, str: Expr): Option[Index] = expr match
    case Const(n: Int) if n >= 0 => Some(IndexAt(str, L, n))
    case Const(n: Int) if n < 0 => Some(IndexAt(str, R, -n))
    case StrLen(e) if e == str => Some(IndexAt(str, R, 0))
    case Arith(SUB, StrLen(e), Const(n: Int)) if e == str && n >= 0 => Some(IndexAt(str, R, n))
    case StrFind(e, Const(s: String)) if e == str && s.length == 1 => Some(IndexOf(str, s.head))
    case _ => None

  private def inferIndexVar(variable: Var, str: Expr): AbsIndex =
    val constraints = ctx.collect(unlift {
      case cond: Cmp => Rewriter.push(variable, cond)
      case _ => None
    })
    logger.debug(s"infer $variable: constraints: $constraints")

    val eqs = constraints.flatMap {
      case (EQ, e) => evalIndex(e, str)
      case _ => None
    }
    if eqs.nonEmpty then
      require(eqs.length == 1, s"multiple EQ: $eqs")
      return eqs.head
    val lbs = constraints.flatMap {
      case (GE, e) => evalIndex(e, str)
      case (GT, e) => evalIndex(e, str).map(_ + 1)
      case _ => None
    }
    val ubs = constraints.flatMap {
      case (LE, e) => evalIndex(e, str)
      case (LT, e) => evalIndex(e, str).map(_ - 1)
      case _ => None
    }
    val common = lbs.toSet & ubs.toSet
    if common.nonEmpty then
      require(common.size == 1, s"multiple EQ: $common")
      return common.head

    logger.debug(s"lbs: $lbs  ubs: $ubs")
    val lb = tryAll(
      () => lbs.find(_.isInstanceOf[IndexOf]),
      () => lbs.filter {
        case IndexAt(_, L, _) => true
        case _ => false
      }.maxByOption(_.asInstanceOf[IndexAt].index),
      () => lbs.minByOption(_.asInstanceOf[IndexAt].index)
    ).getOrElse(IndexAt(str, L, 0))
    val ub = tryAll(
      () => ubs.find(_.isInstanceOf[IndexOf]),
      () => ubs.filter {
        case IndexAt(_, L, _) => true
        case _ => false
      }.minByOption(_.asInstanceOf[IndexAt].index),
      () => ubs.maxByOption(_.asInstanceOf[IndexAt].index)
    ).getOrElse(IndexAt(str, R, 1))
    logger.debug(s"lb: $lb  ub: $ub")
    // check soundness
    val lemma = mkAnd(GE(variable, lb.toExpr), LE(variable, ub.toExpr))
    assert(smtSolver.prove(lemma) == Valid, "index solver unsound")
    if lb == ub then lb else IndexRange(lb, ub)
