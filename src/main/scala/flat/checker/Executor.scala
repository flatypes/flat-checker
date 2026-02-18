package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.{Config, Issuer}

import scala.collection.mutable

final case class EvalCtx private(values: Map[String, Expr], versions: Map[String, Int]):
  def apply(x: String): Expr = values(x)

  def updated(x: String, v: Expr): EvalCtx = copy(values = values + (x -> v))

  def havoc(x: String): EvalCtx =
    require(!x.contains(':'))
    val n = versions(x) + 1
    copy(values = values + (x -> Var(s"$x:$n")), versions = versions + (x -> n))

object EvalCtx:
  def apply(programVars: Set[String]): EvalCtx =
    EvalCtx(values = Map.from(for x <- programVars yield x -> Var(s"$x:0")),
      versions = Map.from(for x <- programVars yield x -> 0))

/** Symbolic Executor */
class Executor(funDef: FunDef)(using config: Config, issuer: Issuer) extends LazyLogging:
  def exec(): Unit =
    val ctx = EvalCtx(funDef.programVars)
    for ep <- funDef.requires do
      checkSide(ep, ctx)
      prover.assume(eval(ep, ctx))
    exec(funDef.body, ctx)

  private val loopStack = mutable.Stack.empty[While]

  private def exec(body: List[Stmt], ctx: EvalCtx): Unit = body match
    case Nil => if loopStack.nonEmpty then checkInv(ctx) else checkPost(ctx)
    case Assign(x, e) :: rest =>
      checkSide(e, ctx)
      exec(rest, ctx.updated(x, eval(e, ctx)))
    case Assert(e) :: rest =>
      checkSide(e, ctx)
      check(e, "assert", ctx)
      if eval(e, ctx) != Const(false) then
        exec(rest, ctx)
    case Assume(e) :: rest =>
      checkSide(e, ctx)
      prover.assume(eval(e, ctx))
      exec(rest, ctx)
    case ShowType(e) :: rest =>
      exec(rest, ctx)
    case Hint(_) :: rest =>
      exec(rest, ctx)

    case IfStmt(e, thenBody, elseBody) :: rest =>
      checkSide(e, ctx)
      // then branch
      prover.push()
      prover.assume(eval(e, ctx))
      exec(thenBody ++ rest, ctx)
      prover.pop()
      // else branch
      prover.push()
      prover.assume(eval(Not(e), ctx))
      exec(elseBody ++ rest, ctx)
      prover.pop()

    case (loop@While(e, loopBody)) :: rest =>
      for ei <- loop.invariants do
        checkSide(ei, ctx)
        check(ei, "inv on entry", ctx)
      // havoc loop variables
      val ctx1 = Analyzer.collectModifiedVars(loopBody).foldLeft(ctx)(_.havoc(_))
      // assume loop invariants
      for ei <- loop.invariants do
        checkSide(ei, ctx1)
        prover.assume(eval(ei, ctx1))
      // execute loop body
      prover.push()
      checkSide(e, ctx1)
      prover.assume(eval(e, ctx1))
      loopStack.push(loop)
      exec(loopBody, ctx1)
      loopStack.pop()
      prover.pop()
      // execute rest
      val es = Analyzer.collectBreakConds(loopBody)
      val exit = mkOr(Not(e) :: es)
      prover.assume(eval(exit, ctx1))
      exec(rest, ctx1)

    case Break() :: _ => checkInv(ctx)
    case Continue() :: _ => checkInv(ctx)
    case Return() :: _ => checkPost(ctx)

  private def checkInv(ctx: EvalCtx): Unit =
    val loop = loopStack.top
    for ei <- loop.invariants do
      checkSide(ei, ctx)
      check(ei, "inv", ctx)

  private def checkPost(ctx: EvalCtx): Unit =
    val ctx1 = EvalCtx(funDef.params.map(_.name).toSet)
    val ctx2 = ctx1.updated("return", ctx("return"))
    for ep <- funDef.ensures do
      checkSide(ep, ctx2)
      check(ep, "post", ctx2)

  private def checkSide(expr: Expr, ctx: EvalCtx): Unit =
    expr.traverse:
      case CharAt(e, ei) =>
        check(And(LE(Const(0), ei), LT(ei, StringLength(e))), "index", ctx)
      case Substring(es, ei, ej) =>
        if ei != Const(0) then
          check(LE(Const(0), ei), "index", ctx)
        if ej != StringLength(es) then
          check(LE(Const(0), ej), "index", ctx)

  private val verifier = new Verifier
  private val prover = new verifier.Prover(using VarCtx(funDef.lCtx), funDef.sortingContext)
  private var nextGoal = 1

  private def check(cond: Expr, kind: String, ctx: EvalCtx): Unit =
    logger.info("")
    logger.info(s"Goal {} ({}): {}", nextGoal, kind, verifier.ppGoal(prover.getCtx.premises, cond))
    prover.prove(eval(cond, ctx))
    nextGoal += 1

  private def eval(expr: Expr, ctx: EvalCtx): Expr =
    expr.transform:
      case Var(x) => ctx(x)
