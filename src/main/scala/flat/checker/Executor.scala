package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.ast.*
import flat.{Config, Issuer}

import scala.collection.mutable

/** Symbolic Executor */
class Executor(funDef: FunDef)(using config: Config, issuer: Issuer) extends LazyLogging:
  private val programVars = Map.from(for Decl(x, s) <- funDef.params ++ funDef.locals yield x -> s)
  private val store: mutable.Stack[mutable.Map[String, Expr]] =
    mutable.Stack(mutable.Map.from(for (x, s) <- programVars yield x -> Var(x)(s)))
  private val versions = mutable.Map.from(for x <- programVars.keys yield x -> 0)

  enum Edge:
    case IfTrue(stmt: IfStmt)
    case IfFalse(stmt: IfStmt)
    case WhileEnter(stmt: While)

  private val path = mutable.Stack.empty[Edge]

  private val verifier = new Verifier
  private val prover = new verifier.Prover(using VarCtx(programVars))
  private var nextGoal = 1

  def exec(): Unit =
    for e <- funDef.requires do
      prover.assume(eval(e))
    exec(funDef.body)

  private def exec(body: List[Stmt]): Unit = body match
    case Nil => if inLoop then checkInvariants() else checkPost()
    case Assign(x, e) :: rest =>
      store.top(x) = eval(e)
      exec(rest)
    case Assert(e) :: rest =>
      checkSide(e)
      logger.info("")
      logger.info(s"Goal {} (assert): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, e))
      prover.prove(eval(e))
      nextGoal += 1
      if eval(e) != Const(false) then
        exec(rest)
    case Assume(e) :: rest =>
      prover.assume(eval(e))
      exec(rest)
    case ShowType(e) :: rest =>
      exec(rest)
    case Hint(_) :: rest =>
      exec(rest)

    case (ifStmt@IfStmt(e, thenBody, elseBody)) :: rest =>
      checkSide(e)
      // true
      path.push(Edge.IfTrue(ifStmt))
      store.push(mutable.Map.empty)
      prover.locally:
        prover.assume(eval(e))
        exec(thenBody ++ rest)
      path.pop()
      store.pop()
      // false
      path.push(Edge.IfFalse(ifStmt))
      store.push(mutable.Map.empty)
      prover.locally:
        prover.assume(eval(Not(e)))
        exec(elseBody ++ rest)
      path.pop()
      store.pop()

    case (loop@While(e, loopBody)) :: rest =>
      checkSide(e)
      for ei <- loop.invariants do
        logger.info("")
        logger.info(s"Goal {} (inv pre): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, ei))
        prover.prove(eval(ei))
        nextGoal += 1
      // havoc modified variables
      for x <- Analyzer.collectModifiedVars(loopBody) do
        versions(x) += 1
        store.top(x) = Var(s"$x@${versions(x)}")(programVars(x))
      for ei <- loop.invariants do
        prover.assume(eval(ei))
      // execute loop body
      path.push(Edge.WhileEnter(loop))
      store.push(mutable.Map.empty)
      prover.locally:
        prover.assume(eval(e))
        exec(loopBody)
      path.pop()
      store.pop()
      // execute rest
      val es = Analyzer.collectBreakConds(loopBody)
      val exit = mkOr(Not(e) :: es)
      prover.assume(eval(exit))
      exec(rest)

    case Break() :: _ => checkInvariants()
    case Continue() :: _ => checkInvariants()
    case Return() :: _ => checkPost()

  private def inLoop: Boolean =
    path.exists:
      case Edge.WhileEnter(_) => true
      case _ => false

  private def checkPost(): Unit =
    for Decl(x, _) <- funDef.params do
      store.top(x) = Var(x)(programVars(x))
    for e <- funDef.ensures do
      logger.info("")
      logger.info(s"Goal {} (post): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, eval(e)))
      prover.prove(eval(e))

  private def checkInvariants(): Unit =
    require(inLoop)
    val loop = path.collectFirst { case Edge.WhileEnter(loop) => loop }.get
    for e <- loop.invariants do
      logger.info("")
      logger.info(s"Goal {} (inv post): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, eval(e)))
      prover.prove(eval(e))

  private def checkSide(expr: Expr): Unit =
    expr.traverse:
      case CharAt(e, ei) =>
        logger.info("")
        logger.info(s"Goal {} (side): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, expr))
        prover.prove(eval(And(Cmp(CmpOp.LE, Const(0), ei), Cmp(CmpOp.LT, ei, StringLength(e)))))
        nextGoal += 1
      case Substring(es, ei, ej) =>
        if ei != Const(0) then
          logger.info("")
          logger.info(s"Goal {} (side): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, expr))
          prover.prove(eval(Cmp(CmpOp.LE, Const(0), ei)))
          nextGoal += 1
        if ej != StringLength(es) then
          logger.info("")
          logger.info(s"Goal {} (side): {}", nextGoal, verifier.ppGoal(prover.getCtx.premises, expr))
          prover.prove(eval(Cmp(CmpOp.LE, Const(0), ej)))
          nextGoal += 1

  private def eval(expr: Expr): Expr =
    expr.transform:
      case Var(x) => store.collectFirst { case m if m.contains(x) => m(x) }.get
