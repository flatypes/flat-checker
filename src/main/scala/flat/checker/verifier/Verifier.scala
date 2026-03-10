package flat.checker.verifier

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ExprSimplifier.simplify
import flat.checker.ast.Printer.ppExpr
import flat.checker.{Analyzer, VarCtx, Verifier}
import flat.regex.CharSet
import flat.{Config, checker}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Verifier(using config: Config) extends LazyLogging:
  private type Cont = State => Unit

  private case class Handlers(onReturn: Cont,
                              onBreak: Cont = _ => throw RuntimeException("break outside of a loop"),
                              onContinue: Cont = _ => throw RuntimeException("continue outside of a loop"))

  private type Reporter = ListBuffer[ReportItem]

  def verify(module: Module): Report =
    // Pass 1: collect signatures
    val signatures = mutable.Map.empty[String, FunSig]
    module.body.foreach:
      case FunDef(f, params, returnParams, requires, ensures, _, _) =>
        signatures(f) = FunSig(params, returnParams, requires, ensures)
    // Pass 2: verify function bodies
    val reporter = ListBuffer.empty[ReportItem]
    module.body.foreach:
      case funDef: FunDef =>
        Analyzer.guessInvariants(funDef.body)
        exec(funDef, signatures.toMap)(using reporter)
    for item <- reporter do
      logger.info(s"${item.task}: ${item.result}")
    Report(reporter.toList)

  private def exec(funDef: FunDef, signatures: Map[String, FunSig])(using reporter: Reporter): Unit =
    val localCtx = Map.from(for Decl(x, s) <- funDef.params ++ funDef.returnParams ++ funDef.locals yield x -> s)
    val symbolicCtx = Map.from(for Decl(x, s) <- funDef.params yield x -> s) ++
      (for Decl(x, s) <- funDef.returnParams ++ funDef.locals yield s"$x(0)" -> s) // mutable variables have versions
    val symbolicValues = Map.from(for Decl(x, _) <- funDef.params yield x -> Var(x)) ++
      (for Decl(x, _) <- funDef.returnParams ++ funDef.locals yield x -> Var(s"$x(0)"))
    val freshCounts = Map.from(for Decl(x, s) <- funDef.returnParams ++ funDef.locals yield x -> 1)
    val state = State(signatures, funDef.name, localCtx, symbolicCtx, symbolicValues, freshCounts)
    if check(funDef.requires, state) then
      val postChecker: Cont = (state: State) => prove(funDef.ensures.map(PostTask(_)), state)
      exec(funDef.body, addMany(funDef.requires, state, "pre"), postChecker)(
        using Handlers(onReturn = postChecker), reporter)

  private def exec(body: List[Stmt], state: State, cont: Cont)
                  (using handlers: Handlers, reporter: Reporter): Unit = body match
    case Nil => cont(state)
    case stmt :: rest => exec(stmt, state, exec(rest, _, cont))

  private def exec(stmt: Stmt, state: State, cont: Cont)
                  (using handlers: Handlers, reporter: Reporter): Unit = stmt match
    case Assign(x, e) =>
      if check(e, state) then
        cont(update(x, e, state))
    case Havoc(xs) =>
      cont(havoc(xs, state))
    case Assume(e) =>
      if check(e, state) then
        cont(add(e, state))
    case Assert(e) =>
      prove(AssertTask(e), state)
      cont(state)
    case ShowType(_) => throw UnsupportedOperationException("show type")
    case Return() => handlers.onReturn(state)
    case ifStmt@IfStmt(e, thenBody, elseBody) =>
      if check(e, state) then
        val (thenState, elseState) = addIf(e, state, "if", "else")
        exec(thenBody, thenState, cont)
        exec(elseBody, elseState, cont)
    case loop@While(e, loopBody) =>
      prove(loop.invariants.toList.map(InvTask(_, onEntry = true)), state)
      val havocState = havoc(Analyzer.collectModifiedVars(loopBody).toList, state)
      if check(e :: loop.invariants.toList, havocState) then
        val invState = addMany(loop.invariants.toList, havocState, "invariant")
        val (entryState, exitState) = addIf(e, invState, "while", "exit")
        val invChecker: Cont = state => prove(loop.invariants.toList.map(InvTask(_, onEntry = false)), state)
        exec(loopBody, entryState, invChecker)(using handlers.copy(onBreak = cont, onContinue = invChecker))
        cont(exitState)
    case Break() => handlers.onBreak(state)
    case Continue() => handlers.onContinue(state)

  private def eval(expr: Expr, state: State): (Expr, State) =
    val value = expr.transform:
      case Var(x) => state.symbolicValues(x)
      case Apply(Var(f), es) if state.signatures.contains(f) => ???
    (value.simplify, state)

  private def update(name: String, expr: Expr, state: State): State =
    val (value, newState) = eval(expr, state)
    newState.copy(symbolicValues = state.symbolicValues + (name -> value))

  private def fresh(name: String, sort: Sort, state: State): (String, State) =
    val k = state.freshCounts.getOrElse(name, 0)
    val freshName = s"$name($k)"
    (freshName, state.copy(
      symbolicCtx = state.symbolicCtx + (freshName -> sort),
      freshCounts = state.freshCounts + (name -> (k + 1))))

  private def havoc(name: String, state: State): State =
    val (freshName, newState) = fresh(name, state.localCtx(name), state)
    newState.copy(symbolicValues = state.symbolicValues + (name -> Var(freshName)))

  private def havoc(names: List[String], state: State): State =
    names.foldLeft(state) { (s, x) => havoc(x, s) }

  private def add(cond: Expr, state: State, annot: String = ""): State =
    val (value, newState) = eval(cond, state)
    newState.copy(
      constraints = state.constraints :+ value,
      constraintAnnots = state.constraintAnnots :+ annot)

  private def addMany(conds: List[Expr], state: State, annot: String = ""): State =
    conds.foldLeft(state) { (s, e) => add(e, s, annot) }

  private def addIf(cond: Expr, state: State,
                    trueAnnot: String = "", falseAnnot: String = ""): (State, State) =
    val (value, newState) = eval(cond, state)
    val trueState = newState.copy(
      constraints = state.constraints :+ value,
      constraintAnnots = state.constraintAnnots :+ trueAnnot)
    val falseState = newState.copy(
      constraints = state.constraints :+ Not(value),
      constraintAnnots = state.constraintAnnots :+ falseAnnot)
    (trueState, falseState)

  private def check(expr: Expr, state: State)(using reporter: Reporter): Boolean = expr match
    case And(es) => check(es, state)
    case Implies(e1, e2) => check(e1, state) && check(e2, add(e1, state))
    case Ite(e, e1, e2) => check(e, state) && check(e1, add(e, state)) && check(e2, add(Not(e), state))

    case CharFromCode(e) => // 0 <= e <= 0xFFFF
      check(e, state) &&
        prove(SideTask(e, And(LE(Const(0), e), LE(e, Const(0xFFFF))), "code point out of bound"), state)
    case CharAt(e, ei) => // 0 <= ei < |e|
      check(e, state) && check(ei, state) &&
        prove(SideTask(ei, And(LE(Const(0), ei), LT(ei, StringLength(e))), "index out of bound"), state)
    case Substring(es, ei, ej) => // 0 <= ei and 0 <= ej
      check(es, state) && check(ei, state) && check(ej, state) &&
        prove(SideTask(ei, LE(Const(0), ei), "index out of bound"), state) &&
        prove(SideTask(ej, LE(Const(0), ej), "index out of bound"), state)
    case StringToInt(e, base) => // e is a string representing an integer in the given base
      check(e, state) &&
        prove(SideTask(e,
          StringForall(e, Lambda(List(Decl("c", CharSort)), RefinedBy(Var("c"), CharSet.intDigit(base)))),
          "not a number string"), state)

    case SeqGet(e, ei) => // 0 <= ei < |e|
      check(e, state) && check(ei, state) &&
        prove(SideTask(ei, And(LE(Const(0), ei), LT(ei, SeqLength(e))), "index out of bound"), state)
    case SeqSlice(es, ei, ej) => // 0 <= ei and 0 <= ej
      check(es, state) && check(ei, state) && check(ej, state) &&
        prove(SideTask(ei, LE(Const(0), ei), "index out of bound"), state) &&
        prove(SideTask(ej, LE(Const(0), ej), "index out of bound"), state)

    case MapGet(e, ek) => // e contains key ek
      check(e, state) && check(ek, state) &&
        prove(SideTask(ek, MapContains(e, ek), "key not bound"), state)
    case MapRemove(e, ek) => // e contains key ek
      check(e, state) && check(ek, state) &&
        prove(SideTask(ek, MapContains(e, ek), "key not bound"), state)

    case _ =>
      expr.productIterator.forall:
        case e: Expr => check(e, state)
        case _ => true

  @tailrec
  private def check(exprs: List[Expr], state: State)(using reporter: Reporter): Boolean = exprs match
    case Nil => true
    case e :: es => check(e, state) && check(es, add(e, state))

  private val verifier = new checker.Verifier

  private def prove(task: ProofTask, state: State)(using reporter: Reporter): Boolean =
    if !check(task.cond, state) then
      return false

    val (conclusion, newState) = eval(task.cond, state)
    if conclusion == Const(true) then
      return true

    logger.info("Prove: {} => {}", state.constraints.map(ppExpr).mkString(" ∧ "), ppExpr(conclusion))
    val prover = verifier.Prover(using VarCtx(state.symbolicCtx), SortingContext(state.symbolicCtx))
    state.constraints.foreach(prover.assume)
    prover.prove(conclusion) match
      case Left(_) =>
        reporter += ReportItem(task, state, Unverified)
        false
      case Right(_) =>
        reporter += ReportItem(task, state, Verified)
        true

  private def prove(tasks: List[ProofTask], state: State)(using reporter: Reporter): Boolean =
    val results = tasks.map(prove(_, state))
    results.forall(_ == true)