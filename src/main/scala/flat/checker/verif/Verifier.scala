package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.tpd.*
import flat.checker.verif.Signal.*

import java.util.concurrent.atomic.AtomicInteger

class Verifier(using reporter: Reporter) extends LazyLogging:
  def verify(script: Script): Unit =
    val initState = State(script.uri)
    execBody(script.body, List(initState))

  private def execBody(body: List[Stmt], states: List[State]): List[State] =
    require(states.forall(_.signal == NORMAL), "States should be in normal mode")
    body match
      case Nil => states
      case stmt :: rest =>
        val (nextStates, returnStates) = states.flatMap(exec(stmt, _)).partition(_.signal == NORMAL)
        execBody(rest, nextStates) ++ returnStates

  private def exec(stmt: Stmt, state: State): List[State] = stmt match
    case ExprStmt(e) =>
      safetyCheck(e, state)
      List(state)
    case Declare(x, t, Some(e)) =>
      if safetyCheck(e, state) then
        List(state.declare(x, t).assign(x, eval(e, state)))
      else
        Nil
    case Declare(x, t, None) =>
      List(state.declare(x, t).havoc(x))
    case Assign(x, e) =>
      if safetyCheck(e, state) then
        List(state.assign(x, eval(e, state)))
      else
        Nil
    case Havoc(x) =>
      List(state.havoc(x))

    // Proof derivatives
    case Assume(e) =>
      if safetyCheck(e, state) then
        List(state.assume(eval(e, state)))
      else
        List(state)
    case Assert(e, err) =>
      if safetyCheck(e, state) then
        evalAndProve(e, state, err)
      List(state)
    case s@Abort() =>
      evalAndProve(BoolConst(false), state, () => AssertNotProved(s.range))
      Nil

    // Control flow
    case Break() => List(state.copy(signal = BREAK))
    case Continue() => List(state.copy(signal = CONTINUE))
    case Return() => Nil

    case If(e, bd1, bd2) =>
      if safetyCheck(e, state) then
        val trueStates = execBody(bd1, List(state.assume(eval(e, state))))
        val falseStates = execBody(bd2, List(state.assume(eval(Not(e), state))))
        trueStates ++ falseStates
      else
        Nil

    case While(e, inv, bd) =>
      val havocState = state.havoc(collectModifiedVars(bd))
      if safetyCheck(e, havocState) then
        // Check invariants on entry
        for (ei, span) <- inv do
          evalAndProve(ei, state, () => InvNotProvedOnEntry(span))
        val invState = havocState.assume(for (ei, _) <- inv yield eval(ei, havocState))
        // Execute loop body once and check invariants again
        val finalStates = execBody(bd, List(invState.assume(eval(e, invState))))
        for st <- finalStates; if st.signal == NORMAL || st.signal == CONTINUE do
          for (ei, span) <- inv do
            evalAndProve(ei, st, () => InvNotMaintained(span))
        // Loop exits
        val normalExitState = havocState.assume(eval(Not(e), havocState))
        val breakExitStates = finalStates.filter(_.signal == BREAK).map(_.copy(signal = NORMAL))
        normalExitState :: breakExitStates
      else
        Nil

  def safetyCheck(expr: Expr, state: State): Boolean = expr match
    // Boolean
    case And(e1, e2) => safetyCheck(e1, state) && safetyCheck(e2, evalAndAssume(e1, state))
    case Or(e1, e2) => safetyCheck(e1, state) && safetyCheck(e2, evalAndAssume(Not(e1), state))
    case Ite(e, e1, e2) =>
      safetyCheck(e, state)
        && safetyCheck(e1, evalAndAssume(e, state)) && safetyCheck(e2, evalAndAssume(Not(e), state))

    // String
    case CharAt(e, ei) =>
      safetyCheck(e, state) && safetyCheck(ei, state)
        && evalAndProve(And(Le(IntConst(0), ei), Lt(ei, StrLength(e))), state, () => IndexOutOfBounds(ei.range))
    case Substr(e, ei, ej) =>
      safetyCheck(e, state) && safetyCheck(ei, state) && safetyCheck(ej, state)
        && evalAndProve(Le(IntConst(0), ei), state, () => IndexOutOfBounds(ei.range))
        && evalAndProve(Le(IntConst(0), ej), state, () => IndexOutOfBounds(ej.range))

    // List
    case ListSelect(e, ei) =>
      safetyCheck(e, state) && safetyCheck(ei, state)
        && evalAndProve(And(Le(IntConst(0), ei), Lt(ei, ListLength(e))), state, () => IndexOutOfBounds(ei.range))
    case ListSlice(e, ei, ej) =>
      safetyCheck(e, state) && safetyCheck(ei, state) && safetyCheck(ej, state)
        && evalAndProve(Le(IntConst(0), ei), state, () => IndexOutOfBounds(ei.range))
        && evalAndProve(Le(IntConst(0), ej), state, () => IndexOutOfBounds(ej.range))

    // Map
    case MapSelect(e, ek) =>
      safetyCheck(e, state) && safetyCheck(ek, state)
        && evalAndProve(MapContains(e, ek), state, () => KeyNotExist(ek.range))

    // Others
    case Lambda(_, _) => true
    case _ => expr.subtrees.forall(safetyCheck(_, state))

  private def eval(expr: Expr, state: State, boundVars: Set[String] = Set.empty): Expr = expr match
    case Var(x) => if boundVars.contains(x) then expr else state.getValue(x)
    case f@Lambda(ps, e) => Lambda(ps, eval(e, state, boundVars ++ f.paramNames))
    case _ =>
      logger.debug("Evaluating expression {}, bound: {}", expr, boundVars)
      val values = expr.subtrees.map(eval(_, state, boundVars))
      expr.rebuild(values)

  private inline def evalAndAssume(expr: Expr, state: State): State = state.assume(eval(expr, state))

  private val goalCounter = new AtomicInteger(0)

  private def evalAndProve(cond: Expr, state: State, err: () => ProofError): Boolean =
    val goal = Goal(eval(cond, state), state.conds, state.symbolicVars)
    val number = goalCounter.incrementAndGet()
    logger.info("Goal {}:\n{}", number, goal.pp)
    if Prover.prove(goal) then
      logger.info("Goal {} PROVED", number)
      true
    else
      reporter.report(state.uri, err())
      logger.warn("Goal {} ❌", number)
      System.exit(1)
      false

  private def collectModifiedVars(block: List[Stmt]): Set[String] =
    block.flatMap:
      case Assign(x, _) => List(x)
      case Havoc(x) => List(x)
      case If(_, bd1, bd2) => collectModifiedVars(bd1) ++ collectModifiedVars(bd2)
      case While(_, _, bd) => collectModifiedVars(bd)
      case _ => Nil
    .toSet