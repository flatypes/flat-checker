package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.Show.show
import flat.checker.flan.TypeOps.{erase, isSort}
import flat.checker.flan.tpd.*
import flat.checker.verif.Signal.*

import java.util.concurrent.atomic.AtomicInteger

class Verifier(using reporter: Reporter) extends LazyLogging:
  def verify(module: Module): Unit =
    val methodSigs = for m <- module.body yield
      m.name -> MethodSig(m.params, m.returnType, m.requires, m.ensures)
    val state = State(module.uri, methodSigs.toMap)
    for m <- module.body do
      execMethod(m.body, state.copy(currentMethod = Some(m.name)))

  private def execMethod(body: List[Stmt], state: State): Unit =
    val sig = state.getMethodSig
    val initState = state.pushed.declare(sig.params).havoc(sig.paramNames.toSet)
    execBody(body, List(assume(sig.pre, initState)))

  private def execBody(body: List[Stmt], states: List[State]): List[State] =
    require(states.forall(_.signal == NEXT), "States should be in normal mode")
    body match
      case Nil => states
      case stmt :: rest =>
        val (nextStates, returnStates) = states.flatMap(exec(stmt, _)).partition(_.signal == NEXT)
        execBody(rest, nextStates) ++ returnStates

  private def exec(stmt: Stmt, state: State): List[State] = stmt match
    case ExprStmt(e) =>
      safetyCheck(e, state)
      List(state)
    case Declare(x, t, Some(e)) =>
      assign(x, e, state.declare(x, t))
    case Declare(x, t, None) =>
      List(state.declare(x, t).havoc(x))
    case Assign(x, e) =>
      assign(x, e, state)
    case Havoc(x) =>
      List(state.havoc(x))

    case s@MethodCall(xs, m, es) =>
      val sig = state.methods(m)
      // Check preconditions
      val safe = es.map(safetyCheck(_, state))
      if safe.forall(_ == true) then
        for (e, t) <- es.zip(sig.paramTypes) do
          typeCheck(e, t, state)
        for b <- sig.applyPre(es) do
          prove(b, state, () => PreNotProved(s.range))
      // Assume postconditions
      val newState = xs match
        case Nil => state
        case List(x) =>
          val updatedState = state.declare(x, sig.returnType).havoc(x)
          if safe.forall(_ == true) then assume(sig.applyPost(es, List(Var(x))), updatedState)
          else updatedState
        case _ => sig.returnType match
          case TupleType(ts) if xs.length == ts.length =>
            val updatedState = state.declare(xs.zip(ts)).havoc(xs.toSet)
            if safe.forall(_ == true) then assume(sig.applyPost(es, xs.map(Var(_))), updatedState)
            else updatedState
          case _ =>
            throw new IllegalArgumentException("invalid number of return variables for method call")
      List(newState)

    // Proof derivatives
    case Assume(e) =>
      List(if safetyCheck(e, state) then assume(e, state) else state)
    case Assert(e, err) =>
      if safetyCheck(e, state) then
        prove(e, state, () => AssertNotProved(e.range))
      List(state)
    case s@Abort() =>
      prove(BoolConst(false), state, () => AssertNotProved(s.range))
      Nil

    // Control flow
    case Break() => List(state.copy(signal = BREAK))
    case Continue() => List(state.copy(signal = CONTINUE))
    case Return(e) =>
      val sig = state.getMethodSig
      if safetyCheck(e, state) then
        typeCheck(e, sig.returnType, state)
        for b <- sig.applyPost(sig.paramNames.map(Var(_)), List(e)) do
          prove(b, state, () => PostNotProved(e.range))
      Nil

    case If(e, bd1, bd2) =>
      val (trueState, falseState) =
        if safetyCheck(e, state) then (assume(e, state), assume(Not(e), state))
        else (state, state)
      execBody(bd1, List(trueState)) ++ execBody(bd2, List(falseState))

    case While(e, inv, bd) =>
      val havocState = state.havoc(collectModifiedVars(bd))
      val safeInv = inv.filter(safetyCheck(_, havocState))
      // Check invariants on entry
      for ei <- safeInv do
        prove(ei, state, () => InvNotProvedOnEntry(ei.range))
      // Execute loop body once
      val invState = assume(safeInv, havocState)
      val (trueState, falseState) =
        if safetyCheck(e, state) then (assume(e, invState), assume(Not(e), invState))
        else (invState, invState)
      val finalStates = execBody(bd, List(trueState))
      // Check invariants maintained
      for st <- finalStates; if st.signal == NEXT || st.signal == CONTINUE do
        for ei <- inv do
          prove(ei, st, () => InvNotMaintained(ei.range))
      // Loop exits
      falseState :: finalStates.filter(_.signal == BREAK).map(_.copy(signal = NEXT))

  private def safetyCheck(expr: Expr, state: State): Boolean = expr match
    // Boolean
    case And(e1, e2) => safetyCheck(e1, state) && safetyCheck(e2, assume(e1, state))
    case Or(e1, e2) => safetyCheck(e1, state) && safetyCheck(e2, assume(Not(e1), state))
    case Ite(e, e1, e2) =>
      safetyCheck(e, state)
        && safetyCheck(e1, assume(e, state)) && safetyCheck(e2, assume(Not(e), state))

    // String
    case CharAt(e, ei) =>
      safetyCheck(e, state) && safetyCheck(ei, state)
        && prove(And(Le(IntConst(0), ei), Lt(ei, StrLength(e))), state, () => IndexOutOfBounds(ei.range))
    case Substr(e, ei, ej) =>
      safetyCheck(e, state) && safetyCheck(ei, state) && safetyCheck(ej, state)
        && prove(Le(IntConst(0), ei), state, () => IndexOutOfBounds(ei.range))
        && prove(Le(IntConst(0), ej), state, () => IndexOutOfBounds(ej.range))

    // List
    case ListSelect(e, ei) =>
      safetyCheck(e, state) && safetyCheck(ei, state)
        && prove(And(Le(IntConst(0), ei), Lt(ei, ListLength(e))), state, () => IndexOutOfBounds(ei.range))
    case ListSlice(e, ei, ej) =>
      safetyCheck(e, state) && safetyCheck(ei, state) && safetyCheck(ej, state)
        && prove(Le(IntConst(0), ei), state, () => IndexOutOfBounds(ei.range))
        && prove(Le(IntConst(0), ej), state, () => IndexOutOfBounds(ej.range))

    // Map
    case MapSelect(e, ek) =>
      safetyCheck(e, state) && safetyCheck(ek, state)
        && prove(MapContains(e, ek), state, () => KeyNotExist(ek.range))

    // Others
    case Lambda(_, _) => true
    case _ => expr.subtrees.forall(safetyCheck(_, state))

  private def typeCheck(expr: Expr, typ: Type, state: State): Boolean =
    hasType(expr, typ).forall(prove(_, state, () => ReftNotProved(expr.range)))

  private def assign(variable: String, expr: Expr, state: State): List[State] =
    if safetyCheck(expr, state) && typeCheck(expr, state.getType(variable), state) then
      List(state.assign(variable, eval(expr, state)))
    else
      Nil

  private inline def assume(expr: Expr, state: State): State =
    state.copy(conds = state.conds :+ eval(expr, state))

  private inline def assume(exprs: List[Expr], state: State): State =
    state.copy(conds = state.conds ++ exprs.map(eval(_, state)))

  private val goalCounter = new AtomicInteger(0)

  private def prove(cond: Expr, state: State, err: () => ProofError): Boolean =
    val typeConds = state.symbolicVars.flatMap { (x, t) => hasType(Var(x), t) }.toList
    val sorts = for (x, t) <- state.symbolicVars yield x -> t.erase
    val goal = Goal(eval(cond, state), typeConds ++ state.conds, sorts)
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

  private def eval(expr: Expr, state: State, boundVars: Set[String] = Set.empty): Expr = expr match
    case Var(x) => if boundVars.contains(x) then expr else state.getSymbolicValue(x)
    case f@Lambda(ps, e) => Lambda(ps, eval(e, state, boundVars ++ f.paramNames))
    case _ =>
      val values = expr.subtrees.map(eval(_, state, boundVars))
      expr.rebuild(values)

  private def hasType(expr: Expr, typ: Type): Option[Expr] = typ match
    case IntType | BoolType | StrType => None
    case ListType(t) => for b <- hasType(Var("x"), t) yield ListForall(expr, Lambda(List("x" -> t), b))
    case TupleType(ts) =>
      val bs = ts.zipWithIndex.flatMap { (t, i) => hasType(TupleSelect(i, expr), t) }
      if bs.isEmpty then None else Some(mkAnd(bs))
    case OptType(t) => for b <- hasType(expr, t) yield Implies(Ne(expr, NullConst()), b)
    case RefinedType(x, t, e) =>
      assert(t.isSort)
      Some(e.subst(Map(x -> expr)))
    case _ => throw new IllegalArgumentException(typ.show)

  private def collectModifiedVars(block: List[Stmt]): Set[String] =
    block.flatMap:
      case Assign(x, _) => List(x)
      case Havoc(x) => List(x)
      case If(_, bd1, bd2) => collectModifiedVars(bd1) ++ collectModifiedVars(bd2)
      case While(_, _, bd) => collectModifiedVars(bd)
      case _ => Nil
    .toSet