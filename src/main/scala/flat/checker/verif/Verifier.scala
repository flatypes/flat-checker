package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.Show.show
import flat.checker.flan.Subst.*
import flat.checker.flan.tpd.*
import flat.checker.verif.Simplifier.simplify
import org.eclipse.lsp4j.Range

class Verifier(using reporter: Reporter) extends LazyLogging:
  def verify(program: Program): Unit =
    val methodInfos = Map.from(for node <- program.body yield
      node.name -> MethodInfo(node.params, node.returns, node.requires, node.ensures, node.locals))
    program.body.foreach(verifyBody(_, methodInfos))

  private def verifyBody(node: MethodDef, methodInfos: Map[String, MethodInfo]): Unit =
    node.body match
      case None => // abstract method, nothing to verify
      case Some(body) =>
        val state = State(methods = methodInfos, currentMethod = node.name)
        val m = methodInfos(node.name)
        val onReturn: (Range, State) => Unit = (r, st) => assert(m.ensures, st, _ => PostNotProvedError(r))
        havoc(m.paramNames, state, st1 =>
          assume(m.requires, st1, st2 =>
            havoc(m.localNames, st2, st3 =>
              execBody(body, st3, onReturn(node.endRange, _))
                (using Handlers(
                  onReturn = onReturn,
                  onBreak = (_, _) => throw RuntimeException("break outside of loop"),
                  onContinue = (_, _) => throw RuntimeException("continue outside of loop"))))))

  private case class Handlers(onReturn: (Range, State) => Unit,
                              onBreak: (Range, State) => Unit,
                              onContinue: (Range, State) => Unit)

  private def execBody(body: List[Stmt], state: State, cont: State => Unit)
                      (using handlers: Handlers): Unit = body match
    case Nil => cont(state)
    case stmt :: rest => exec(stmt, state, st => execBody(rest, st, cont))

  private def exec(stmt: Stmt, state: State, cont: State => Unit)
                  (using handlers: Handlers): Unit = stmt match
    case Assign(x, e) =>
      update(x, e, state, cont)
    case Havoc(x) =>
      havoc(x, state, cont)
    case ExprStmt(e) =>
      check(e, state)
      cont(state)
    case If(e, List(Assert(eb@Const(false))), Nil) =>
      assert(Not(e), state, AssertNotProvedError(eb.range))
      cont(state)
    case If(e, b1, b2) =>
      branch(e, state,
        execBody(b1, _, cont),
        execBody(b2, _, cont))
    case s@While(e, eis, b) =>
      assert(eis, state, InvNotProvedOnEntryError(_))
      val onExit: State => Unit = assert(eis, _, InvNotMaintainedError(_))
      havoc(collectModifiedVars(b), state, st1 =>
        assume(eis, st1, st2 =>
          branch(e, st2,
            execBody(b, _, onExit)
              (using handlers.copy(onBreak = (_, st) => cont(st), onContinue = (_, st) => onExit(st))),
            cont)))
    case brk@Break() =>
      handlers.onBreak(brk.range, state)
    case cont@Continue() =>
      handlers.onContinue(cont.range, state)
    case ret@Return() =>
      handlers.onReturn(ret.range, state)
    case Assume(e) =>
      assume(e, state, cont)
    case Assert(e) =>
      assert(e, state, AssertNotProvedError(e.range))
      cont(state)

  private def update(name: String, expr: Expr, state: State, cont: State => Unit): Unit =
    if check(expr, state) then
      for e <- state.types(name).reft do
        assert(e.subst("_", expr), state, ReftNotProvedError(expr.range))
      val (v, st) = eval(expr, state)
      cont(st.copy(values = st.values + (name -> v)))
    else
      havoc(name, state, cont)

  private def havoc(name: String, state: State, cont: State => Unit): Unit =
    val typ = state.types(name)
    var (y, st) = state.fresh(name, typ.sort)
    for e <- typ.reft do
      st = st.add(e.subst("_", Var(y)))
    cont(st.copy(values = st.values + (name -> Var(y))))

  private def havoc(names: List[String], state: State, cont: State => Unit): Unit = names match
    case Nil => cont(state)
    case x :: xs => havoc(x, state, havoc(xs, _, cont))

  private def branch(cond: Expr, state: State, contTrue: State => Unit, contFalse: State => Unit): Unit =
    if check(cond, state) then
      val (v, st) = eval(cond, state)
      contTrue(st.add(v))
      contFalse(st.add(Not(v)))
    else
      contTrue(state)
      contFalse(state)

  private def assume(cond: Expr, state: State, cont: State => Unit): Unit =
    if check(cond, state) then
      cont(evalAndAdd(cond, state))
    else
      cont(state)

  private def assume(conds: List[Expr], state: State, cont: State => Unit): Unit = conds match
    case Nil => cont(state)
    case c :: cs => assume(c, state, assume(cs, _, cont))

  private inline def evalAndAdd(cond: Expr, state: State): State =
    val (v, st) = eval(cond, state)
    st.add(v)

  private def assert(cond: Expr, state: State, err: => VerifError): Boolean =
    if check(cond, state) then
      evalAndProve(cond, state, err)
    else
      false

  private def assert(conds: List[Expr], state: State, err: Range => VerifError): Boolean =
    val results = conds.map(c => assert(c, state, err(c.range)))
    results.forall(_ == true)

  private inline def evalAndProve(expr: Expr, state: State, err: => VerifError): Boolean =
    val (v, st) = eval(expr, state)
    prove(v, st.ctx, err)

  private def check(expr: Expr, state: State): Boolean = expr match
    // indices and keys
    case SeqSelect(e, ei) => // 0 <= ei < |e|
      check(e, state) && check(ei, state) &&
        evalAndProve(And(Le(Const(0), ei), Lt(ei, SeqLength(e))), state, IndexOutOfBoundsError(ei.range))
    case SeqSlice(e, ei, NoExpr) => // 0 <= ei
      check(e, state) && check(ei, state) &&
        evalAndProve(Le(Const(0), ei), state, IndexOutOfBoundsError(ei.range))
    case SeqSlice(e, ei, ej) => // 0 <= ei, 0 <= ej
      check(e, state) && check(ei, state) && check(ej, state) &&
        (evalAndProve(Le(Const(0), ei), state, IndexOutOfBoundsError(ei.range)) &
          evalAndProve(Le(Const(0), ej), state, IndexOutOfBoundsError(ej.range)))
    case MapSelect(e, ek) => // ek in e
      check(e, state) && check(ek, state) &&
        evalAndProve(MapContains(e, ek), state, KeyNotExistError(ek.range))

    // method call
    case Apply(MethodRef(f), es) =>
      val m = state.methods(f)
      if es.forall(check(_, state)) then
        val (vs, st) = eval(es, state)
        val results1 = for i <- es.indices; e <- m.params(i).typ.reft yield
          prove(e.subst("_", vs(i)), st.ctx, ReftNotProvedError(es(i).range))
        val results2 = for e <- m.requires yield
          prove(e.subst(m.paramNames, vs), st.ctx, PreNotProvedError(expr.range))
        results1.forall(_ == true) && results2.forall(_ == true)
      else
        false

    // contextual
    case And(e1, e2) => check(e1, state) && check(e2, evalAndAdd(e1, state))
    case Implies(e1, e2) => check(e1, state) && check(e2, evalAndAdd(e1, state))
    case Ite(e, e1, e2) => check(e, state) && check(e1, evalAndAdd(e, state)) && check(e2, evalAndAdd(Not(e), state))

    // others
    case _ =>
      val results = expr.subtrees.map(check(_, state))
      results.forall(_ == true)

  private def eval(expr: Expr, state: State)(using boundVars: Set[String] = Set.empty): (Expr, State) = expr match
    case Var(x) =>
      val value = if !boundVars.contains(x) then state.values(x) else expr
      (value, state)
    case Apply(MethodRef(f), es) =>
      val m = state.methods(f)
      val (vs, st1) = eval(es, state)
      var st = st1
      m.ensures match
        case List(Eq(Var("_"), e)) if e.collect { case Var("_") => () }.isEmpty =>
          (e.subst(m.paramNames, vs), st)
        case _ =>
          val (y, st1) = st.fresh(f, m.returns.typ.sort)
          st = st1
          val v: Expr = Var(y)
          for e <- m.returns.typ.reft do
            st = st.add(e.subst("_", v))
          for e <- m.ensures do
            st = st.add(e.subst("_" :: m.paramNames, v :: vs))
          (v, st)
    case _ =>
      val (vs, st) = eval(expr.subtrees, state)
      (expr.rebuild(vs), st)

  private def eval(exprs: List[Expr], state: State): (List[Expr], State) = exprs match
    case Nil => (Nil, state)
    case e :: es =>
      val (v, st1) = eval(e, state)
      val (vs, st2) = eval(es, st1)
      (v :: vs, st2)

  private val prover = Prover()

  private inline def prove(value: Expr, ctx: PrfCtx, err: => VerifError): Boolean =
    val goal = Goal(ctx.premises, value.simplify(using ctx.vars))(using ctx.vars)
    if prover.prove(goal) then
      logger.trace("Proved:\n{}", showTask(ctx, goal.conclusion))
      true
    else
      logger.debug("FAILED:\n{}", showTask(ctx, goal.conclusion))
      reporter.report(err)
      false

  private def collectModifiedVars(body: List[Stmt]): List[String] =
    val vars = body.flatMap:
      case Assign(x, _) => List(x)
      case Havoc(x) => List(x)
      case If(_, b1, b2) => collectModifiedVars(b1) ++ collectModifiedVars(b2)
      case While(_, _, b) => collectModifiedVars(b)
      case _ => Nil
    vars.distinct

  private def showTask(ctx: PrfCtx, value: Expr): String =
    val lines = for e <- ctx.premises yield s"  ${e.show}\n"
    lines.mkString + s" ⇒ ${value.show}\n"
