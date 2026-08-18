package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.tpd.*

class Verifier(using reporter: Reporter) extends LazyLogging:
  def verify(program: Program): Unit =
    val methodInfos = Map.from(for node <- program.body yield
      node.name -> MethodInfo(node.params, node.returns, node.requires, node.ensures, node.locals))
    program.body.foreach(verifyBody(_, methodInfos))

  private def verifyBody(node: MethodDef, methodInfos: Map[String, MethodInfo]): Unit =
    node.body match
      case None => // abstract method, nothing to verify
      case Some(body) =>
        val types = Map.from(for p <- node.params yield p.name -> p.typ) ++
          Map.from(for r <- node.returns yield r.name -> r.typ) ++
          Map.from(for l <- node.locals yield l.name -> l.typ)
        val generator = VCGenerator(types, methodInfos)
        val post = mkVCAssert(mkAnd(node.ensures), e => PostNotProvedError(e.range))
        val vc = mkAnd(node.requires) -> generator.wlp(body, post, post, VCTrue, VCTrue)
        val discharger = VCDischarger(methodInfos)
        discharger.discharge(vc, types, Nil)
//
//        val state = State(methods = methodInfos, currentMethod = node.name)
//        val m = methodInfos(node.name)
//        val onReturn: (Range, State) => Unit = (r, st) => assert(m.ensures, st, _ => PostNotProvedError(r))
//        havoc(m.names, state, st1 =>
//          assume(m.requires, st1, st2 =>
//            execBody(body, st2, onReturn(node.endRange, _))
//              (using Handlers(
//                onReturn = onReturn,
//                onBreak = (_, _) => throw RuntimeException("break outside of loop"),
//                onContinue = (_, _) => throw RuntimeException("continue outside of loop")))))
//
//  private case class Handlers(onReturn: (Range, State) => Unit,
//                              onBreak: (Range, State) => Unit,
//                              onContinue: (Range, State) => Unit)
//
//  private def execBody(body: List[Stmt], state: State, cont: State => Unit)
//                      (using handlers: Handlers): Unit = body match
//    case Nil => cont(state)
//    case stmt :: rest => exec(stmt, state, st => execBody(rest, st, cont))
//
//  private def exec(stmt: Stmt, state: State, cont: State => Unit)
//                  (using handlers: Handlers): Unit = stmt match
//    case Assign(x, Some(e)) =>
//      update(x, e, state, cont)
//    case Havoc(x) =>
//      havoc(x, state, cont)
//    case ExprStmt(e) =>
//      check(e, state)
//      cont(state)
//    case If(e, List(Assert(eb@BoolLit(false))), b) =>
//      assert(Not(e), state, AssertNotProvedError(eb.range))
//      execBody(b, state, cont)
//    case If(e, List(Abort(eb)), b) =>
//      assert(Not(e), state, AssertNotProvedError(eb.range))
//      execBody(b, state, cont)
//    case If(e, b1, b2) =>
//      branch(e, state,
//        execBody(b1, _, cont),
//        execBody(b2, _, cont))
//
//    case s@While(e, eis, b) =>
//      assert(eis, state, InvNotProvedOnEntryError(_))
//      val onExit: State => Unit = assert(eis, _, InvNotMaintainedError(_))
//      havoc(collectModifiedVars(b), state, st1 =>
//        assume(eis, st1, st2 =>
//          branch(e, st2,
//            execBody(b, _, onExit)
//              (using handlers.copy(onBreak = (_, st) => cont(st), onContinue = (_, st) => onExit(st))),
//            cont)))
//
//    case s@For(x, SeqLit(es), eis, b) if es.length < 5 =>
//      execFor(x, es, b, state, cont)
//    case s@For(x, StrLit(cs), eis, b) if cs.length < 5 =>
//      execFor(x, cs.toList.map(CharLit(_)), b, state, cont)
//    case s@For(x, e, eis, b) =>
//      assert(eis, state, InvNotProvedOnEntryError(_))
//      val ex: Expr = e.sort(using state.sorts) match
//        case `strType` => SeqContains(e, CharToString(Var(x)))
//        case ListType(s) => SeqContains(e, SeqLit(List(Var(x)))(s))
//        case _ => throw RuntimeException(s"Expected a sequence type for For-loop, but got ${e.sort(using state.sorts)}")
//      val onExit: State => Unit = assert(eis, _, InvNotMaintainedError(_))
//      havoc(collectModifiedVars(b), state, st1 =>
//        assume(ex :: eis, st1, st2 =>
//          execBody(b, st2, onExit)
//            (using handlers.copy(onBreak = (_, st) => cont(st), onContinue = (_, st) => onExit(st))))
//        cont(st1))
//
//    case brk@Break() =>
//      handlers.onBreak(brk.range, state)
//    case cont@Continue() =>
//      handlers.onContinue(cont.range, state)
//    case ret@Return() =>
//      handlers.onReturn(ret.range, state)
//
//    case Assume(e) =>
//      assume(e, state, cont)
//    case Assert(e) =>
//      assert(e, state, AssertNotProvedError(e.range))
//      cont(state)
//    case Abort(msg) =>
//      assert(BoolLit(false), state, AssertNotProvedError(msg.range))
//
//  private def execFor(name: String, values: List[Expr], body: List[Stmt], state: State, cont: State => Unit)
//                     (using handlers: Handlers): Unit = values match
//    case Nil => cont(state)
//    case v :: vs =>
//      logger.debug("Unrolling for-loop {} = {}", name, v.show)
//      val onExit = (st: State) => execFor(name, vs, body, st, cont)
//      update(name, v, state, st1 =>
//        execBody(body, st1, onExit)
//          (using handlers.copy(onBreak = (_, st) => cont(st), onContinue = (_, st) => onExit(st))))
//
//  private def update(name: String, expr: Expr, state: State, cont: State => Unit): Unit =
//    if check(expr, state) && checkType(expr, state.types(name), state) then
//      val (v, st) = eval(expr, state)
//      cont(st.copy(values = st.values + (name -> v)))
//
//  private def checkType(expr: Expr, typ: Type, state: State): Boolean = typ match
//    case IntType | BoolType | CharType | `strType` | NullType => true
//    case RefinedType(_, e) =>
//      assert(e.subst("_", expr), state, ReftNotProvedError(expr.range))
//    case _ => throw RuntimeException(s"Unsupported type $typ in checkType")
//
//  private def havoc(name: String, state: State, cont: State => Unit): Unit =
//    val typ = state.types(name)
//    val (y, st) = state.fresh(name, typ)
//    cont(st.copy(values = st.values + (name -> Var(y))))
//
//  private def havoc(names: List[String], state: State, cont: State => Unit): Unit = names match
//    case Nil => cont(state)
//    case x :: xs => havoc(x, state, havoc(xs, _, cont))
//
//  private def branch(cond: Expr, state: State, contTrue: State => Unit, contFalse: State => Unit): Unit =
//    if check(cond, state) then
//      val (v, st) = eval(cond, state)
//      v.simplify(using st.ctx.vars) match
//        case BoolLit(true) => contTrue(st)
//        case BoolLit(false) => contFalse(st)
//        case _ =>
//          contTrue(st.add(v))
//          contFalse(st.add(Not(v)))
//    else
//      contTrue(state)
//      contFalse(state)
//
//  private def assume(cond: Expr, state: State, cont: State => Unit): Unit =
//    if check(cond, state) then
//      cont(evalAndAdd(cond, state))
//    else
//      cont(state)
//
//  private def assume(conds: List[Expr], state: State, cont: State => Unit): Unit = conds match
//    case Nil => cont(state)
//    case c :: cs => assume(c, state, assume(cs, _, cont))
//
//  private inline def evalAndAdd(cond: Expr, state: State): State =
//    val (v, st) = eval(cond, state)
//    st.add(v)
//
//  private def assert(cond: Expr, state: State, err: => VerifError): Boolean =
//    if check(cond, state) then
//      evalAndProve(cond, state, err)
//    else
//      false
//
//  private def assert(conds: List[Expr], state: State, err: Range => VerifError): Boolean =
//    val results = conds.map(c => assert(c, state, err(c.range)))
//    results.forall(_ == true)
//
//  private inline def evalAndProve(expr: Expr, state: State, err: => VerifError): Boolean =
//    val (v, st) = eval(expr, state)
//    prove(v, st.ctx, err)
//
//  private def check(expr: Expr, state: State): Boolean = expr match
//    // indices and keys
//    case ListAt(e, ei) => // 0 <= ei < |e|
//      check(e, state) && check(ei, state) &&
//        evalAndProve(And(Le(IntLit(0), ei), Lt(ei, SeqLength(e))), state, IndexOutOfBoundsError(ei.range))
//    case SeqSlice(e, ei, NoExpr) => // 0 <= ei
//      check(e, state) && check(ei, state) &&
//        evalAndProve(Le(IntLit(0), ei), state, IndexOutOfBoundsError(ei.range))
//    case SeqSlice(e, ei, ej) => // 0 <= ei, 0 <= ej
//      check(e, state) && check(ei, state) && check(ej, state) &&
//        (evalAndProve(Le(IntLit(0), ei), state, IndexOutOfBoundsError(ei.range)) &
//          evalAndProve(Le(IntLit(0), ej), state, IndexOutOfBoundsError(ej.range)))
//    case MapSelect(e, ek) => // ek in e
//      check(e, state) && check(ek, state) &&
//        evalAndProve(MapContains(e, ek), state, KeyNotExistError(ek.range))
//
//    // method call
//    case Apply(MethodRef(f), es) =>
//      val m = state.methods(f)
//      if es.forall(check(_, state)) then
//        val results1 = for i <- es.indices yield checkType(es(i), m.params(i).typ, state)
//        val (vs, st) = eval(es, state)
//        val results2 = for e <- m.requires yield
//          prove(e.subst(m.paramNames, vs), st.ctx, PreNotProvedError(expr.range))
//        results1.forall(_ == true) && results2.forall(_ == true)
//      else
//        false
//
//    // contextual
//    case And(e1, e2) => check(e1, state) && check(e2, evalAndAdd(e1, state))
//    case Implies(e1, e2) => check(e1, state) && check(e2, evalAndAdd(e1, state))
//    case Ite(e, e1, e2) => check(e, state) && check(e1, evalAndAdd(e, state)) && check(e2, evalAndAdd(Not(e), state))
//
//    // others
//    case _ =>
//      val results = expr.subtrees.map(check(_, state))
//      results.forall(_ == true)
//
//  private def eval(expr: Expr, state: State)(using boundVars: Set[String] = Set.empty): (Expr, State) = expr match
//    case Var(x) =>
//      val value = if !boundVars.contains(x) then state.values(x) else expr
//      (value, state)
//    case Apply(MethodRef(f), es) =>
//      val m = state.methods(f)
//      val (vs, st1) = eval(es, state)
//      var st = st1
//      m.ensures match
//        case List(Eq(Var(y), e)) if m.returnNames == List(y) && e.collect { case Var(`y`) => () }.isEmpty =>
//          (e.subst(m.paramNames, vs).simplify(using state.ctx.vars), st)
//        case _ =>
//          val rvs = ListBuffer.empty[Expr]
//          for p <- m.returns do
//            val (y, st1) = st.fresh(f + "_" + p.name, ???)
//            st = st1
//          for e <- m.ensures do
//            st = st.add(e.subst(m.paramNames ++ m.returnNames, vs ++ rvs))
//          val v: Expr = rvs.toList match
//            case List(e) => e
//            case es => TupleExpr(es)
//          (v, st)
//    case _ =>
//      val (vs, st) = eval(expr.subtrees, state)
//      (expr.rebuild(vs).simplify(using state.ctx.vars), st)
//
//  private def eval(exprs: List[Expr], state: State): (List[Expr], State) = exprs match
//    case Nil => (Nil, state)
//    case e :: es =>
//      val (v, st1) = eval(e, state)
//      val (vs, st2) = eval(es, st1)
//      (v :: vs, st2)
//
//  private val prover = Prover()
//
//  private inline def prove(value: Expr, ctx: PrfCtx, err: => VerifError): Boolean =
//    val goal = Goal(ctx.premises, value.simplify(using ctx.vars))(using ctx.vars)
//    logger.debug("")
//    logger.debug("Goal:\n{}", showTask(ctx, goal.conclusion))
//    if prover.prove(goal) then
//      logger.debug("PROVED")
//      true
//    else
//      logger.warn("❌ FAILED")
//      reporter.report(err)
//      false
//
//  private def collectModifiedVars(body: List[Stmt]): List[String] =
//    val vars = body.flatMap:
//      case Assign(x, _) => List(x)
//      case Havoc(x) => List(x)
//      case If(_, b1, b2) => collectModifiedVars(b1) ++ collectModifiedVars(b2)
//      case While(_, _, b) => collectModifiedVars(b)
//      case _ => Nil
//    vars.distinct
//
