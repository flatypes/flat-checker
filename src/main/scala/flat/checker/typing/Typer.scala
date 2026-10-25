package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.*
import flat.checker.flan.Show.show
import flat.checker.flan.sorting.*
import flat.checker.flan.tpd.*
import flat.checker.verif.AssertNotProved

import scala.collection.mutable.ListBuffer

class Typer(using reporter: Reporter) extends LazyLogging:
  private val resolver = new Resolver

  def typeCheck(tree: untpd.Module): Module =
    val ctx = resolver.resolve(tree)
    val body = tree.body.flatMap:
      case node: untpd.FunDef => Some(checkMethod(node, ctx))
      case _ => None
    val importedMethods = ctx.scope.collect:
      case (m, info: MethodInfo) if body.forall(_.name != m) =>
        MethodDef(m, info.params, info.returnType, info.requires, info.ensures, Nil, Nil)
    Module(tree.uri, importedMethods.toList ++ body)

  private def checkMethod(node: untpd.FunDef, globalCtx: TypingCtx): MethodDef =
    val name = node.ident.name
    val info = globalCtx.get(name).get.asInstanceOf[MethodInfo]
    val (body, localCtx) = checkBlock(node.body, globalCtx.copy(stack = info.scope :: globalCtx.stack), info, 0)
    val locals = localCtx.scope.collect { case (x, VarInfo(_, t)) => x -> t }
    MethodDef(name, info.params, info.returnType, info.requires, info.ensures, locals.toList, body)

  private def checkBlock(nodes: List[untpd.Stmt], localCtx: TypingCtx,
                         methodInfo: MethodInfo, loopLevel: Int): (List[Stmt], TypingCtx) =
    var ctx = localCtx
    val body = ListBuffer.empty[Stmt]
    for node <- nodes do
      val (stmts, newCtx) = checkStmt(node, ctx, methodInfo, loopLevel)
      ctx = newCtx
      body ++= stmts
    (body.toList, ctx)

  private def checkStmt(node: untpd.Stmt, ctx: TypingCtx,
                        methodInfo: MethodInfo, loopLevel: Int): (List[Stmt], TypingCtx) = node match
    case untpd.Pass() => (Nil, ctx)
    case untpd.ExprStmt(e) =>
      val (typ, expr, side) = resolver.coreAPIs.infer(e, ctx)
      (side :+ ExprStmt(expr), ctx)

    case untpd.Assign(tg@untpd.TermName(x), e) =>
      ctx.lookup(x) match
        case Some(VarInfo(_, typ)) => // assign to existing variable
          val (value, side) = resolver.coreAPIs.check(e, typ, ctx)
          (side :+ Assign(x, value), ctx)
        case Some(_) => // not assignable
          reporter.report(ctx.uri, NotAssignable(tg.range))
          (Nil, ctx)
        case None => // declare a new variable
          val (typ, value, side) = resolver.coreAPIs.infer(e, ctx)
          val info = VarInfo(untpd.Ident(x)(tg.range), typ)
          (side :+ Assign(x, value), ctx.declare(x, info))
    case untpd.Assign(tg, e) =>
      val (typ, value, side) = resolver.coreAPIs.infer(e, ctx)
      val (stmts, newCtx) = matchTarget(tg, value, typ, ctx)
      (side ++ stmts, newCtx)

    case untpd.AnnAssign(id, t, init) =>
      val typ = resolver.coreAPIs.unfold(t, ctx)
      val newCtx = resolver.coreAPIs.declare(id, VarInfo(id, typ), ctx)
      init match
        case Some(e) =>
          val (value, side) = resolver.coreAPIs.check(e, typ, ctx)
          (side ++ List(Assign(id.name, value)), newCtx)
        case None =>
          (Nil, newCtx)

    // Proof derivatives
    case untpd.Assume(e) =>
      val (cond, side) = resolver.coreAPIs.check(e, BoolType, ctx)
      (side :+ Assume(cond), ctx)
    case untpd.Assert(e) =>
      val (cond, side) = resolver.coreAPIs.check(e, BoolType, ctx)
      (side :+ Assert(cond, () => AssertNotProved(e.range)), ctx)
    case untpd.Abort(em) =>
      resolver.coreAPIs.check(em, StrType, ctx)
      (List(Abort()(em.range)), ctx)
    case untpd.If(e, List(untpd.Abort(em)), Nil) =>
      resolver.coreAPIs.check(em, StrType, ctx)
      val (cond, side) = resolver.coreAPIs.check(e, BoolType, ctx)
      (side :+ Assert(Not(cond), () => AssertNotProved(em.range)), ctx)

    // Control flow
    case untpd.If(e, bd1, bd2) =>
      val (cond, side) = resolver.coreAPIs.check(e, BoolType, ctx)
      val (thenBody, thenCtx) =
        checkBlock(bd1, resolver.coreAPIs.assume(cond, ctx), methodInfo, loopLevel)
      val (elseBody, elseCtx) =
        checkBlock(bd2, resolver.coreAPIs.assume(Not(cond), ctx.copy(stack = thenCtx.stack)), methodInfo, loopLevel)
      (side :+ If(cond, thenBody, elseBody), ctx.copy(stack = elseCtx.stack))

    case s@untpd.Return(Some(e)) =>
      val (value, side) = resolver.coreAPIs.check(e, methodInfo.returnType, ctx)
      (side ++ List(Return(value)(s.range)), ctx)
    case s@untpd.Return(None) =>
      if methodInfo.returnType != unitType then
        reporter.report(ctx.uri, MissingReturnValue(s.range))
      (List(Return(TupleExpr(Nil))(s.range)), ctx)

    case s@untpd.While(e, _, bd) =>
      val (cond, side) = resolver.coreAPIs.check(e, BoolType, ctx)
      assert(side.isEmpty, "Side effects are not allowed in loop guards")
      val (invariants, side1) = s.invariants.map(resolver.coreAPIs.check(_, BoolType, ctx)).unzip
      assert(side1.flatten.isEmpty, "Side effects are not allowed in loop invariants")
      val (loopBody, loopCtx) = checkBlock(bd, ctx, methodInfo, loopLevel + 1)
      (List(While(cond, invariants, loopBody)), ctx.copy(stack = loopCtx.stack))

    case s@untpd.Break() =>
      if loopLevel > 0 then
        (List(Break()(s.range)), ctx)
      else
        reporter.report(ctx.uri, BreakOutOfLoop(s.range))
        (Nil, ctx)

    case s@untpd.Continue() =>
      if loopLevel > 0 then
        (List(Continue()(s.range)), ctx)
      else
        reporter.report(ctx.uri, ContinueOutOfLoop(s.range))
        (Nil, ctx)

  private def matchTarget(tg: untpd.Expr, value: Expr, typ: Type,
                          localCtx: TypingCtx): (List[Stmt], TypingCtx) = tg match
    case untpd.TermName("_") =>
      (Nil, localCtx)
    case untpd.TermName(x) =>
      localCtx.lookup(x) match
        case Some(VarInfo(_, varType)) => // assign to existing variable
          if !typ.toSort.subsortOf(varType.toSort) then
            reporter.report(localCtx.uri, TypeMismatch(tg.range, varType.toSort.show, typ.toSort.show))
          (List(Assign(x, value)), localCtx)
        case Some(_) => // not assignable
          reporter.report(localCtx.uri, NotAssignable(tg.range))
          (Nil, localCtx)
        case None => // declare a new variable
          val info = VarInfo(untpd.Ident(x)(tg.range), typ)
          (List(Assign(x, value)), localCtx.declare(x, info))
    case untpd.TupleExpr(tgs) =>
      typ match
        case TupleType(ts) if ts.length == tgs.length =>
          var ctx = localCtx
          val body = ListBuffer.empty[Stmt]
          for i <- ts.indices do
            val (stmts, newCtx) = matchTarget(tgs(i), TupleSelect(i, value)(noRange), ts(i), ctx)
            body ++= stmts
            ctx = newCtx
          (body.toList, ctx)
        case _ =>
          reporter.report(localCtx.uri, PatternTypeMismatch(tg.range, s"${typ.show}", s"${tgs.length}-tuple"))
          (Nil, localCtx)
    case _ =>
      throw IllegalArgumentException(s"Unexpected assignment target: $tg")
