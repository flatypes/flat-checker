package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.*
import flat.checker.flan.TypeOps.{:<:, erase}
import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

class Checker(using reporter: Reporter) extends LazyLogging:
  private val resolver = Resolver()

  def checkProgram(module: untpd.Program): List[Script] =
    val ctx = resolver.resolve(module)
    module.body.flatMap:
      case node: untpd.MethodDef => Some(checkMethod(node, ctx))
      case _ => None

  private def checkMethod(node: untpd.MethodDef, globalCtx: Ctx): Script =
    val name = node.ident.name
    var ctx = globalCtx.push()
    val info = globalCtx.lookup(name).get.asInstanceOf[MethodInfo]
    val locals = ListBuffer.empty[VarDecl]
    val body = ListBuffer.empty[Stmt]
    val typer = Typer(body)
    // load parameters and check preconditions
    for (x, paramInfo) <- info.paramInfos do
      ctx = ctx.define(x, paramInfo)
      locals += VarDecl(x, paramInfo.typ.erase)
    val requires = node.requires.map(typer.check(_, BoolType, ctx))
    body += Assume(mkAnd(requires))
    // load return variables and check postconditions
    for (x, paramInfo) <- info.returnInfos do
      ctx = ctx.define(x, paramInfo)
      locals += VarDecl(x, paramInfo.typ.erase)
    val ensures = node.ensures.map(typer.check(_, BoolType, ctx))
    // check body
    for block <- node.body do
      val (lcs, ss) = checkBlock(block, ctx, info)
      locals ++= lcs
      body ++= ss
    Script(locals.toList, body.toList)

  private def checkBlock(node: List[untpd.Stmt], ctx: Ctx, info: MethodInfo): (List[VarDecl], List[Stmt]) =
    val visitor = BlockVisitor(ctx, info)
    node.foreach(visitor.check)
    (visitor.locals.toList, visitor.body.toList)

  private class BlockVisitor(localCtx: Ctx, info: MethodInfo):
    val locals: ListBuffer[VarDecl] = ListBuffer.empty[VarDecl]
    val body: ListBuffer[Stmt] = ListBuffer.empty[Stmt]
    val typer = Typer(body)
    private var ctx = localCtx

    def check(stmt: untpd.Stmt): Unit = stmt match
      // Assignments
      case untpd.VarDecl(id, t) =>
        val typ = typer.normalize(t, ctx)
        declareVar(id, typ)
      case untpd.Assign(x, e) =>
        assign(x, e)
      case untpd.ExprStmt(e) =>
        val (value, _) = typer.infer(e, ctx)
        body += ExprStmt(value)
      // Proof derivatives
      case untpd.Assume(e) =>
        val expr = typer.check(e, BoolType, ctx)
        body += Assume(expr)
      case untpd.Assert(e) =>
        val expr = typer.check(e, BoolType, ctx)
        body += Assert(expr)
      case untpd.Abort(e) =>
        val (expr, _) = typer.infer(e, ctx)
        body += Abort()(e.range)
      // Conditional
      case untpd.If(e: untpd.Expr, List(untpd.Abort(_)), Nil) =>
        val cond = typer.check(e, BoolType, ctx)
        body += Assert(Not(cond))
      case untpd.If(g, b1, b2) =>
        val cond = checkGuard(g)
        val (thenLocals, thenBody) = checkBlock(b1, typer.assume(cond, ctx.push()), info)
        val (elseLocals, elseBody) = checkBlock(b2, typer.assume(Not(cond), ctx.push()), info)
        locals ++= thenLocals
        locals ++= elseLocals
        body += If(cond, thenBody, elseBody)

      // Loops
      case untpd.While(g, is, b) =>
        val cond = checkGuard(g)
        val invariants = is.map(typer.check(_, BoolType, ctx))
        val (loopLocals, loopBody) = checkBlock(b, typer.assume(cond, ctx.push(isLoop = true)), info)
        locals ++= loopLocals
        body += While(cond, mkAnd(invariants), loopBody)

      case untpd.For(id, e, is, b) =>
        val (iter, iterSort) = typer.infer(e, ctx)
        iterSort match
          case ListType(elemSort) =>
            val loopCtx = ctx.define(id.name, VarInfo(elemSort)(id.range)).push(isLoop = true)
            val invariants = is.map(typer.check(_, BoolType, loopCtx))
            val (loopLocals, loopBody) = checkBlock(b, loopCtx, info)
            locals ++= loopLocals
            body += For(id.name, iter, mkAnd(invariants), loopBody)
          case _ =>
            reporter.reportTypeMismatch(e.range, "list", iterSort)

      // Jumps
      case ret@untpd.Return(None) =>
        body += Return()(ret.range)
      case ret@untpd.Return(Some(e)) =>
        val value = typer.check(e, info.returnType.erase, ctx)
        info.returnParams match
          case List((id, _)) =>
            body += Assign(id.name, value)
          case ps =>
            for (id, i) <- ps.map(_._1).zipWithIndex do
              body += Assign(id.name, TupleSelect(i, value)(e.range))
        body += Return()(ret.range)

      case node: untpd.Break =>
        if ctx.insideLoop then
          body += Break()(node.range)
        else
          reporter.reportBreakOutOfLoop(node.range)
      case node: untpd.Continue =>
        if ctx.insideLoop then
          body += Continue()(node.range)
        else
          reporter.reportContinueOutOfLoop(node.range)

    private def declareVar(ident: untpd.Ident, typ: Type): Unit =
      ctx.getDefined(ident.name) match
        case None =>
          locals += VarDecl(ident.name, typ)
          ctx = ctx.define(ident.name, VarInfo(typ)(ident.range))
        case Some(conflict) =>
          reporter.reportNameRedefined(ident.range, conflict.range)

    private def assign(target: untpd.Target, expr: untpd.Expr): Unit = target match
      case untpd.TargetName(x) =>
        ctx.lookup(x) match
          case Some(VarInfo(typ)) =>
            val value = typer.check(expr, typ, ctx)
            body += Assign(x, value)
          case Some(_) =>
            reporter.reportNotAssignable(target.range)
          case None =>
            reporter.reportNameUndefined(target.range)

      case untpd.ValTarget(id, _) =>
        throw UnsupportedOperationException("val target is not supported yet")

      case untpd.VarTarget(id, Some(t)) =>
        val typ = typer.normalize(t, ctx)
        val value = typer.check(expr, typ, ctx)
        declareVar(id, typ)
        body += Assign(id.name, value)
      case untpd.VarTarget(_, None) =>
        val (value, typ) = typer.infer(expr, ctx)
        assign(target, value, typ)

      case untpd.TupleTarget(xs) =>
        expr match
          case untpd.TupleExpr(es) if es.length == xs.length =>
            for (x, e) <- xs zip es do
              assign(x, e)
          case _ =>
            val (value, typ) = typer.infer(expr, ctx)
            assign(target, value, typ)

      case untpd.ListTarget(_) =>
        val (value, typ) = typer.infer(expr, ctx)
        assign(target, value, typ)

    private def assign(target: untpd.Target, value: Expr, valueType: Type): Unit = target match
      case untpd.TargetName(x) =>
        ctx.lookup(x) match
          case Some(VarInfo(t)) =>
            if valueType.erase :<: t.erase then
              body += Assign(x, value)
            else
              reporter.reportTypeMismatch(target.range, t.erase, valueType)
          case Some(_) =>
            reporter.reportNotAssignable(target.range)
          case None =>
            reporter.reportNameUndefined(target.range)

      case untpd.ValTarget(id, _) =>
        throw UnsupportedOperationException("val target is not supported yet")

      case untpd.VarTarget(id, Some(t)) =>
        val typ = typer.normalize(t, ctx)
        if valueType.erase :<: typ.erase then
          declareVar(id, typ)
          body += Assign(id.name, value)
        else
          reporter.reportTypeMismatch(target.range, typ.erase, valueType)
      case untpd.VarTarget(id, None) =>
        declareVar(id, valueType)
        body += Assign(id.name, value)

      case untpd.TupleTarget(xs) => valueType match
        case TupleType(ts) if ts.length == xs.length =>
          val (fresh, ctx1) = ctx.defineFreshVal(valueType)
          locals += VarDecl(fresh, valueType)
          ctx = ctx1
          body += Assign(fresh, value)
          val tuple = Var(fresh)(value.range)
          for (x, i) <- xs.zipWithIndex do
            val e = TupleSelect(i, tuple)(value.range)
            assign(x, e, ts(i))
        case _ =>
          reporter.reportTypeMismatch(value.range, s"${xs.length}-tuple", valueType)

      case untpd.ListTarget(xs) => valueType match
        case ListType(s) =>
          val (fresh, ctx1) = ctx.defineFreshVal(valueType)
          locals += VarDecl(fresh, valueType)
          ctx = ctx1
          body += Assign(fresh, value)
          val list = Var(fresh)(value.range)
          for (x, i) <- xs.zipWithIndex do
            val e = ListAt(list, IntLit(i)(value.range))(value.range)
            assign(x, e, s)
        case _ =>
          reporter.reportTypeMismatch(value.range, "list", valueType)

    private def checkGuard(guard: untpd.Expr | untpd.Nondet): Expr = guard match
      case e: untpd.Expr =>
        typer.check(e, BoolType, ctx)
      case nd: untpd.Nondet =>
        throw UnsupportedOperationException("nondet guard will be dropped")
