package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.*
import flat.checker.flan.TypeOps.{:<:, erase}
import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

class Typer(using reporter: Reporter) extends LazyLogging:
  private val resolver = Resolver()

  def typeCheck(module: untpd.Module): List[Script] =
    val ctx = resolver.resolve(module)
    module.body.flatMap:
      case node: untpd.FunDef => Some(checkMethod(node, ctx))
      case _ => None

  private def checkMethod(node: untpd.FunDef, globalCtx: Ctx): Script =
    val name = node.ident.name
    var ctx = globalCtx.push()
    val info = globalCtx.lookup(name).get.asInstanceOf[MethodInfo]
    val locals = ListBuffer.empty[VarDecl]
    val body = ListBuffer.empty[Stmt]
    val typer = API(body)
    // load parameters
    for (x, paramInfo) <- info.paramInfos do
      ctx = ctx.define(x, paramInfo)
      locals += VarDecl(x, paramInfo.typ.erase)
      paramInfo.typ match
        case RefinedType(_, p) =>
          body += Assume(p.subst(Map("_" -> Var(x))))
        case _ =>
    // load return variables
    for (x, paramInfo) <- info.returnInfos do
      ctx = ctx.define(x, paramInfo)
      locals += VarDecl(x, paramInfo.typ.erase)
    // check body
    if node.body.nonEmpty then
      val (lcs, ss) = checkBlock(node.body, ctx, info)
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
    val typer = API(body)
    private var ctx = localCtx

    def check(stmt: untpd.Stmt): Unit = stmt match
      case untpd.Pass() => // skip
      case untpd.ExprStmt(e) =>
        val (value, _) = typer.infer(e, ctx)
        body += ExprStmt(value)

      case untpd.VarStmt(id, Some(t), init) =>
        val typ = typer.normalize(t, ctx)
        declareVar(id, typ)
        for e <- init do
          val value = typer.check(e, typ, ctx)
          body += Assign(id.name, value)
      case untpd.VarStmt(id, None, Some(e)) =>
        val (value, typ) = typer.infer(e, ctx)
        declareVar(id, typ)
        body += Assign(id.name, value)
      case untpd.VarStmt(id, None, None) =>
        reporter.reportMissingTypeAnnot(id.range)

      case untpd.Assign(x, e) => assign(x, e)
      case untpd.AugAssign(x, op, e: untpd.Expr) =>
        assign(untpd.LRef(x.name)(x.range),
          untpd.BinaryExpr(untpd.TermRef(x.name)(x.range), untpd.Ident(op.name.init)(op.range), e))

      case untpd.Assume(e) =>
        val expr = typer.check(e, BoolType, ctx)
        body += Assume(expr)
      case untpd.Assert(e) =>
        val expr = typer.check(e, BoolType, ctx)
        body += Assert(expr)
      case untpd.Abort(e) =>
        typer.check(e, StrType, ctx)
        body += Abort()(e.range)

      case untpd.If(e: untpd.Expr, List(untpd.Abort(em)), Nil) =>
        typer.check(em, StrType, ctx)
        val cond = typer.check(e, BoolType, ctx)
        body += Assert(Not(cond))
      case untpd.If(g, b1, b2) =>
        val cond = checkGuard(g)
        val (thenLocals, thenBody) = checkBlock(b1, typer.assume(cond, ctx.push()), info)
        val (elseLocals, elseBody) = checkBlock(b2, typer.assume(Not(cond), ctx.push()), info)
        locals ++= thenLocals
        locals ++= elseLocals
        body += If(cond, thenBody, elseBody)

      case ret@untpd.Return(None) =>
        body += Return()(ret.range)
      case ret@untpd.Return(Some(e)) =>
        val value = typer.check(e, info.returnType, ctx)
        body += Return()(ret.range)

      case s@untpd.While(g, _, b) =>
        val cond = checkGuard(g)
        val invariants = s.invariants.map(typer.check(_, BoolType, ctx))
        val (loopLocals, loopBody) = checkBlock(b, typer.assume(cond, ctx.push(isLoop = true)), info)
        locals ++= loopLocals
        body += While(cond, mkAnd(invariants), loopBody)

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

      case s@untpd.For(id, e, _, b) =>
        val (iter, iterSort) = typer.infer(e, ctx)
        val elemSort = iterSort match
          case StrType => StrType
          case ListType(elemSort) => elemSort
          case _ =>
            reporter.reportTypeMismatch(e.range, "list", iterSort)
            NoType
        val loopCtx = ctx.define(id.name, VarInfo(elemSort)(id.range)).push(isLoop = true)
        val invariants = s.invariants.map(typer.check(_, BoolType, loopCtx))
        val (loopLocals, loopBody) = checkBlock(b, loopCtx, info)
        locals ++= loopLocals
        body += For(id.name, iter, mkAnd(invariants), loopBody)

    private def declareVar(ident: untpd.Ident, typ: Type): Unit =
      ctx.getDefined(ident.name) match
        case None =>
          locals += VarDecl(ident.name, typ)
          ctx = ctx.define(ident.name, VarInfo(typ)(ident.range))
        case Some(conflict) =>
          reporter.reportNameRedefined(ident.range, conflict.range)

    private def assign(target: untpd.LExpr, expr: untpd.Expr): Unit = target match
      case untpd.LRef("_") => // ignore
      case untpd.LRef(x) =>
        ctx.lookup(x) match
          case Some(VarInfo(typ)) =>
            val value = typer.check(expr, typ, ctx)
            body += Assign(x, value)
          case Some(_) =>
            reporter.reportNotAssignable(target.range)
          case None =>
            reporter.reportNameUndefined(target.range)

      case untpd.LTuple(xs) =>
        expr match
          case untpd.TupleExpr(es) if es.length == xs.length =>
            for (x, e) <- xs zip es do
              assign(x, e)
          case _ =>
            val (value, typ) = typer.infer(expr, ctx)
            assign(target, value, typ)

      case untpd.LList(_) =>
        val (value, typ) = typer.infer(expr, ctx)
        assign(target, value, typ)

    private def assign(target: untpd.LExpr, value: Expr, valueType: Type): Unit = target match
      case untpd.LRef("_") => // ignore
      case untpd.LRef(x) =>
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

      case untpd.LTuple(xs) => valueType match
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

      case untpd.LList(xs) => valueType match
        case ListType(s) =>
          val (fresh, ctx1) = ctx.defineFreshVal(valueType)
          locals += VarDecl(fresh, valueType)
          ctx = ctx1
          body += Assign(fresh, value)
          val list = Var(fresh)(value.range)
          for (x, i) <- xs.zipWithIndex do
            val e = CharAt(list, IntConst(i)(value.range))(value.range)
            assign(x, e, s)
        case _ =>
          reporter.reportTypeMismatch(value.range, "list", valueType)

    private def checkGuard(guard: untpd.Expr): Expr = guard match
      case e: untpd.Expr =>
        typer.check(e, BoolType, ctx)
      case _ => throw IllegalStateException()
