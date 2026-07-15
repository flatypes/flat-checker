package flat.checker.typing

import flat.checker.Reporter
import flat.checker.flan.*
import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

class Checker(using reporter: Reporter):
  private val typer = Typer()

  def checkProgram(tree: untpd.Program): Program =
    var ctx = GlobalCtx()
    for node <- tree.body do
      val info = resolveInfo(node, ctx)
      ctx.lookup(node.ident.name) match
        case None =>
          ctx = ctx.define(node.ident.name, info)
        case Some(conflict) =>
          reporter.reportNameRedefined(node.ident.range, conflict.range)
    val body = tree.body.flatMap:
      case node: untpd.MethodDef => Some(checkMethod(node, ctx))
      case _ => None
    Program(body)

  private def resolveInfo(node: untpd.TopDef, ctx: GlobalCtx): Info = node match
    case untpd.TypeDef(id, t) =>
      val value = typer.normalize(t)(using ctx)
      TypeInfo(value)(id.range)

    case untpd.ConstDef(id, e) =>
      val (sort, value) = typer.infer(e)(using ctx, VarStore())
      ConstInfo(sort, value)(id.range)

    case untpd.MethodDef(id, ps, rt, _, _, _) =>
      val params = typer.inferParamList(ps)(using ctx)
      val returnType: NormType = rt match
        case Some(t) => typer.normalize(t)(using ctx)
        case None => unitSort
      MethodInfo(params, returnType)(id.range)

  private def checkMethod(node: untpd.MethodDef, globalCtx: GlobalCtx): MethodDef =
    val name = node.ident.name
    var ctx = LocalCtx(globalCtx, name)
    val info = globalCtx.lookup(name).get.asInstanceOf[MethodInfo]
    val vs = VarStore()
    // load parameters and check preconditions
    for (p, d) <- node.params zip info.params do
      val index = vs.add(d.name, d.typ)
      ctx = ctx.define(d.name, VarInfo(d.typ, index)(p.ident.range))
    val requires = node.requires.map(typer.check(_, BoolSort)(using ctx, vs))
    // load return variable and check postconditions
    val index = vs.add("_", info.returnType)
    ctx = ctx.define("_", VarInfo(info.returnType, index)(node.ident.range))
    val ensures = node.ensures.map(typer.check(_, BoolSort)(using ctx, vs))
    // check body
    val body = node.body.map(checkBlock(_, ctx)(using vs))
    val locals = vs.toList.drop(info.params.length + 1) // exclude parameters and return variable
    MethodDef(name, info.params, VarDecl("_", info.returnType), requires, ensures, locals, body)(
      node.endRange)

  private def declareVar(ident: untpd.Ident, normType: NormType, ctx: LocalCtx)(using vs: VarStore): LocalCtx =
    ctx.get(ident.name) match
      case None =>
        val index = vs.add(ident.name, normType)
        ctx.define(ident.name, VarInfo(normType, index)(ident.range))
      case Some(conflict) =>
        reporter.reportNameRedefined(ident.range, conflict.range)
        ctx

  private def checkBlock(block: List[untpd.Stmt], blockCtx: LocalCtx)(using vs: VarStore): List[Stmt] =
    var ctx = blockCtx
    val body = ListBuffer.empty[Stmt]
    block.foreach:
      case untpd.VarStmt(id, Some(t), rhs) =>
        val typ = typer.normalize(t)(using ctx)
        val init = rhs match
          case e: untpd.Expr => Some(typer.check(e, typ.sort)(using ctx))
          case _ => None
        ctx.get(id.name) match
          case None =>
            val index = vs.add(id.name, typ)
            ctx = ctx.define(id.name, VarInfo(typ, index)(id.range))
            for value <- init do
              body += Assign(vs.getName(index), value)
          case Some(conflict) =>
            reporter.reportNameRedefined(id.range, conflict.range)

      case untpd.VarStmt(id, None, e: untpd.Expr) =>
        val (sort, value) = typer.infer(e)(using ctx)
        val index = vs.add(id.name, sort)
        ctx.get(id.name) match
          case None =>
            val index = vs.add(id.name, sort)
            ctx = ctx.define(id.name, VarInfo(sort, index)(id.range))
            body += Assign(vs.getName(index), value)
          case Some(conflict) =>
            reporter.reportNameRedefined(id.range, conflict.range)
      case untpd.VarStmt(id, None, _) =>
        reporter.reportMissingTypeAnnot(id.range)

      case untpd.Assign(id, rhs) =>
        ctx.lookup(id.name) match
          case Some(VarInfo(typ, index)) =>
            rhs match
              case e: untpd.Expr =>
                val value = typer.check(e, typ.sort)(using ctx)
                body += Assign(vs.getName(index), value)
              case nd: untpd.Nondet =>
                body += Havoc(vs.getName(index))
          case result =>
            rhs match
              case e: untpd.Expr => typer.infer(e)(using ctx)
              case nd: untpd.Nondet =>
            result match
              case Some(_) =>
                reporter.reportNotAssignable(id.range)
              case None =>
                reporter.reportNameUndefined(id.range)

      case untpd.ExprStmt(e) =>
        val (_, value) = typer.infer(e)(using ctx)
        body += ExprStmt(value)

      case ret@untpd.Return(rhs) =>
        (ctx.info.returnSort, rhs) match
          case (`unitSort`, None) =>
          case (`unitSort`, Some(e)) =>
            typer.infer(e)(using ctx)
          // no assignment as this return value is discarded
          case (s, Some(e)) =>
            val value = typer.check(e, s)(using ctx)
            body += Assign("_", value)
          case (_, None) =>
            reporter.reportMissingReturnValue(ret.range)
        body += Return()(ret.range)

      case untpd.If(g, b1, b2) =>
        val cond = checkGuard(g, ctx)
        val thenBody = checkBlock(b1, narrow(cond, ctx).push)
        val elseBody = checkBlock(b2, ctx.push)
        body += If(cond, thenBody, elseBody)

      case untpd.While(g, is, b) =>
        val cond = checkGuard(g, ctx)
        val invariants = is.map(typer.check(_, BoolSort)(using ctx))
        val loopBody = checkBlock(b, narrow(cond, ctx).enterLoop)
        body += While(cond, invariants, loopBody)

      case node: untpd.Break =>
        if ctx.inLoop then
          body += Break()(node.range)
        else
          reporter.reportBreakOutOfLoop(node.range)
      case node: untpd.Continue =>
        if ctx.inLoop then
          body += Continue()(node.range)
        else
          reporter.reportContinueOutOfLoop(node.range)

      case untpd.Assume(e) =>
        val expr = typer.check(e, BoolSort)(using ctx)
        body += Assume(expr)
      case untpd.Assert(e) =>
        val expr = typer.check(e, BoolSort)(using ctx)
        body += Assert(expr)

    body.toList

  private def checkGuard(guard: untpd.Expr | untpd.Nondet, ctx: LocalCtx)(using va: VarStore): Expr = guard match
    case e: untpd.Expr =>
      typer.check(e, BoolSort)(using ctx)
    case nd: untpd.Nondet =>
      val index = va.add("*", BoolSort)
      Var(va.getName(index))(nd.range)

  private def narrow(cond: Expr, ctx: LocalCtx): LocalCtx = cond match
    case Ne(Var(x), Const(null)) =>
      ctx.lookup(x) match
        case Some(VarInfo(NormType(sort, reft), _)) =>
          ctx.updateType(x, NormType(sortMinusNull(sort), reft))
        case _ => ctx
    case _ => ctx

  private def sortMinusNull(sort: Sort): Sort = sort match
    case UnionSort(NullSort, s) => s
    case UnionSort(s, NullSort) => s
    case UnionSort(s1, s2) => UnionSort(sortMinusNull(s1), sortMinusNull(s2))
    case _ => sort