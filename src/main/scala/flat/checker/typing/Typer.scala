package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.*
import flat.checker.flan.Show.show
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
    Module(tree.uri, body)

  private def checkMethod(node: untpd.FunDef, globalCtx: TypingCtx): MethodDef =
    val name = node.ident.name
    val info = globalCtx.get(name).get.asInstanceOf[MethodInfo]
    val body = checkBlock(node.body, globalCtx.copy(stack = info.scope :: globalCtx.stack), info, 0)
    MethodDef(name, info.params, info.returnType, info.requires, info.ensures, body)

  private def checkBlock(nodes: List[untpd.Stmt], localCtx: TypingCtx,
                         methodInfo: MethodInfo, loopLevel: Int): List[Stmt] =
    var ctx = localCtx
    val body = ListBuffer.empty[Stmt]
    for node <- nodes do
      val (stmts, newCtx) = checkStmt(node, ctx, methodInfo, loopLevel)
      ctx = newCtx
      body ++= stmts
    body.toList

  private def checkStmt(node: untpd.Stmt, ctx: TypingCtx,
                        methodInfo: MethodInfo, loopLevel: Int): (List[Stmt], TypingCtx) = node match
    case untpd.Pass() => (Nil, ctx)
    case untpd.ExprStmt(e) =>
      val (typ, expr, side) = resolver.coreAPIs.infer(e, ctx)
      (side :+ ExprStmt(expr), ctx)

    case untpd.VarDecl(p, e@untpd.Apply(untpd.TermName(m), es))
      if patternIsVarList(p) && ctx.lookup(m).exists(_.isInstanceOf[MethodInfo]) =>
      val info = ctx.lookup(m).get.asInstanceOf[MethodInfo]
      val (args, argsSide) = resolver.coreAPIs.checkArgs(es, info.paramTypes, e.range, ctx)
      val ids: List[untpd.Ident] = p match
        case untpd.PatternVar(id, None) => List(id)
        case untpd.TuplePattern(ps) => ps.map:
          case untpd.PatternVar(id, None) => id
          case _ => assert(false)
        case _ => assert(false)
      val call = MethodCall(ids.map(_.name), m, args)(e.range)
      val newCtx = ids match
        case List(id) =>
          resolver.coreAPIs.define(id, VarInfo(id, info.returnType), ctx)
        case _ =>
          info.returnType match
            case TupleType(ts) if ts.length == ids.length =>
              resolver.coreAPIs.define(for (id, t) <- ids.zip(ts) yield id -> VarInfo(id, t), ctx)
            case t =>
              reporter.report(ctx.uri, PatternTypeMismatch(p.range, s"${t.show}", s"${ids.length}-tuple"))
              ctx
      (argsSide ++ List(call), newCtx)

    case untpd.VarDecl(untpd.PatternVar(id, Some(t)), e) if id.name != "_" =>
      val typ = resolver.coreAPIs.unfold(t, ctx)
      val (value, side) = resolver.coreAPIs.check(e, typ, ctx)
      val info = VarInfo(id, typ)
      (side ++ List(Declare(id.name, typ, Some(value))), resolver.coreAPIs.define(id, info, ctx))
    case untpd.VarDecl(p, e) =>
      val (typ, value, side) = resolver.coreAPIs.infer(e, ctx)
      val (stmts, newCtx) = checkVarDecl(p, value, typ, ctx)
      (side ++ stmts, newCtx)

    case untpd.Assign(id, e) =>
      ctx.lookup(id.name) match
        case Some(VarInfo(_, typ)) =>
          val (value, side) = resolver.coreAPIs.check(e, typ, ctx)
          (side :+ Assign(id.name, value), ctx)
        case Some(_) =>
          reporter.report(ctx.uri, NotAssignable(id.range))
          (Nil, ctx)
        case None =>
          reporter.report(ctx.uri, UndefinedName(id.range, id.name))
          (Nil, ctx)

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
      val thenBody = checkBlock(bd1, ctx.pushed.assume(cond), methodInfo, loopLevel)
      val elseBody = checkBlock(bd2, ctx.pushed.assume(Not(cond)), methodInfo, loopLevel)
      (side :+ If(cond, thenBody, elseBody), ctx)

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
      val loopBody = checkBlock(bd, ctx.pushed, methodInfo, loopLevel + 1)
      (List(While(cond, invariants, loopBody)), ctx)

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

  private def checkVarDecl(pat: untpd.Pattern, value: Expr, typ: Type,
                           localCtx: TypingCtx): (List[Stmt], TypingCtx) = pat match
    case untpd.PatternVar(untpd.Ident("_"), None) =>
      (Nil, localCtx)
    case untpd.PatternVar(id, None) =>
      val info = VarInfo(id, typ)
      val newCtx = resolver.coreAPIs.define(id, info, localCtx)
      (List(Declare(id.name, typ, Some(value))), newCtx)
    case untpd.TuplePattern(ps) =>
      typ match
        case TupleType(ts) if ts.length == ps.length =>
          var ctx = localCtx
          val body = ListBuffer.empty[Stmt]
          for i <- ts.indices do
            val e = TupleSelect(i, value)(noRange)
            val (side, newCtx) = checkVarDecl(ps(i), e, ts(i), ctx)
            ctx = newCtx
            body ++= side
          (body.toList, ctx)
        case t =>
          reporter.report(localCtx.uri, PatternTypeMismatch(pat.range, s"${t.show}", s"${ps.length}-tuple"))
          (Nil, localCtx)
    case _ =>
      throw IllegalStateException(s"Unexpected pattern: $pat")

  private def patternIsVarList(pat: untpd.Pattern): Boolean = pat match
    case untpd.PatternVar(_, None) => true
    case untpd.TuplePattern(ps) =>
      ps.forall:
        case untpd.PatternVar(_, None) => true
        case _ => false
    case _ => false