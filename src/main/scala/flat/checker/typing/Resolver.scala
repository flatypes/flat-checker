package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.tpd.*
import flat.checker.flan.untpd
import flat.checker.parsing.Parser
import flat.checker.{Reporter, Source}

import scala.collection.mutable

class Resolver(using reporter: Reporter) extends LazyLogging:
  def resolve(module: untpd.Module): TypingCtx =
    val ctx0 = TypingCtx(module.uri)
    module.body.foldLeft(ctx0) { (ctx, stmt) => check(stmt, ctx) }

  val coreAPIs = new CoreAPIs

  def check(stmt: untpd.TopStmt, ctx: TypingCtx): TypingCtx = stmt match
    case untpd.Import(mod, ids) =>
      loadModule(mod.name) match
        case Some(mCtx) => ids.foldLeft(ctx) { (to, id) => doImport(id, mCtx, to) }
        case None =>
          reporter.report(ctx.uri, UndefinedName(mod.range, mod.name))
          ctx

    case untpd.TypeAlias(id, t) =>
      val typ = coreAPIs.unfold(t, ctx)
      val info = TypeInfo(id, typ)
      coreAPIs.define(id, info, ctx)

    case untpd.ValDef(id, e) =>
      val (typ, value, side) = coreAPIs.infer(e, ctx)
      assert(side.isEmpty, "Side effects are not allowed in top-level val definitions")
      val info = ConstInfo(id, typ, value)
      coreAPIs.define(id, info, ctx)

    case untpd.LangDef(id, e) =>
      val regEx = coreAPIs.expand(e, ctx)
      regEx.name = id.name
      val info = LangInfo(id, regEx)
      coreAPIs.define(id, info, ctx)

    case untpd.FunDef(id, ps, rt, pre, post, _) =>
      val (params, localCtx) = coreAPIs.resolveParams(ps, Nil, ctx.pushed)
      val returnType = rt.map(coreAPIs.unfold(_, localCtx)).getOrElse(unitType)
      val (requires, side1) = pre.map(coreAPIs.check(_, BoolType, localCtx)).unzip
      assert(side1.forall(_.isEmpty), "Side effects are not allowed in preconditions")
      val ensures = post.map(checkEnsureSpec(_, returnType, localCtx))
      val info = MethodInfo(id, params, returnType, requires, ensures, localCtx)
      coreAPIs.define(id, info, ctx)

  private def checkEnsureSpec(node: untpd.Expr, returnType: Type, ctx: TypingCtx): Expr =
    val expectedType = (node, returnType) match
      case (untpd.LambdaExpr(ps, _), TupleType(ts)) if ts.length >= 2 && ps.length >= 2 => FunType(ts, BoolType)
      case _ => FunType(List(returnType), BoolType)
    val (cond, side) = coreAPIs.check(node, expectedType, ctx)
    assert(side.isEmpty, "Side effects are not allowed in ensures clauses")
    cond

  private val cache = mutable.Map.empty[String, TypingCtx]

  private def loadModule(name: String): Option[TypingCtx] =
    cache.get(name) match
      case Some(ctx) => Some(ctx)
      case None =>
        val path = reporter.source.uri.stripPrefix("file://")
        val basePath = path.substring(0, path.lastIndexOf('/') + 1)
        val moduleSource = Source.fromPath(os.Path(s"$basePath$name.flan"))
        val parser = Parser(moduleSource.uri, moduleSource.text)
        val ctx = resolve(parser.parse())
        cache(name) = ctx
        Some(ctx)

  private def doImport(id: untpd.Ident, from: TypingCtx, to: TypingCtx): TypingCtx =
    from.get(id.name) match
      case Some(info) => coreAPIs.define(id, info, to)
      case None =>
        reporter.report(to.uri, UndefinedName(id.range, id.name))
        to
