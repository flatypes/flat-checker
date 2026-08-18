package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.untpd
import flat.checker.parsing.Parser
import flat.checker.{Reporter, Source}

import scala.collection.mutable

class Resolver(using reporter: Reporter) extends LazyLogging:
  private val typer = Typer()

  def resolve(module: untpd.Program): Ctx =
    var ctx = Ctx()
    // Check imports
    for
      stmt <- module.imports
      moduleCtx <- load(stmt.module.name)
      item <- stmt.items
    do
      moduleCtx.lookup(item.name) match
        case Some(info) =>
          ctx = ctx.define(item.name, info)
        case None =>
          reporter.reportNameUndefined(item.range)
    // Check definitions
    for node <- module.body do
      val info = resolveInfo(node, ctx)
      ctx.lookup(node.ident.name) match
        case None =>
          ctx = ctx.define(node.ident.name, info)
        case Some(conflict) =>
          reporter.reportNameRedefined(node.ident.range, conflict.range)
    ctx

  private val cache = mutable.Map.empty[String, Ctx]

  private def load(moduleName: String): Option[Ctx] =
    cache.get(moduleName) match
      case Some(ctx) => Some(ctx)
      case None =>
        val path = reporter.source.uri.stripPrefix("file://")
        val basePath = path.substring(0, path.lastIndexOf('/') + 1)
        val moduleSource = Source.fromPath(os.Path(s"$basePath$moduleName.flan"))
        val parser = Parser()
        val ctx = resolve(parser.parse(moduleSource))
        cache(moduleName) = ctx
        Some(ctx)

  private def resolveInfo(node: untpd.TopDef, ctx: Ctx): Info = node match
    case untpd.TypeDef(id, t) =>
      val value = typer.normalize(t, ctx)
      TypeInfo(value)(id.range)

    case untpd.LangDef(id, l) =>
      val regEx = typer.translate(l, ctx)
      regEx.name = id.name
      LangInfo(regEx)(id.range)

    case untpd.ConstDef(id, e) =>
      val (value, sort) = typer.infer(e, ctx)
      ConstInfo(sort, value)(id.range)

    case untpd.MethodDef(id, ps1, ps2, _, _, _) =>
      val params = typer.inferParamList(ps1, ctx)
      val returnParams = typer.inferParamList(ps2, ctx)
      MethodInfo(params, returnParams)(id.range)
