package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.untpd
import flat.checker.parsing.Parser
import flat.checker.{Reporter, Source}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Resolver(using reporter: Reporter) extends LazyLogging:
  private val typer = Typer(ListBuffer.empty)

  def resolve(module: untpd.Module): Ctx =
    var ctx = Ctx()
    module.body.foreach:
      case untpd.Import(mod, ids) =>
        for
          moduleCtx <- load(mod.name)
          id <- ids
        do
          moduleCtx.lookup(id.name) match
            case Some(info) =>
              ctx = ctx.define(id.name, info)
            case None =>
              reporter.reportNameUndefined(id.range)

      case untpd.TypeAlias(id, t) =>
        val value = typer.normalize(t, ctx)
        ctx = define(ctx, id, TypeInfo(value)(id.range))

      case untpd.ValDef(id, Some(t), e) =>
        val typ = typer.normalize(t, ctx)
        val value = typer.check(e, typ, ctx)
        ctx = define(ctx, id, ConstInfo(typ, value)(id.range))
      case untpd.ValDef(id, None, e) =>
        val (value, sort) = typer.infer(e, ctx)
        ctx = define(ctx, id, ConstInfo(sort, value)(id.range))

      case untpd.LangDef(id, e) =>
        val regEx = typer.translate(e, ctx)
        regEx.name = id.name
        ctx = define(ctx, id, LangInfo(regEx)(id.range))

      case untpd.FunDef(id, ps1, t, _) =>
        val params = typer.inferParamList(ps1, ctx)
        val returnParams = t match
          case Some(rt) =>
            val returnType = typer.normalize(rt, ctx)
            List((untpd.Ident("return")(rt.range), returnType))
          case None =>
            Nil
        ctx = define(ctx, id, MethodInfo(params, returnParams)(id.range))

    ctx

  private val cache = mutable.Map.empty[String, Ctx]

  private def load(moduleName: String): Option[Ctx] =
    cache.get(moduleName) match
      case Some(ctx) => Some(ctx)
      case None =>
        val path = reporter.source.uri.stripPrefix("file://")
        val basePath = path.substring(0, path.lastIndexOf('/') + 1)
        val moduleSource = Source.fromPath(os.Path(s"$basePath$moduleName.flan"))
        val parser = Parser(moduleSource.uri, moduleSource.text)
        val ctx = resolve(parser.parse())
        cache(moduleName) = ctx
        Some(ctx)

  private def define(ctx: Ctx, id: untpd.Ident, info: Info): Ctx =
    ctx.lookup(id.name) match
      case None =>
        ctx.define(id.name, info)
      case Some(conflict) =>
        reporter.reportNameRedefined(id.range, conflict.range)
        ctx
