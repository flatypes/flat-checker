package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.TypeOps.erase
import flat.checker.flan.tpd.{BoolType, FunType, TupleType}
import flat.checker.flan.untpd
import flat.checker.parsing.Parser
import flat.checker.{Reporter, Source}

import scala.collection.mutable

class Resolver(using reporter: Reporter) extends LazyLogging:
  private val api = API()

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
        val value = api.normalize(t, ctx)
        ctx = define(ctx, id, TypeInfo(value)(id.range))

      case untpd.ValDef(id, Some(t), e) =>
        val typ = api.normalize(t, ctx)
        val value = api.check(e, typ, ctx)(using null)
        ctx = define(ctx, id, ConstInfo(typ, value)(id.range))
      case untpd.ValDef(id, None, e) =>
        val (value, sort) = api.infer(e, ctx)(using null)
        ctx = define(ctx, id, ConstInfo(sort, value)(id.range))

      case untpd.LangDef(id, e) =>
        val regEx = api.translate(e, ctx)
        regEx.name = id.name
        ctx = define(ctx, id, LangInfo(regEx)(id.range))

      case untpd.FunDef(id, in, out, pre, post, _) =>
        val sw = new ScriptWriter
        val params = api.inferParamList(in, ctx)
        var localCtx = ctx.push()
        for (id, typ) <- params do
          localCtx = define(localCtx, id, ValInfo(typ)(id.range))
          sw.declare(id.name, typ.erase)
        val returnType = out.map(api.normalize(_, ctx)).getOrElse(TupleType(Nil))
        val requires = pre.map(api.check(_, BoolType, localCtx)(using sw))
        val ensures = post.map(api.check(_, FunType(List(returnType), BoolType), localCtx)(using sw))
        ctx = define(ctx, id, MethodInfo(params, returnType, requires, ensures, sw, localCtx)(id.range))

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
