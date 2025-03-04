package flat.checker.frontend.python

import flat.checker.frontend.python.ast.*
import flat.checker.{Issuer, ast, SyntaxError}

enum GItem:
  case TypeConstr(qualifiedName: String)
  case Type(expansion: ast.Type)
  case Func(args: Seq[(String, ast.Type)], returns: ast.Type)

final class GlobalContext(vars: Map[String, GItem]):
  def contains(name: String): Boolean = vars.contains(name)

  def get(name: String): Option[GItem] = vars.get(name)

  def apply(name: String): GItem = vars(name)

  def updated(name: String, item: GItem): GlobalContext = GlobalContext(vars + (name -> item))

object GlobalContext:
  val empty: GlobalContext = GlobalContext(Map.empty)

class GlobalChecker(override val issuer: Issuer) extends AnnotChecker:
  import GItem.*

  def check(module: Module, ctx: GlobalContext = GlobalContext.empty): ast.Program =
    val newCtx = module.body.foldLeft(ctx) { (c, s) => s.accept(FirstPass, c) }
    val localChecker = LocalChecker(issuer, newCtx)
    val body = module.body.flatMap {
      case FunctionDef(f, _, body, _) =>
        val Func(args, returns) = newCtx(f): @unchecked
        val localCtx = LocalContext(args.toMap, returns)
        val block = localChecker.check(body, localCtx)
        Some(ast.FunDef(ast.Ident(f), args, returns, block))
      case _ => None
    }
    ast.Program(body)

  private object FirstPass extends NodeVisitor[GlobalContext, GlobalContext]:
    override def visitImportFrom(node: ImportFrom, ctx: GlobalContext): GlobalContext =
      node.names.foldLeft(ctx) { (c, a) => fromImport(node, a, c) }

    private def fromImport(node: ImportFrom, alias: Alias, ctx: GlobalContext): GlobalContext =
      val x = alias.asName.getOrElse(alias.name)
      if ctx.contains(x) then
        issuer.report(RedefinedName(node.loc))
        return ctx

      node.module match
        case "typing" =>
          alias.name match
            case "Any" => ctx.updated(x, Type(ast.AnyType))
            case y =>
              if !Set("Callable", "Literal").contains(y) then
                issuer.report(UndefinedName(alias.loc))
              ctx.updated(x, TypeConstr(s"typing.$y"))
        case "flat.py" =>
          val y = alias.name
          if !Set("lang", "range").contains(y) then
            issuer.report(UndefinedName(alias.loc))
          ctx.updated(x, TypeConstr(s"flat.py.$y"))
        case other =>
          issuer.report(UndefinedName(node.loc))
          ctx

    override def visitFunctionDef(node: FunctionDef, ctx: GlobalContext): GlobalContext =
      val f = node.name
      if ctx.contains(f) then
        issuer.report(RedefinedName(node.loc))
        return ctx

      val argNames = node.args.map(_.arg)
      for
        i <- argNames.indices
        x = argNames(i)
        if argNames.take(i - 1).contains(x)
      do issuer.report(RedefinedName(node.args(i).loc))
      val args = for arg <- node.args yield (arg.arg, checkAnnot(arg.annotation, ctx))
      val returns = checkAnnot(node.returns, ctx)
      ctx.updated(node.name, Func(args, returns))

    override def visitTypeAlias(node: TypeAlias, ctx: GlobalContext): GlobalContext =
      node.name match
        case Name(x) =>
          if ctx.contains(x) then
            issuer.report(RedefinedName(node.loc))
            return ctx

          val t = checkAnnot(node.value, ctx)
          ctx.updated(x, Type(t))
        case _ =>
          issuer.report(SyntaxError(node.name.loc, Seq("expect a name")))
          ctx

    override def visitDefault(node: Node, ctx: GlobalContext): GlobalContext =
      issuer.report(SyntaxError(node.loc,
        Seq("unexpected statement: expect type aliases, function definitions, and imports")))
      ctx
