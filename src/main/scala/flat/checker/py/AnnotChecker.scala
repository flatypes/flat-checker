package flat.checker.py

import flat.checker.*
import flat.checker.py.ast.*
import flat.regex.{REParser, RegEx}
import flat.{Issuer, Location}

class AnnotChecker(using issuer: Issuer):
  def checkAnnot(annot: Expr, ctx: GCtx): ast.Type = annot.accept(Visitor, ctx)

  private object Visitor extends NodeVisitor[GCtx, ast.Type]:
    private def resolve(node: Name, ctx: GCtx): ast.Type | String =
      val x = node.id
      ctx.get(x) match
        case Some(TypeInfo(t, _)) => t
        case Some(_) =>
          issuer.report(TypeError("expect a type", node.loc))
          ast.NoType
        case None =>
          x match
            case "int" => ast.IntSort
            case "bool" => ast.BoolSort
            case "str" => ast.StrSort
            case "Char" => ast.charType
            case "Callable" => "Callable"
            case "tuple" | "Tuple" => "Tuple"
            case "list" | "List" => "List"
            case "Literal" => "Literal"
            case "range" => "range"
            case "lang" => "lang"
            case _ =>
              issuer.report(Undefined(node.asIdent))
              ast.NoType

    override def visitName(node: Name, ctx: GCtx): ast.Type =
      resolve(node, ctx) match
        case t: ast.Type => t
        case constr: String =>
          issuer.report(TypeError(s"missing type argument for type constructor $constr", node.loc))
          ast.NoType

    override def visitSubscript(node: Subscript, ctx: GCtx): ast.Type =
      node.value match
        case name: Name =>
          resolve(name, ctx) match
            case "Callable" =>
              node.index match
                case TupleExpr(Seq(first, second)) =>
                  val ts = first match
                    case ListExpr(args) => for arg <- args yield arg.accept(this, ctx)
                    case arg => Seq(arg.accept(this, ctx))
                  val t = second.accept(this, ctx)
                  ast.FunType(ts.toList, t)
                case _ =>
                  issuer.report(TypeError(
                    "invalid arguments for typing.Callable\n" +
                      "expect an input type list and an output type: Callable[[input, ...], output]", node.index.loc))
                  ast.NoType
            case "Tuple" =>
              node.index match
                case TupleExpr(args) if args.length != 1 =>
                  val ts = for arg <- args yield arg.accept(this, ctx)
                  ast.TupleType(ts.toList)
                case _ =>
                  issuer.report(TypeError(
                    "invalid arguments for typing.Tuple\n" +
                      "expect a type list: Tuple[t1, t2, ...]", node.index.loc))
                  ast.NoType
            case "List" =>
              node.index match
                case e: Expr =>
                  val t = e.accept(this, ctx)
                  ast.ListType(t)
                case _ =>
                  issuer.report(TypeError(
                    "invalid argument for typing.List\n" + "expect a type", node.index.loc))
                  ast.NoType
            case "Literal" =>
              node.index match
                case Constant(s: String) => ast.literalType(s)
                case _ =>
                  issuer.report(TypeError(
                    "invalid argument for typing.Literal\n" + "expect a constant string", node.index.loc))
                  ast.NoType
            case "lang" =>
              node.index match
                case c@Constant(s: String) =>
                  ast.LangType(parseReExpr(s, c.loc, ctx))
                case _ =>
                  issuer.report(TypeError("invalid argument for flat.py.lang\n" +
                    "expect a string (that compiles to a regular expression)", node.index.loc))
                  ast.NoType
            case qualifiedName: String =>
              throw NotImplementedError(s"type constructor '$qualifiedName'")
            case t: ast.Type =>
              issuer.report(TypeError(s"extra type argument: ${t.show} does not take type arguments", node.index.loc))
              t
        case _ =>
          issuer.report(TypeError("expect a type constructor", node.value.loc))
          ast.NoType

    private def matchesRangeBound(arg: Option[Expr]): Boolean =
      arg match
        case Some(Constant(_: Int)) | None => true
        case _ => false

    override def visitCall(node: Call, ctx: GCtx): ast.Type =
      node.func match
        case name: Name =>
          resolve(name, ctx) match
            case "lang" =>
              node.args match
                case Seq(c@Constant(s: String)) =>
                  ast.LangType(parseReExpr(s, c.loc, ctx))
                case Seq(arg) =>
                  issuer.report(TypeError("invalid argument for flat.py.lang\n" +
                    "expect a string (that compiles to a regular expression)", arg.loc))
                  ast.NoType
                case _ =>
                  issuer.report(TypeError("type constructor flat.py.lang takes exactly one argument", node.loc))
                  ast.NoType
            case _: String =>
              issuer.report(TypeError("expect a function", node.func.loc))
              ast.NoType
            case t: ast.Type =>
              issuer.report(TypeError("cannot apply type arguments using '()', use '[]' instead", node.loc))
              t
        case _ =>
          issuer.report(TypeError("expect a type", node.loc))
          ast.NoType

    override def visitDefault(node: Node, ctx: GCtx): ast.Type =
      issuer.report(TypeError("expect a type", node.loc))
      ast.NoType

    private def parseReExpr(input: CharSequence, loc: Location, ctx: GCtx): RegEx =
      val rules = Map.from(ctx.collect { case (x, TypeInfo(ast.LangType(r), _)) => x -> r })
      REParser.tryParse(input, rules) match
        case Left(detail) =>
          issuer.report(SyntaxError(detail, loc))
          RegEx.RENone
        case Right(r) => r
