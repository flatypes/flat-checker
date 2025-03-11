package flat.checker.py

import flat.checker.Bound.{Fin, NegInf, PosInf}
import flat.checker.py.ast.*
import flat.checker.{Bound, CharSet, Interval, Issuer, Location, ReLang, ast}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

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
            case "int" => ast.intType
            case "bool" => ast.boolType
            case "str" => ast.stringType
            case "Callable" => "Callable"
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
          issuer.report(TypeError(s"missing type argument for type constructor ${constr}", node.loc))
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
                  ast.FunType(ts, t)
                case _ =>
                  issuer.report(TypeError(
                    "invalid arguments for typing.Callable\n" +
                      "expect an input type list and an output type: Callable[[input, ...], output]", node.index.loc))
                  ast.NoType
            case "List" =>
              node.index match
                case e: Expr =>
                  val t = e.accept(this, ctx)
                  ast.ArrayType(t)
                case _ =>
                  issuer.report(TypeError(
                    "invalid argument for typing.List\n" + "expect a type", node.index.loc))
                  ast.NoType
            case "Literal" =>
              node.index match
                case Constant(v) => ast.literalType(v)
                case _ =>
                  issuer.report(TypeError(
                    "invalid argument for typing.Literal\n" + "expect a constant", node.index.loc))
                  ast.NoType
            case "range" =>
              node.index match
                case Slice(lower, upper) if matchesRangeBound(lower) && matchesRangeBound(upper) =>
                  val lb: Bound = lower match
                    case None => NegInf
                    case Some(Constant(n: Int)) => n
                    case _ => assert(false)
                  val ub: Bound = upper match
                    case None => PosInf
                    case Some(Constant(n: Int)) => n
                    case _ => assert(false)
                  ast.IntervalType(Interval(lb, ub))
                case _ =>
                  issuer.report(TypeError(
                    "invalid argument for flat.py.range\n" + "expect a range of two constant integers: range[lb:ub]",
                    node.index.loc))
                  ast.NoType
            case "lang" =>
              node.index match
                case c@Constant(s: String) =>
                  ast.LangType(parseReExpr(s, c.loc))
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
                  ast.LangType(parseReExpr(s, c.loc))
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

    def parseReExpr(input: CharSequence, location: Location): ReLang =
      ReExprParser(input) match
        case Left(details) =>
          issuer.report(SyntaxError("regular expression has invalid syntax" + details, location))
          ReLang.empty
        case Right(r) => r

    private object ReExprParser extends Parsers:
      type Elem = Char

      private def getInput: Parser[Input] = in => Success(in, in)

      private def err(msg: String, nxt: Input): Parser[Nothing] = _ => Error(msg, nxt)

      private def not(special: String): Parser[Char] = acceptIf(c => !special.contains(c))(_ => "expect char literal")

      given Conversion[String, Parser[Char]] = _.map(accept).reduce(_ | _)

      private def escapeSeq: Parser[Char] = '\\' ~>
        ('b' ^^^ '\b' | 't' ^^^ '\t' | 'n' ^^^ '\n' | 'f' ^^^ '\f' | 'r' ^^^ '\r' | "\"'\\" | ".^$*+?[-]|(){,}"
          | err("bad escape"))

      private def allChar: Parser[ReLang] = '.' ^^^ ReLang.allChar

      private def charRange: Parser[CharSet] =
        for
          c1 <- not("]")
          c2 <- '-' ~> not("]")
          if c1 <= c2
        yield CharSet.from(c1 to c2)

      private def charSet: Parser[ReLang] =
        for
          _ <- '['
          neg <- '^' ^^^ true | success(false)
          choices <- (charRange | (not("]") | escapeSeq) ^^ (CharSet.of(_))).*
          _ <- ']'
          cs = if choices.isEmpty then CharSet.empty else choices.reduce(_ | _)
        yield ReLang.ReChars(if neg then !cs else cs)

      private def base: Parser[ReLang] =
        allChar | (not(".^$*+?\\[]|(){") | escapeSeq) ^^ ReLang.fromChar | charSet | '(' ~> expr <~ ')'

      private def fin: Parser[Fin] = rep1("0123456789") ^^ (cs => Fin(cs.mkString.toInt))

      private def loopQuantifier: Parser[(Bound, Bound)] =
        for
          in <- getInput
          lb <- '{' ~> (fin | success(Fin(0)))
          ub <- ',' ~> (fin | success(PosInf)) <~ '}'
          r <- if lb <= ub then success((lb, ub)) else err("empty repetition range", in)
        yield r

      private def quantifier: Parser[(Bound, Bound)] =
        '*' ^^^ (Fin(0), PosInf) | '+' ^^^ (Fin(1), PosInf) | '?' ^^^ (Fin(0), Fin(1))
          | '{' ~> fin <~ '}' ^^ (b => (b, b)) | loopQuantifier

      private def repeat: Parser[ReLang] = base ~ quantifier.? ^^ {
        case r ~ None => r
        case r ~ Some(lb, ub) => r.loop(lb, ub)
      }

      private def concat: Parser[ReLang] = (repeat | '}' ^^ ReLang.fromChar).* ^^ ReLang.mkConcat

      private def expr: Parser[ReLang] = repsep(concat, '|') ^^ ReLang.mkUnion

      private def formatError(source: CharSequence, offset: Int, msg: String): String =
        val pos = OffsetPosition(source, offset)
        val indentation = " ".repeat(pos.column - 1)
        pos.lineContents + "\n" + indentation + "^\n" + indentation + msg

      def apply(input: CharSequence): Either[String, ReLang] =
        val in = CharSequenceReader(input)
        phrase(expr)(in) match
          case Success(r, _) => Right(r)
          case Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
          case Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))

