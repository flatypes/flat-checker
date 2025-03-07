package flat.checker.frontend.python

import flat.checker.Bound.*
import flat.checker.frontend.python.ast.*
import flat.checker.{Bound, CharSet, Interval, Issuer, Location, ReLang, SyntaxError, TypeError, ast}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

trait AnnotChecker:
  val issuer: Issuer

  def checkAnnot(annot: Expr, ctx: GlobalContext): ast.Type = annot.accept(Checker, ctx)

  import GItem.*

  private object Checker extends NodeVisitor[GlobalContext, ast.Type]:
    private def resolve(node: Name, ctx: GlobalContext): ast.Type | TypeConstr =
      val x = node.id
      ctx.get(x) match
        case Some(Type(t)) => t
        case Some(c@TypeConstr(_)) => c
        case Some(_) =>
          issuer.report(TypeError(node.loc, Seq("expect a type")))
          ast.NoType
        case None =>
          x match
            case "int" => ast.intType
            case "bool" => ast.boolType
            case "str" => ast.stringType
            case "list" => TypeConstr("typing.List")
            case _ =>
              issuer.report(UndefinedName(node.loc))
              ast.NoType

    override def visitName(node: Name, ctx: GlobalContext): ast.Type =
      resolve(node, ctx) match
        case t: ast.Type => t
        case c: TypeConstr =>
          issuer.report(TypeError(node.loc,
            Seq(s"missing type argument for type constructor ${c.qualifiedName}")))
          ast.NoType

    override def visitSubscript(node: Subscript, ctx: GlobalContext): ast.Type =
      node.value match
        case name: Name =>
          resolve(name, ctx) match
            case c: TypeConstr =>
              c.qualifiedName match
                case "typing.Callable" =>
                  node.slice match
                    case TupleExpr(Seq(first, second)) =>
                      val ts = first match
                        case ListExpr(args) => for arg <- args yield arg.accept(this, ctx)
                        case arg => Seq(arg.accept(this, ctx))
                      val t = second.accept(this, ctx)
                      ast.FunType(ts, t)
                    case _ =>
                      issuer.report(TypeError(node.slice.loc, Seq(
                        "invalid arguments for typing.Callable",
                        "expect an input type list and an output type: Callable[[input, ...], output]"
                      )))
                      ast.NoType
                case "typing.List" =>
                  val t = node.slice.accept(this, ctx)
                  ast.ArrayType(t)
                case "typing.Literal" =>
                  node.slice match
                    case Constant(v) => ast.literalType(v)
                    case _ =>
                      issuer.report(TypeError(node.slice.loc, Seq(
                        "invalid argument for typing.Literal",
                        "expect a constant"
                      )))
                      ast.NoType
                case "flat.py.range" =>
                  node.slice match
                    case Slice(lower, upper, None) if matchesRangeBound(lower) && matchesRangeBound(upper) =>
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
                      issuer.report(TypeError(node.slice.loc, Seq(
                        "invalid argument for flat.py.range",
                        "expect a range of two constant integers: range[lb:ub]"
                      )))
                      ast.NoType
                case "flat.py.lang" =>
                  node.slice match
                    case c@Constant(s: String) =>
                      ast.LangType(parseReExpr(s, c.loc))
                    case _ =>
                      issuer.report(TypeError(node.slice.loc, Seq(
                        "invalid argument for flat.py.lang",
                        "expect a string (that compiles to a regular expression)"
                      )))
                      ast.NoType
                case _ =>
                  throw NotImplementedError(s"type constructor '${c.qualifiedName}'")
            case t: ast.Type =>
              issuer.report(TypeError(node.slice.loc,
                Seq(s"extra type argument: ${t.show} does not take type arguments")))
              t
        case _ =>
          issuer.report(TypeError(node.value.loc, Seq("expect a type constructor")))
          ast.NoType

    private def matchesRangeBound(arg: Option[Expr]): Boolean =
      arg match
        case Some(Constant(_: Int)) | None => true
        case _ => false

    override def visitCall(node: Call, ctx: GlobalContext): ast.Type =
      node.func match
        case name: Name =>
          resolve(name, ctx) match
            case c: TypeConstr =>
              c.qualifiedName match
                case "flat.py.lang" =>
                  node.args match
                    case Seq(c@Constant(s: String)) =>
                      ast.LangType(parseReExpr(s, c.loc))
                    case Seq(arg) =>
                      issuer.report(TypeError(arg.loc, Seq(
                        "invalid argument for flat.py.lang",
                        "expect a string (that compiles to a regular expression)"
                      )))
                      ast.NoType
                    case _ =>
                      issuer.report(TypeError(node.loc,
                        Seq("type constructor flat.py.lang takes exactly one argument")))
                      ast.NoType
                case _ =>
                  issuer.report(TypeError(node.func.loc, Seq("expect a function")))
                  ast.NoType
            case t: ast.Type =>
              issuer.report(TypeError(node.loc, Seq("cannot apply type arguments using '()', use '[]' instead")))
              t
        case _ =>
          issuer.report(TypeError(node.loc, Seq("expect a type")))
          ast.NoType

    override def visitDefault(node: Node, ctx: GlobalContext): ast.Type =
      issuer.report(SyntaxError(node.loc, Seq("expect a type")))
      ast.NoType

  def parseReExpr(input: CharSequence, location: Location): ReLang =
    ReExprParser(input) match
      case Left(details) =>
        issuer.report(SyntaxError(location, "regular expression has invalid syntax" +: details))
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

    private def formatError(source: CharSequence, offset: Int, msg: String): Seq[String] =
      val pos = OffsetPosition(source, offset)
      val indentation = " ".repeat(pos.column - 1)
      Seq(pos.lineContents, indentation + "^", indentation + msg)

    def apply(input: CharSequence): Either[Seq[String], ReLang] =
      val in = CharSequenceReader(input)
      phrase(expr)(in) match
        case Success(r, _) => Right(r)
        case Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
        case Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))
