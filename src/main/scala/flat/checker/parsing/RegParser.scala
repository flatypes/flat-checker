package flat.checker.parsing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.untpd.*
import flat.checker.parsing.PosUtils.{*, given}
import org.eclipse.lsp4j.{Position, Range as Span}

import scala.util.parsing.combinator.RegexParsers

class RegParser(uri: String, start: Position)(using reporter: Reporter) extends RegexParsers, ExprParsers, LazyLogging:
  override def skipWhitespace: Boolean = false

  private def decInt: Parser[BigInt] = """[0-9]+""".r ^^ (BigInt(_))

  private def expr: Parser[PExpr] = infixR('|' ^^^ (PUnion(_, _)), concat)

  private def concat: Parser[PExpr] = infixR(success(PConcat(_, _)), term) | success(PStr(""))

  private def term: Parser[PExpr] = postfix(rChar | allChar | charSet | paren, quantifier)

  private val reserved: String = "\\^$.*+?()[]{}|"

  private def rChar: Parser[PChar] = (elem("source char", !reserved.contains(_)) | rCharEscape) ^^ (PChar(_))

  private def rCharEscape: Parser[Char] = '\\' ~> (controlEscape | unicodeEscape | identityEscape)

  private def controlEscape: Parser[Char] =
    'f' ^^^ '\f' | 'n' ^^^ '\n' | 'r' ^^^ '\r' | 't' ^^^ '\t' | 'v' ^^^ '\u000B'

  private def unicodeEscape: Parser[Char] = """u[0-9a-fA-F]{4}""".r ^^ { s => Integer.parseInt(s.drop(1), 16).toChar }

  private def identityEscape: Parser[Char] = elem("identity escape", (reserved + "-'\"").contains)

  private def allChar: Parser[PAllChar.type] = '.' ^^^ PAllChar

  private def charSet: Parser[PCharSet] =
    '[' ~> '^'.? ~ charSetItem.* <~ ']' ^^ { case neg ~ items => PCharSet(neg.isEmpty, items) }

  private def charSetItem: Parser[Char | CharRange] =
    withSpan(charSetChar ~ ('-' ~> charSetChar).?) ^^ {
      case (c ~ None, _) => c
      case (c1 ~ Some(c2), span) => CharRange(c1, c2)(span)
    }

  private def charSetChar: Parser[Char] = elem("character", !"\\[]-".contains(_)) | rCharEscape

  private def paren: Parser[PExpr] = '(' ~> expr <~ ')'

  private def quantifier: Parser[PExpr => PExpr] =
    '*' ^^^ (PStar(_)) | '+' ^^^ (PPlus(_)) | '?' ^^^ (POpt(_)) | times ^^ { times => PRep(_, times) }

  private def times: Parser[BigInt | IntRange] =
    withSpan('{' ~> decInt ~ (',' ~> decInt).? <~ '}') ^^ {
      case (i ~ None, _) => i
      case (i1 ~ Some(i2), span) => IntRange(i1, Some(i2))(span)
    }

  private def withSpan[T](p: => Parser[T]): Parser[(T, Span)] = in =>
    p(in) match
      case Success(result, next) => Success((result, Span(in.pos, next.pos)), next)
      case ns: NoSuccess => ns

  def parse(input: String): PExpr =
    parseAll(expr, input) match
      case Success(e, _) => e
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(start + 2 + next.offset, msg))
        PAllChar
