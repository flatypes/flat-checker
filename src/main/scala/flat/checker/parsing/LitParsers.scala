package flat.checker.parsing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.untpd.*
import flat.checker.parsing.PosUtils.{*, given}
import org.eclipse.lsp4j.{Position, Range as Span}

import scala.util.parsing.combinator.RegexParsers

class LitParsers(uri: String, start: Position)(using reporter: Reporter) extends RegexParsers, ExprParsers, LazyLogging:
  override def skipWhitespace: Boolean = false

  private def int: Parser[BigInt] =
    decInt | """0[xX][0-9a-fA-F]+""".r ^^ (s => BigInt(s.drop(2), 16))
      | """0[bB][01]+""".r ^^ (s => BigInt(s.drop(2), 2))

  private def decInt: Parser[BigInt] = """[0-9]+""".r ^^ (BigInt(_))

  def parseIntLit(input: String): BigInt =
    parseAll(int, input) match
      case Success(n, _) => n
      case _ => throw new IllegalArgumentException(s"invalid integer literal: $input")

  private def char: Parser[Char] =
    elem("source char", _ != '\\') | '\\' ~> (controlEscape | unicodeEscape | identityEscape)

  private def controlEscape: Parser[Char] =
    'f' ^^^ '\f' | 'n' ^^^ '\n' | 'r' ^^^ '\r' | 't' ^^^ '\t' | 'v' ^^^ '\u000B'

  private def unicodeEscape: Parser[Char] = """u[0-9a-fA-F]{4}""".r ^^ { s => Integer.parseInt(s.drop(1), 16).toChar }

  private def identityEscape: Parser[Char] = elem('\\') | '\'' | '\"'

  def parseCharLit(input: String): Char =
    require(input.startsWith("'") && input.endsWith("'"))
    parseAll(char, input.drop(1).dropRight(1)) match
      case Success(c, _) => c
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(start + 1 + next.offset, msg))
        '\u0000'

  def parseStrLit(input: String): String =
    require(input.startsWith("\"") && input.endsWith("\""))
    parseAll(char.*, input.drop(1).dropRight(1)) match
      case Success(cs, _) => cs.mkString
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(start + 1 + next.offset, msg))
        ""

  private def rExpr: Parser[PExpr] = infixRight('|' ^^^ (PUnion(_, _)), rConcat)

  private def rConcat: Parser[PExpr] = infixRight(success(PConcat(_, _)), rTerm) | success(PStr(""))

  private def rTerm: Parser[PExpr] = postfix(rChar | rAllChar | rCharSet | rParen, quantifier)

  private val reserved: String = "\\^$.*+?()[]{}|"

  private def rChar: Parser[PChar] = (elem("source char", !reserved.contains(_)) | rCharEscape) ^^ (PChar(_))

  private def rCharEscape: Parser[Char] = '\\' ~> (controlEscape | unicodeEscape | rIdentityEscape)

  private def rIdentityEscape: Parser[Char] = elem("identity escape", (reserved + "-'\"").contains)

  private def rAllChar: Parser[PAllChar.type] = '.' ^^^ PAllChar

  private def rCharSet: Parser[PCharSet] =
    '[' ~> '^'.? ~ rCharSetItem.* <~ ']' ^^ { case neg ~ items => PCharSet(neg.isEmpty, items) }

  private def rCharSetItem: Parser[Char | CharRange] =
    withSpan(rCharSetChar ~ ('-' ~> rCharSetChar).?) ^^ {
      case (c ~ None, _) => c
      case (c1 ~ Some(c2), span) => CharRange(c1, c2)(span)
    }

  private def rCharSetChar: Parser[Char] = elem("character", !"\\[]-".contains(_)) | rCharEscape

  private def rParen: Parser[PExpr] = '(' ~> rExpr <~ ')'

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

  def parseRStrLit(input: String): PExpr =
    require(input.startsWith("r\"") && input.endsWith("\""))
    parseAll(rExpr, input.drop(2).dropRight(1)) match
      case Success(e, _) => e
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(start + 2 + next.offset, msg))
        PAllChar
