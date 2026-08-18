package flat.checker.parsing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.parsing.PosUtils.{*, given}
import org.eclipse.lsp4j.{Position, Range as Span}

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

enum TokenType:
  case INT, CHAR, STR, R_STR, KEYWORD, IDENTIFIER, INDENT, DEDENT, NEWLINE

import flat.checker.parsing.TokenType.*

final case class Token(typ: TokenType, value: String, span: Span)

val operators: List[String] = List(
  ",", ":", "=", "->", "?",
  "!", "-", "~",
  "+", "*", "/", "%", "==", "!=", "<=", "<", ">=", ">",
  "&&", "||", "==>", "&", "|", "^", "<<", ">>", ".",
  "+=", "-=", "*=", "/=", "%="
).sortBy(_.length).reverse

val keywords: List[String] = List(
  "from", "import", "type", "val", "lang", "def", "requires", "ensures",
  "true", "false", "null", "in",
  "pass", "var", "assume", "assert", "abort", "if", "else", "return",
  "while", "invariant", "break", "continue", "for"
)

val tabSize = 4

class Lexer(uri: String, text: String)(using reporter: Reporter) extends RegexParsers, LazyLogging:
  private var level = 0
  private val openingParens = mutable.Stack.empty[Token]

  override def skipWhitespace: Boolean = false

  private def file: Parser[List[Token]] = (emptyLine | nonEmptyLine).* ^^ processFile

  private def emptyLine: Parser[List[Token]] = whitespace ~> "\n" ^^^ Nil

  private val ws1: Parser[String] = """([ \f\t]|#[^\n]*|\\\n)*""".r

  private val ws2: Parser[String] = """([ \f\t\n]|#[^\n]*|\\\n)*""".r

  private def whitespace: Parser[String] = in => if openingParens.isEmpty then ws1(in) else ws2(in)

  private def nonEmptyLine: Parser[List[Token]] = elem(' ').* ~> (token <~ whitespace).+ <~ "\n" ^^ processLine

  private def token: Parser[Token] =
    int | char | str | rStr | openingParen | closingParen | operator | keywordOrIdentifier

  private def int: Parser[Token] =
    withSpan("""[1-9](_?[0-9])*|0[bB](_?[01])+|0[oO](_?[0-7])+|0[xX](_?[0-9a-fA-F])+|0""".r) ^^ (Token(INT, _, _))

  private def char: Parser[Token] =
    withSpan(Regex("'" + """([^\n\\']|""" + escapeSeq + ")'")) ^^ (Token(CHAR, _, _))

  private val escapeSeq = """\\([\\'"abfnrtv]|u[0-9a-fA-F]{4})"""

  private def str: Parser[Token] =
    withSpan(Regex("\"" + """([^\n\\"]|""" + escapeSeq + ")*\"")) ^^ (Token(STR, _, _))

  private def rStr: Parser[Token] =
    withSpan(Regex("r\"" + """([^\n\\"]|\\[^\n])*""" + "\"")) ^^ (Token(R_STR, _, _))

  private def openingParen: Parser[Token] =
    withSpan(elem('(') | '[' | '{')
      ^^ { (c, span) => val tk = Token(KEYWORD, c.toString, span); openingParens.push(tk); tk }

  private def closingParen: Parser[Token] =
    withSpan(elem(')') | ']' | '}')
      ^^ { (c, span) => val tk = Token(KEYWORD, c.toString, span); matchParen(tk); tk }

  private def matchParen(close: Token): Unit =
    if openingParens.nonEmpty then
      val openingParen = openingParens.top.value
      val expected = openingParen match
        case "(" => ")"
        case "[" => "]"
        case "{" => "}"
        case _ => throw IllegalArgumentException(s"invalid opening parenthesis: $openingParen")
      if expected == close.value then
        openingParens.pop()
      else
        reporter.report(uri, ParenMismatch(close.span, close.value, openingParen))
    else
      reporter.report(uri, UnmatchedParen(close.span, close.value))

  private def operator: Parser[Token] = withSpan(operators.map(literal).reduce(_ | _)) ^^ (Token(KEYWORD, _, _))

  private def keywordOrIdentifier: Parser[Token] =
    withSpan("""[a-zA-Z_][a-zA-Z0-9_]*""".r)
      ^^ { (s, span) => Token(if keywords.contains(s) then KEYWORD else IDENTIFIER, s, span) }

  private def processLine(line: List[Token]): List[Token] =
    require(line.nonEmpty, "line must be non-empty")
    val start = line.head.span.getStart
    val spaces = start.getCharacter
    if spaces % tabSize != 0 then
      reporter.report(uri, BadIndent(start, spaces))
    val newLevel = spaces / tabSize
    val indentation =
      if newLevel > level then List.fill(newLevel - level)(Token(INDENT, "", start))
      else if newLevel < level then List.fill(level - newLevel)(Token(DEDENT, "", start))
      else Nil
    level = newLevel
    indentation ++ line ++ List(Token(NEWLINE, "", line.last.span.getEnd))

  private def processFile(lines: List[List[Token]]): List[Token] =
    val tokens = lines.flatten
    if tokens.isEmpty then
      return Nil

    while openingParens.nonEmpty do
      val open = openingParens.pop()
      reporter.report(uri, UnclosedParen(open.span, open.value))
    val end = tokens.last.span.getEnd
    tokens ++ List.fill(level)(Token(DEDENT, "", end))

  private def withSpan[T](p: => Parser[T]): Parser[(T, Span)] = in =>
    p(in) match
      case Success(result, next) => Success((result, Span(in.pos, next.pos)), next)
      case ns: NoSuccess => ns

  def lex(): List[Token] =
    val safeText = if text.endsWith("\n") then text else text + "\n"
    parseAll(file, safeText) match
      case Success(tks, _) => tks
      case NoSuccess(msg, next) =>
        val pos: Position = next.pos
        reporter.report(uri, SyntaxError(pos, msg))
        Nil
