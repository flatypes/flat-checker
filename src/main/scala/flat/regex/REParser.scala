package flat.regex

import com.ibm.icu.text.UnicodeSet

import scala.jdk.CollectionConverters.*
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

object REParser extends Parsers:
  type Elem = Char

  def parse(input: CharSequence): RegEx = tryParse(input) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r

  def tryParse(input: CharSequence): Either[String, RegEx] =
    val in = CharSequenceReader(input)
    phrase(expr)(in) match
      case Success(r, _) => Right(r)
      case Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
      case Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))

  private def formatError(source: CharSequence, offset: Int, msg: String): String =
    val pos = OffsetPosition(source, offset)
    val indentation = " ".repeat(pos.column - 1)
    "regular expression has invalid syntax:\n" + pos.lineContents + "\n" + indentation + "^\n" + indentation + msg

  // expr -> alt ('|' alt)*
  private def expr: Parser[RegEx] = rep1sep(alt, '|') ^^ RegEx.union

  // alt -> term*
  private def alt: Parser[RegEx] = term.* ^^ RegEx.concat

  // term -> atom quantifier?
  private def term: Parser[RegEx] = atom >> parseQuantifier

  // quantifier -> '*' | '+' | '?' | '{' (interval | int) '}'
  private def parseQuantifier(r: RegEx): Parser[RegEx] =
    '*' ^^^ r.* | '+' ^^^ r.+ | '?' ^^^ r.? | '{' ~>! (interval ^^ r.loop | int ^^ (r ^ _)) <~ '}' | success(r)

  // interval -> int? ',' int?
  private inline def interval: Parser[Interval] =
    (int | success(0)) ~ (',' ~> (int | success(Inf))) ^^ { case lb ~ ub => Interval(lb, ub) }

  // Reserved characters in regex syntax
  private val syntaxChars: String = "^$\\.*+?()[]{}|"

  // atom -> char (EXCEPT syntaxChars) | '\' (charEscape | classEscape) | '.' | '[' classContents ']' | '(' expr ')'
  private def atom: Parser[RegEx] =
    acceptMatch("character", { case c if !syntaxChars.contains(c) => RegEx.fromChar(c) }) |
      '\\' ~>! (charEscape ^^ RegEx.fromChar | classEscape ^^ RegEx.fromCharSet) |
      '.' ^^^ RegEx.allChar | '[' ~>! classContents <~ ']' | '(' ~>! expr <~ ')'

  // classContents -> '^'? classContent*
  private def classContents: Parser[RegEx] =
    for
      neg <- '^'.?
      css <- classContent.*
      cs = if css.isEmpty then CharSet.empty else css.reduce(_ | _)
    yield RegEx.fromCharSet(if neg.isDefined then !cs else cs)

  // classContent -> classChar ('-' classChar)? | classEscape
  private def classContent: Parser[CharSet] =
    classChar ~ ('-' ~> classChar).? >> {
      case c ~ None => success(CharSet(c))
      case c1 ~ Some(c2) =>
        if c1 <= c2 then success(CharSet.from(c1 to c2))
        else err("invalid character range")
    } | '\\' ~> classEscape

  // classChar -> char (EXCEPT '\' '[' ']' '-') | '\' charEscape
  private def classChar: Parser[Char] =
    acceptMatch("character", { case c if !"\\[]-".contains(c) => c }) | '\\' ~> charEscape

  // charEscape -> controlEscape | identityEscape | 'c' letter | 'x' hexDigit{2} | 'u' hexDigit{4}
  private def charEscape: Parser[Char] =
    'f' ^^^ '\f' | 'n' ^^^ '\n' | 'r' ^^^ '\r' | 't' ^^^ '\t' | 'v' ^^^ 11.toChar |
      acceptMatch("identity escape", { case c if (syntaxChars + "-'\"/").contains(c) => c }) |
      'c' ~>! letter ^^ { c => (c % 32).toChar } |
      'x' ~>! repN(2, hexDigit) ^^ { cs => Integer.parseInt(cs.mkString, 16).toChar } |
      'u' ~>! repN(4, hexDigit) ^^ { cs => Integer.parseInt(cs.mkString, 16).toChar }

  // classEscape -> 'd' | 'D' | 's' | 'S' | 'w' | 'W' | 'p' '{' propertyValueExpr '}'
  private def classEscape: Parser[CharSet] =
    'd' ^^^ CharSet.asciiDigit | 'D' ^^^ !CharSet.asciiDigit |
      's' ^^^ CharSet.asciiSpace | 'S' ^^^ !CharSet.asciiSpace |
      'w' ^^^ CharSet.asciiWord | 'W' ^^^ !CharSet.asciiWord |
      'p' ~>! '{' ~> acceptIf(_ != '}')(_ => "").+ <~ '}' >> { cs => parsePropertyValueExpr(cs.mkString) }

  private def parsePropertyValueExpr(input: String): Parser[CharSet] =
    try
      val set = UnicodeSet(s"\\p{$input}")
      success(CharSet.from(set.codePoints().asScala.map(_.toChar)))
    catch
      case ex: IllegalArgumentException => err(s"invalid Unicode escape: ${ex.getMessage}")

  private def int: Parser[Int] = rep1(acceptMatch("decimal digit",
    { case c if (c >= '0' && c <= '9') => c })) ^^ (_.mkString.toInt)

  private def hexDigit: Parser[Char] = acceptMatch("hexadecimal digit",
    { case c if (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f') => c })

  private def letter: Parser[Char] = acceptMatch("ASCII letter",
    { case c if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') => c })