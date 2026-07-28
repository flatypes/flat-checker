package flat.checker.domain

import com.ibm.icu.text.UnicodeSet
import flat.checker.domain.given

import scala.jdk.CollectionConverters.*
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

object REParser:
  def parse(input: CharSequence): StrRE = tryParse(input) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r

  def tryParse(input: CharSequence, rules: Map[String, StrRE] = Map.empty): Either[String, StrRE] =
    val in = CharSequenceReader(input)
    val parsers = StrREParsers(rules)
    parsers.phrase(parsers.expr)(in) match
      case parsers.Success(r, _) => Right(r)
      case parsers.Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
      case parsers.Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))

  private def formatError(source: CharSequence, offset: Int, msg: String): String =
    val pos = OffsetPosition(source, offset)
    val indentation = " ".repeat(pos.column - 1)
    "regular expression has invalid syntax:\n" + pos.lineContents + "\n" + indentation + "^\n" + indentation + msg

  private class StrREParsers(rules: Map[String, StrRE]) extends Parsers:
    type Elem = Char

    // expr -> alt ('|' alt)*
    def expr: Parser[StrRE] = rep1sep(alt, '|') ^^ RegEx.sum

    // alt -> term*
    private def alt: Parser[StrRE] = term.* ^^ RegEx.product

    // term -> atom quantifier?
    private def term: Parser[StrRE] = atom >> parseQuantifier

    // quantifier -> '*' | '+' | '?' | '{' (interval | int) '}'
    private def parseQuantifier(r: StrRE): Parser[StrRE] =
      '*' ^^^ r.star | '+' ^^^ r.plus | '?' ^^^ r.opt | '{' ~> (int ^^ (r ^ _)) <~ '}' | success(r)

    // Reserved characters in regex syntax
    private val syntaxChars: String = "^$\\.*+?()[]{}|"

    // atom -> char (EXCEPT syntaxChars) | '\' (charEscape | classEscape) | '.' | '[' classContents ']' | '(' expr ')'
    //       | '{' rule '}'
    private def atom: Parser[StrRE] =
      acceptMatch("character", { case c if !syntaxChars.contains(c) => RegEx.symbol(c) }) |
        '\\' ~>! (charEscape ^^ RegEx.symbol) |
        '.' ^^^ RegEx.Lit(CharSet.full) | '[' ~>! classContents <~ ']' | '(' ~>! expr <~ ')' |
        '{' ~>! rule <~ '}'

    // classContents -> '^'? classContent*
    private def classContents: Parser[StrRE] =
      for
        neg <- '^'.?
        css <- classContent.*
        cs = if css.isEmpty then CharSet.empty else css.reduce(_ | _)
      yield RegEx.symbolSet(if neg.isDefined then ~cs else cs)

    // classContent -> classChar ('-' classChar)? | classEscape
    private def classContent: Parser[CharSet] =
      classChar ~ ('-' ~> classChar).? >> {
        case c ~ None => success(CharSet(c))
        case c1 ~ Some(c2) =>
          if c1 <= c2 then success(CharSet.from(c1 to c2))
          else err("invalid character range")
      } // | '\\' ~> classEscape

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

//    // classEscape -> 'd' | 'D' | 's' | 'S' | 'w' | 'W' | 'p' '{' propertyValueExpr '}'
//    private def classEscape: Parser[CharSet] =
//      'd' ^^^ CharSet.asciiDigit | 'D' ^^^ !CharSet.asciiDigit |
//        's' ^^^ CharSet.asciiSpace | 'S' ^^^ !CharSet.asciiSpace |
//        'w' ^^^ CharSet.asciiWord | 'W' ^^^ !CharSet.asciiWord |
//        'p' ~>! '{' ~> acceptIf(_ != '}')(_ => "").+ <~ '}' >> { cs => parsePropertyValueExpr(cs.mkString) }

    private def rule: Parser[StrRE] =
      ident >> { name =>
        rules.get(name) match
          case Some(r) => success(r)
          case None => err(s"undefined rule: $name")
      }

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

    private def ident: Parser[String] =
      acceptMatch("identifier start character", { case c if c.isLetter || c == '_' => c }) ~
        rep1(acceptMatch("identifier character", { case c if c.isLetterOrDigit || c == '_' => c })) ^^ {
        case c ~ cs => (c :: cs).mkString
      }
