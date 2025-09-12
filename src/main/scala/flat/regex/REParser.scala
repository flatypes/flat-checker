package flat.regex

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

object REParser extends Parsers:
  type Elem = Char

  private def getInput: Parser[Input] = in => Success(in, in)

  private def err(msg: String, nxt: Input): Parser[Nothing] = _ => Error(msg, nxt)

  private def not(special: String): Parser[Char] = acceptIf(c => !special.contains(c))(_ => "expect char literal")

  given Conversion[String, Parser[Char]] = _.map(accept).reduce(_ | _)

  private def escapeSeq: Parser[Char] = '\\' ~>
    ('b' ^^^ '\b' | 't' ^^^ '\t' | 'n' ^^^ '\n' | 'f' ^^^ '\f' | 'r' ^^^ '\r' | "\"'\\" | ".^$*+?[-]|(){,}"
      | err("bad escape"))

  private def allChar: Parser[RegEx] = '.' ^^^ RegEx.allChar

  private def charRange: Parser[CharSet] =
    for
      c1 <- not("]")
      c2 <- '-' ~> not("]")
      if c1 <= c2
    yield CharSet.from(c1 to c2)

  private def charSet: Parser[RegEx] =
    for
      _ <- '['
      neg <- '^' ^^^ true | success(false)
      choices <- (charRange | (not("]") | escapeSeq) ^^ (CharSet.of(_))).*
      _ <- ']'
      cs = if choices.isEmpty then CharSet.empty else choices.reduce(_ | _)
    yield RegEx.RELit(if neg then !cs else cs)

  private def base: Parser[RegEx] =
    allChar | (not(".^$*+?\\[]|(){") | escapeSeq) ^^ RegEx.fromChar | charSet | '(' ~> expr <~ ')'

  private def fin: Parser[Int] = rep1("0123456789") ^^ (cs => cs.mkString.toInt)

  private def loopQuantifier: Parser[(Int, Int | Inf.type)] =
    for
      in <- getInput
      lb <- '{' ~> (fin | success(0))
      ub <- ',' ~> (fin | success(Inf)) <~ '}'
    yield (lb, ub)

  private def repeat: Parser[RegEx] =
    base <~ '*' ^^ { r => r.* } | base <~ '+' ^^ { r => r.+ } | base <~ '?' ^^ { r => r.? } |
      base ~ ('{' ~> fin <~ '}') ^^ { case r ~ k => r ^ k } |
      base ~ loopQuantifier ^^ { case r ~ (l, u) => r.loop(Interval(l, u)) } | base

  private def concat: Parser[RegEx] = (repeat | '}' ^^ RegEx.fromChar).* ^^ RegEx.concat

  private def expr: Parser[RegEx] = repsep(concat, '|') ^^ RegEx.union

  private def formatError(source: CharSequence, offset: Int, msg: String): String =
    val pos = OffsetPosition(source, offset)
    val indentation = " ".repeat(pos.column - 1)
    "regular expression has invalid syntax:\n" + pos.lineContents + "\n" + indentation + "^\n" + indentation + msg

  def tryParse(input: CharSequence): Either[String, RegEx] =
    val in = CharSequenceReader(input)
    phrase(expr)(in) match
      case Success(r, _) => Right(r)
      case Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
      case Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))

  def parse(input: CharSequence): RegEx = tryParse(input) match
    case Left(err) => throw IllegalArgumentException(err)
    case Right(r) => r