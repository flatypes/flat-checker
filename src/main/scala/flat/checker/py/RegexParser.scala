package flat.checker.py

import flat.checker.Bound.{Fin, PosInf}
import flat.checker.{Bound, CharSet, ReLang}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition}

object RegexParser extends Parsers:
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
    "regular expression has invalid syntax:\n" + pos.lineContents + "\n" + indentation + "^\n" + indentation + msg

  def apply(input: CharSequence): Either[String, ReLang] =
    val in = CharSequenceReader(input)
    phrase(expr)(in) match
      case Success(r, _) => Right(r)
      case Failure(msg, nxt) => Left(formatError(input, nxt.offset, msg))
      case Error(msg, nxt) => Left(formatError(input, nxt.offset, msg))
