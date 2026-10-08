package flat.checker.parsing

import scala.util.parsing.combinator.Parsers

trait ExprParsers extends Parsers:
  def prefix[E](op: Parser[E => E], term: Parser[E]): Parser[E] =
    op.* ~ term ^^ { case l ~ e => l.foldRight(e) { case (f, e1) => f(e1) } }

  def postfix[E](term: Parser[E], op: Parser[E => E]): Parser[E] =
    term ~ op.* ^^ { case e ~ l => l.foldLeft(e) { case (e1, f) => f(e1) } }

  def infixL[E](op: Parser[(E, E) => E], term: Parser[E]): Parser[E] =
    term ~ (op ~ term).* ^^ { case e ~ l => l.foldLeft(e) { case (e1, f ~ e2) => f(e1, e2) } }

  def infixR[E](op: Parser[(E, E) => E], term: Parser[E]): Parser[E] =
    term ~ (op ~ term).* ^^ { case e ~ l =>
      val es = e :: l.map(_._2)
      (es.init zip l.map(_._1)).foldRight(es.last) { case ((e1, f), e2) => f(e1, e2) }
    }

  def infixNonAssoc[E](op: Parser[(E, E) => E], term: Parser[E]): Parser[E] =
    term ~ (op ~ term).? ^^ { case e ~ None => e; case e1 ~ Some(f ~ e2) => f(e1, e2) }
