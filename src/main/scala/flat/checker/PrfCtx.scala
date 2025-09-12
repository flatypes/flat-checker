package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.RegEx

class PrfCtx private(hypotheses: List[Expr])(using val types: Types) extends LazyLogging:

  import RegEx.RENone
  import Rewriter.*

  def assumptions: List[Expr] = hypotheses

  def contains(cond: Expr): Boolean = hypotheses.contains(cond)

  def exists(p: Expr => Boolean): Boolean = hypotheses.exists(p)

  def exists(pf: PartialFunction[Expr, Boolean]): Boolean = exists(x => pf.lift(x).getOrElse(false))

  def collectFirst[T](pf: PartialFunction[Expr, T]): Option[T] = hypotheses.collectFirst(pf)

  def collect[T](pf: PartialFunction[Expr, T]): List[T] = hypotheses.collect(pf)

  def foreach(f: Expr => Unit): Unit = hypotheses.foreach(f)

  def destruct(cond: Expr): (PrfCtx, PrfCtx) =
    val ctx1 = PrfCtx(hypotheses.map(_.transform { case Ite(c, e, _) if c == cond => e }) :+ cond)
    val not = destructAnd(simplifyCond(Not(cond)))
    val ctx2 = PrfCtx(hypotheses.map(_.transform { case Ite(c, _, e) if c == cond => e }) ++ not)
    (ctx1, ctx2)

  def tryDestruct: Option[(PrfCtx, PrfCtx)] = hypotheses.zipWithIndex.collectFirst {
    case (Or(b1, b2), i) =>
      val es1 = hypotheses.take(i)
      val es2 = hypotheses.drop(i + 1)
      val ctx1 = PrfCtx(es1 ++ destructAnd(b1) ++ es2)
      val ctx2 = PrfCtx(es1 ++ destructAnd(b2) ++ destructAnd(simplifyCond(Not(b1))) ++ es2)
      (ctx1, ctx2)
    case (e, i) if e.collectFirst { case Ite(_, _, _) => () }.isDefined =>
      val cond = e.collectFirst { case Ite(c, _, _) => c }.get
      val e1 = e.transform { case Ite(c, e, _) if c == cond => e }
      val e2 = e.transform { case Ite(c, _, e) if c == cond => e }
      val es1 = hypotheses.take(i)
      val es2 = hypotheses.drop(i + 1)
      val ctx1 = PrfCtx(es1 ++ destructAnd(cond) ++ destructAnd(e1) ++ es2)
      val ctx2 = PrfCtx(es1 ++ destructAnd(simplifyCond(Not(cond))) ++ destructAnd(e2) ++ es2)
      (ctx1, ctx2)
  }

  def getLang(value: Expr): RegEx =
    hypotheses.reverse.collectFirst {
      case TypeTest(e, LangType(r)) if e == value => r
    }.getOrElse {
      value match
        case Var(x) => types(x).asInstanceOf[LangType].re
        case _ => throw IllegalArgumentException(s"regex not found: $value")
    }

  def +(cond: Expr): PrfCtx = PrfCtx(hypotheses ++ destructAnd(simplifyCond(cond)))

  def ++(conds: List[Expr]): PrfCtx = PrfCtx(hypotheses ++ conds.map(simplifyCond).flatMap(destructAnd))

  def canTriviallyProve(conclusion: Expr): Boolean =
    contains(conclusion) || contains(Const(false)) || exists { case TypeTest(_, LangType(RENone)) => true }

  override def toString: String = hypotheses.mkString(" ∧ ")

object PrfCtx:
  def empty(using types: Types) = PrfCtx(Nil)