package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.RegEx

class PrfCtx private(val hypotheses: List[Expr])(using val types: Types) extends LazyLogging:

  import RegEx.RENone
  import Rewriter.*

  def assumptions: List[Expr] = hypotheses

  def contains(cond: Expr): Boolean = hypotheses.contains(cond)

  def exists(p: Expr => Boolean): Boolean = hypotheses.exists(p)

  def exists(pf: PartialFunction[Expr, Boolean]): Boolean = exists(x => pf.lift(x).getOrElse(false))

  def collectFirst[T](pf: PartialFunction[Expr, T]): Option[T] = hypotheses.collectFirst(pf)

  def collect[T](pf: PartialFunction[Expr, T]): List[T] = hypotheses.collect(pf)

  def foreach(f: Expr => Unit): Unit = hypotheses.foreach(f)

  def destructCandidates: List[Expr] = hypotheses.collect:
    case or: Or => or
    case e if e.collectFirst { case Ite(_, _, _) => () }.isDefined => e

  def destruct(candidates: List[Expr]): List[PrfCtx] = candidates match
    case Nil => List(this)
    case b :: bs =>
      val k = hypotheses.indexOf(b)
      assert(k >= 0)
      val ctxs = b match
        case or: Or =>
          val bs = destructOr(or)
          for i <- bs.indices.toList yield
            val es1 = destructAnd(bs(i))
            val es2 = (0 until i).toList.flatMap(j => destructAnd(simplifyCond(Not(bs(j)))))
            PrfCtx(hypotheses.take(k) ++ es1 ++ es2 ++ hypotheses.drop(k + 1))
        case _ =>
          val cond = b.collectFirst { case Ite(c, _, _) => c }.get
          val (ctx1, ctx2) = destructIf(cond)
          List(ctx1, ctx2)
      ctxs.flatMap(_.destruct(bs))

  def destructIf(cond: Expr): (PrfCtx, PrfCtx) =
    val ctx1 = PrfCtx(hypotheses.map(_.transform { case Ite(b, e, _) if b == cond => e }) :+ cond)
    val not = destructAnd(simplifyCond(Not(cond)))
    val ctx2 = PrfCtx(hypotheses.map(_.transform { case Ite(b, _, e) if b == cond => e }) ++ not)
    (ctx1, ctx2)

  @deprecated
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

  def lookupSuffixLang(str: Expr): Option[(Expr, RegEx)] =
    hypotheses.reverse.collectFirst:
      case TypeTest(suffix@Substr(e1, ei, Length(e2)), LangType(r)) if e1 == str && e2 == str => (ei, r)

  def +(cond: Expr): PrfCtx = PrfCtx(hypotheses ++ destructAnd(simplifyCond(cond)))

  def ++(conds: List[Expr]): PrfCtx = PrfCtx(hypotheses ++ conds.map(simplifyCond).flatMap(destructAnd))

  def canTriviallyProve(conclusion: Expr): Boolean =
    contains(conclusion) || contains(Const(false)) || exists { case TypeTest(_, LangType(RENone)) => true }

  override def toString: String = hypotheses.mkString(" ∧ ")

object PrfCtx:
  def empty(using types: Types) = PrfCtx(Nil)
