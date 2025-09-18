package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.RegEx

class PrfCtx private(val hypotheses: List[Expr])(using val types: Types) extends LazyLogging:

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

  def destruct(candidates: List[Expr]): List[PrfCtxCase] = candidates match
    case Nil => List(PrfCtxCase(this, Nil))
    case b :: bs =>
      val k = hypotheses.indexOf(b)
      assert(k >= 0)
      val cases = b match
        case or: Or =>
          val bs = destructOr(or)
          for i <- bs.indices.toList yield
            val es1 = destructAnd(bs(i))
            val es2 = (0 until i).toList.flatMap(j => destructAnd(simplifyCond(Not(bs(j)))))
            PrfCtxCase(PrfCtx(hypotheses.take(k) ++ es1 ++ es2 ++ hypotheses.drop(k + 1)), List(bs(i).toString))
        case _ =>
          val cond = b.collectFirst { case Ite(c, _, _) => c }.get
          val (ctx1, ctx2) = destructIf(cond)
          List(PrfCtxCase(ctx1, List(cond.toString)), PrfCtxCase(ctx2, List(Not(cond).toString)))
      for
        PrfCtxCase(ctx, labels) <- cases
        PrfCtxCase(ctx1, labels1) <- ctx.destruct(bs)
      yield PrfCtxCase(ctx1, labels ++ labels1)

  def destructIf(cond: Expr): (PrfCtx, PrfCtx) =
    val ctx1 = PrfCtx(hypotheses.map(_.transform { case Ite(b, e, _) if b == cond => e }) :+ cond)
    val not = destructAnd(simplifyCond(Not(cond)))
    val ctx2 = PrfCtx(hypotheses.map(_.transform { case Ite(b, _, e) if b == cond => e }) ++ not)
    (ctx1, ctx2)

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

  def +(cond: Expr): PrfCtx =
    PrfCtx(hypotheses ++ destructAnd(simplifyCond(cond)))

  def ++(conds: List[Expr]): PrfCtx =
    PrfCtx(hypotheses ++ conds.map(simplifyCond).flatMap(destructAnd))

  override def toString: String = hypotheses.mkString(" ∧ ")

object PrfCtx:
  def empty(using types: Types) = PrfCtx(Nil)

final case class PrfCtxCase(ctx: PrfCtx, labels: List[String])
