package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.ExprOps.*
import flat.checker.core.*
import flat.regex.RegEx

class PrfCtx private(val hypotheses: List[Expr])(using val types: Types) extends LazyLogging:
  def destructCandidates: List[Expr] = (hypotheses.collect:
    case or: Or => or
    case e if e.collectFirst { case Ite(_, _, _) => () }.isDefined => e).distinct

  def destruct(candidates: List[Expr]): List[PrfCtxCase] = candidates match
    case Nil => List(PrfCtxCase(this, Nil))
    case b :: bs =>
      hypotheses.indexOf(b) match
        case -1 => destruct(bs)
        case k =>
          val cases = b match
            case or: Or =>
              val bs = or.disjuncts
              for i <- bs.indices.toList yield
                val es1 = conjuncts(bs(i))
                val es2 = (0 until i).toList.flatMap(j => Not(bs(j)).simpl.conjuncts)
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
    val not = Not(cond).simpl.conjuncts
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
    PrfCtx(hypotheses ++ cond.simpl.conjuncts)

  def ++(conds: List[Expr]): PrfCtx =
    PrfCtx(hypotheses ++ conds.map(simpl).flatMap(conjuncts))

  override def toString: String = hypotheses.mkString(" ∧ ")

object PrfCtx:
  def empty(using types: Types) = PrfCtx(Nil)

final case class PrfCtxCase(ctx: PrfCtx, labels: List[String])
