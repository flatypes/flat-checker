package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.{StrRE, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable

class Narrower(goal: Goal) extends LazyLogging:
  private val langs = mutable.Map.empty[Expr, StrRE]

  def narrow: Goal =
    // collect target expressions for narrowing
    goal.premises.foreach:
      case StringInLang(e, r) => langs.update(e, r)
      case _ =>
    // apply narrowing rules
    val updated = mutable.Set.empty[Expr]
    for
      target <- langs.keys
      premise <- goal.premises
      lang <- apply(target, langs(target), premise)
    do
      langs.update(target, lang)
      updated.add(target)
      logger.debug("Narrow {}: {} by {}", target.show, lang.pp, premise.show)
    // return updated goal
    val newPremises: List[Expr] = updated.toList.map(e => StringInLang(e, langs(e)))
    goal.copy(premises = goal.premises ++ newPremises)(using goal.sorts)

  private def apply(e: Expr, r: StrRE, premise: Expr): Option[StrRE] = premise match
    // prefix, suffix
    case SeqStartsWith(`e`, Const(t: String)) => Some(r.filterStartsWith(t.toList))
    case Not(SeqStartsWith(`e`, Const(t: String))) => Some(r.filterNotStartWith(t.toList))
    case SeqEndsWith(`e`, Const(t: String)) => Some(r.filterEndsWith(t.toList))
    case Not(SeqEndsWith(`e`, Const(t: String))) => Some(r.filterNotEndWith(t.toList))
    // equality
    case Eq(`e`, Const(t: String)) => Some(r.filterEq(t.toList))
    case Ne(`e`, Const(t: String)) => Some(r.filterNe(t.toList))
    // infix
    case SeqContains(`e`, Const(t: String)) => Some(r.filterContains(t.toList))
    case Ne(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterContains(t.toList))
    case Le(Const(0), SeqIndexOf(`e`, Const(t: String), Const(0))) => Some(r.filterContains(t.toList))
    case Not(SeqContains(`e`, Const(t: String))) => Some(r.filterNotContain(t.toList))
    case Eq(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterNotContain(t.toList))
    case Lt(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(0)) => Some(r.filterNotContain(t.toList))
    case _ => None
