package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.regex.REOps.*
import flat.regex.RegEx

import scala.collection.mutable

class Narrower(goal: Goal) extends LazyLogging:
  private val langs = mutable.Map.empty[Expr, RegEx]

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
      logger.debug("Narrow {}: {} by {}", target.show, lang.show, premise.show)
    // return updated goal
    val newPremises: List[Expr] = updated.toList.map(e => StringInLang(e, langs(e)))
    goal.copy(premises = goal.premises ++ newPremises)(using goal.sorts)

  private def apply(e: Expr, r: RegEx, premise: Expr): Option[RegEx] = premise match
    // prefix, suffix
    case SeqStartsWith(`e`, Const(t: String)) => Some(r.filterStartWith(t))
    case Not(SeqStartsWith(`e`, Const(t: String))) => Some(r.filterNotStartWith(t))
    case SeqEndsWith(`e`, Const(t: String)) => Some(r.filterEndWith(t))
    case Not(SeqEndsWith(`e`, Const(t: String))) => Some(r.filterNotEndWith(t))
    // equality
    case Eq(`e`, Const(t: String)) => Some(r.filterEq(t))
    case Ne(`e`, Const(t: String)) => Some(r.filterNe(t))
    // infix
    case SeqContains(`e`, Const(t: String)) => Some(r.filterContain(t))
    case Ne(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterContain(t))
    case Le(Const(0), SeqIndexOf(`e`, Const(t: String), Const(0))) => Some(r.filterContain(t))
    case Not(SeqContains(`e`, Const(t: String))) => Some(r.filterNotContain(t))
    case Eq(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterNotContain(t))
    case Lt(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(0)) => Some(r.filterNotContain(t))
    case _ => None
