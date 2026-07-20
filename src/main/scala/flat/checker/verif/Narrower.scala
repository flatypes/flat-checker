package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.regex.RENarrowOps.*
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

  private def apply(target: Expr, regEx: RegEx, premise: Expr): Option[RegEx] = premise match
    // prefix, suffix
    case SeqStartsWith(`target`, Const(t: String)) => Some(regEx.filterStartWith(t))
    case Not(SeqStartsWith(`target`, Const(t: String))) => Some(regEx.filterNotStartWith(t))
    case SeqEndsWith(`target`, Const(t: String)) => Some(regEx.filterEndWith(t))
    case Not(SeqEndsWith(`target`, Const(t: String))) => Some(regEx.filterNotEndWith(t))
    // equality
    case Eq(`target`, Const(t: String)) => Some(regEx.filterEq(t))
    case Ne(`target`, Const(t: String)) => Some(regEx.filterNe(t))
    // infix
    case SeqContains(`target`, Const(t: String)) => Some(regEx.filterContain(t))
    case Ne(SeqIndexOf(`target`, Const(t: String), Const(0)), Const(-1)) => Some(regEx.filterContain(t))
    case Le(Const(0), SeqIndexOf(`target`, Const(t: String), Const(0))) => Some(regEx.filterContain(t))
    case Not(SeqContains(`target`, Const(t: String))) => Some(regEx.filterNotContain(t))
    case Eq(SeqIndexOf(`target`, Const(t: String), Const(0)), Const(-1)) => Some(regEx.filterNotContain(t))
    case Lt(SeqIndexOf(`target`, Const(t: String), Const(0)), Const(0)) => Some(regEx.filterNotContain(t))
    case _ => None
