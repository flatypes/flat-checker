package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.StrREOps.*
import flat.checker.flan.Show.show
import flat.checker.flan.SortOps.sort
import flat.checker.flan.tpd.*
import flat.checker.flan.{Sort, strListSort, stringSort}
import flat.checker.verif.Simplifier.simplify
import flat.checker.verif.Type.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Prover extends LazyLogging:
  def prove(goal: Goal): Boolean =
    goal.conclusion.collectFirst { case Ite(e, _, _) => e } match
      case Some(e) =>
        val (ps1, ps2) = goal.premises.map(caseIf(e, _)(using goal.sorts)).unzip
        val (c1, c2) = caseIf(e, goal.conclusion)(using goal.sorts)
        val goal1 = Goal(ps1 :+ e, c1)(using goal.sorts)
        logger.debug("Case if: {}", e.show)
        logger.debug("Subgoal 1:\n{}", showGoal(goal1))
        if prove(goal1) then
          val goal2 = Goal(ps2 :+ Not(e).simplify(using goal.sorts), c2)(using goal.sorts)
          logger.debug("Subgoal 2:\n{}", showGoal(goal2))
          prove(goal2)
        else
          logger.warn("❌ Subgoal 1 FAILED")
          false
      case None =>
        if prove1(goal) then
          true
        else
          val i = goal.premises.indexWhere(_.isInstanceOf[Or])
          if i >= 0 then
            val Or(e1, e2) = goal.premises(i).asInstanceOf[Or]
            logger.debug("Case left: {}", e1.show)
            val goal1 = Goal(goal.premises.updated(i, e1), goal.conclusion)(using goal.sorts)
            logger.debug("Subgoal 1:\n{}", showGoal(goal1))
            if prove1(goal1) then
              val goal2 = Goal(goal.premises.updated(i, e2), goal.conclusion)(using goal.sorts)
              logger.debug("Case right: {}", e2.show)
              logger.debug("Subgoal 2:\n{}", showGoal(goal2))
              prove1(goal2)
            else
              logger.warn("❌ Subgoal 1 FAILED")
              false
          else
            false

  def prove1(goal: Goal): Boolean =
    goal.conclusion match
      // refinement type checking
      case StringInLang(e, r) =>
        val inferer = Inferer(goal)
        inferer.infer(e) match
          case TStr(r1) =>
            logger.debug("Inferred: {} ∈ {}", e.show, r1.pp)
            RESub.check(r1, r)
          case _ => ???
      case _ =>
        val solver = SMTSolver(using goal.sorts)
        goal.premises.foreach(solver.add)
        if solver.prove(goal.conclusion) then
          true
        else
          val lemmas = synthesizeLemmas(goal)
          if lemmas.nonEmpty then
            logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
            lemmas.foreach(solver.add)
            solver.prove(goal.conclusion)
          else
            false

  private def caseIf(cond: Expr, expr: Expr)(using sorts: Map[String, Sort]): (Expr, Expr) =
    val e1 = expr.transform:
      case Ite(`cond`, e1, _) => e1
      case `cond` => Const(true)
    val e2 = expr.transform:
      case Ite(`cond`, _, e2) => e2
      case `cond` => Const(false)
    (e1.simplify, e2.simplify)

  private inline def isStrOrStrList(e: Expr)(using sorts: Map[String, Sort]): Boolean =
    val s = e.sort
    s == stringSort || s == strListSort

  private def synthesizeLemmas(goal: Goal): List[Expr] =
    val inferer = Inferer(goal)
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.collect:
      case e@SeqLength(es) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TNat(set) =>
            lemmas += inNatSet(e, set)
          case _ => ()
      case e@SeqSelect(es, _) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TChar(set) =>
            lemmas += inCharSet(e, set)
          case _ => ()
      case e@SeqSlice(es, _, _) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TStr(r) =>
            lemmas += inRegEx(e, r)
          case _ => ()
      case e@SeqStartsWith(es, _) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqEndsWith(es, _) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqContains(es, _) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqIndexOf(es, _, Const(0)) if isStrOrStrList(es)(using goal.sorts) =>
        inferer.infer(e) match
          case TIndex(set) =>
            lemmas += inIndexSet(e, set)
          case _ => ()
      case e: StrIsAscii =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e: StringToInt =>
        inferer.infer(e) match
          case TNum(range) =>
            lemmas += inNatRange(e, range)
          case _ => ()

      // Algebraic properties for count
      case e@SeqCount(SeqSlice(es, ei, ej), Const(s: String)) if s.length == 1 && goal.have(Lt(ei, ej)) =>
        val c = s.head
        // If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)`
        goal.premises.collectFirst:
          case Eq(SeqSelect(`es`, ek), Const(c: Char)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, Add(SeqCount(SeqSlice(es, Add(ei, 1), ej), Const(s)), 1))
          case Ne(SeqSelect(`es`, ek), Const(c: Char)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, SeqCount(SeqSlice(es, Add(ei, 1), ej), Const(s)))

    lemmas.toList

  private def inNatSet(elem: Expr, set: CountingRE): Expr =
    if set.isFinite then
      mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, Const(n)))
    else
      Le(set.min, elem)

  private def inCharSet(elem: Expr, set: CharSet): Expr =
    if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, Const(c)))
    else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, Const(c)))

  private def inRegEx(elem: Expr, r: StrRE): Expr = r match
    case RegEx.Lit(a) => inCharSet(elem, a)
    case _ =>
      if r.isFiniteLang then mkOr(for s <- r.getLang.toList.sorted yield Eq(elem, Const(s)))
      else StringInLang(elem, r)

  private def inBoolSet(elem: Expr, set: BoolSet): List[Expr] = set match
    case BoolSet.True => List(elem)
    case BoolSet.False => List(Not(elem))
    case _ => Nil

  private def inIndexSet(elem: Expr, set: IndexSet): Expr =
    val cases = ListBuffer.empty[Expr]
    if set.neg.nonEmpty then
      for i <- set.neg.toList.sorted do
        cases += Eq(elem, Const(i))
    if set.pos.isFinite then
      for n <- set.pos.toFinSet.toList.sorted do
        cases += Eq(elem, Const(n))
    else
      cases += Le(set.pos.min, elem)
    mkOr(cases.toList)

  private def inNatRange(elem: Expr, range: NatRange): Expr = range.max match
    case Some(max) => And(Le(Const(range.min), elem), Le(elem, Const(max)))
    case None => Le(Const(range.min), elem)

  private def showGoal(goal: Goal): String =
    val lines = for e <- goal.premises yield s"  ${e.show}\n"
    lines.mkString + s" ⇒ ${goal.conclusion.show}"
