package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.StrREOps.{getLang, isFiniteLang}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.ExprOps.conjuncts
import flat.checker.verif.Simplifier.simplify
import flat.checker.verif.Type.*

import scala.Function.unlift
import scala.collection.mutable.ListBuffer

object Prover extends LazyLogging:
  def prove(goal: Goal, path: String = ""): Boolean =
    val simplifiedGoal = simplify(goal)
    splitIf(simplifiedGoal) match
      case Some((trueGoal, trueCond), (falseGoal, falseCond)) =>
        prove(trueGoal, s"$path if (true) ${trueCond.show}")
          && prove(falseGoal, s"$path if (false) ${falseCond.show}")
      case None =>
        splitOr(simplifiedGoal) match
          case Some((leftGoal, leftCond), (rightGoal, rightCond)) =>
            prove(leftGoal, s"$path if (left) ${leftCond.show}")
              && prove(rightGoal, s"$path if (right) ${rightCond.show}")
          case None =>
            if path.isEmpty then
              logger.debug("Simplified:\n{}", simplifiedGoal.pp)
            else
              logger.debug("Case{}:\n{}", path, simplifiedGoal.pp)
            val success = solve(simplifiedGoal)
            if !success then
              logger.warn("Failed to prove goal:\n{}", simplifiedGoal.pp)
            success

  private def simplify(goal: Goal): Goal =
    val conclusion = goal.conclusion.simplify
    val premises = goal.premises.flatMap(_.simplify.conjuncts)
    // Eliminate implications
    val premises1 = premises.map:
      case Implies(e1, e2) if premises.contains(e1) => e2
      case Ite(e, e1, _) if premises.contains(e) => e1
      case Ite(e, _, e2) if premises.contains(Not(e)) => e2
      case e => e
    goal.copy(conclusion = conclusion, premises = premises1)

  private def splitIf(goal: Goal): Option[((Goal, Expr), (Goal, Expr))] =
    for e <- (goal.conclusion :: goal.premises).collectFirst(unlift(collectFirstIfCond)) yield
      val ne = Not(e).simplify
      ((intro(e, goal), e), (intro(ne, goal), ne))

  private def collectFirstIfCond: Expr => Option[Expr] = _.collectFirst { case Ite(e, _, _) => e }

  private def splitOr(goal: Goal): Option[((Goal, Expr), (Goal, Expr))] =
    goal.premises.collectFirst:
      case e@Or(e1, e2) =>
        val goal1 = goal.copy(premises = goal.premises.filterNot(_ == e))
        ((intro(e1, goal1), e1), (intro(e2, intro(Not(e1).simplify, goal1)), e2))

  private def intro(cond: Expr, goal: Goal): Goal =
    val conclusion = apply(goal.conclusion, cond)
    val premises = goal.premises.map(apply(_, cond))
    goal.copy(conclusion = conclusion, premises = premises :+ cond)

  private def apply(expr: Expr, cond: Expr): Expr = expr match
    case Implies(`cond`, e) => e
    case Ite(`cond`, e1, _) => e1
    case Ite(e, _, e2) if Not(cond).simplify == e => e2
    case _ => expr

  private def solve(goal: Goal): Boolean =
    goal.conclusion match
      case BoolConst(true) => true
      case StrIn(e, r) =>
        val inferer = Inferer(goal)
        inferer.infer(e) match
          case TStr(r1) =>
            logger.debug("Inferred: {} ∈ {}", e.show, r1.pp)
            r1 == r || RESub.check(r1, r)
          case _ => ???
      case _ =>
        val lemmas = synthesizeLemmas(goal)
        if lemmas.nonEmpty then
          logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
        if lemmas.contains(goal.conclusion) then
          return true

        val solver = new SMTSolver(using goal.ctx)
        goal.premises.foreach(solver.add)
        lemmas.foreach(solver.add)
        solver.prove(goal.conclusion)

  private def synthesizeLemmas(goal: Goal): List[Expr] =
    val inferer = Inferer(goal)
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.collect:
      case e: StrLength =>
        inferer.infer(e) match
          case TNat(set) =>
            lemmas += inNatSet(e, set)
          case _ =>
      case e: CharAt =>
        inferer.infer(e) match
          case TChar(set) =>
            lemmas += inCharSet(e, set)
          case _ => ()
      case e: Substr =>
        inferer.infer(e) match
          case TStr(r) =>
            lemmas += inRegEx(e, r)
          case _ => ()
      case e: (StrStartsWith | StrEndsWith | StrContains | StrIs) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@StrIndexOf(es, _, IntConst(0)) =>
        inferer.infer(e) match
          case TIndex(set) =>
            lemmas += inIndexSet(e, set)
          case _ => ()
      case e: StrToInt =>
        inferer.infer(e) match
          case TNum(range) =>
            lemmas += inNatRange(e, range)
          case _ => ()

      case e@Eq(_, StrConst(_)) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@Ne(_, StrConst(_)) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()

      // Algebraic properties for count
      case e@StrCount(Substr(es, ei, ej), StrConst(s)) if s.length == 1 && goal.have(Lt(ei, ej)) =>
        val c = s.head
        // If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)`
        goal.premises.collectFirst:
          case Eq(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, Add(StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)), IntConst(1)))
          case Ne(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)))

    lemmas.toList

  private def inNatSet(elem: Expr, set: CountingRE): Expr =
    if set.isFinite then
      mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, IntConst(n)))
    else
      Le(IntConst(set.min), elem)

  private def inCharSet(elem: Expr, set: CharSet): Expr =
    if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c)))
    else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c)))

  private def inRegEx(elem: Expr, r: StrRE): Expr = r match
    case RegEx.Lit(a) => inCharSet(elem, a)
    case _ =>
      if r.isFiniteLang then mkOr(for s <- r.getLang.toList.sorted yield Eq(elem, StrConst(s)))
      else StrIn(elem, r)

  private def inBoolSet(elem: Expr, set: BoolSet): List[Expr] = set match
    case BoolSet.True => List(elem)
    case BoolSet.False => List(Not(elem))
    case _ => Nil

  private def inIndexSet(elem: Expr, set: IndexSet): Expr =
    val cases = ListBuffer.empty[Expr]
    if set.neg.nonEmpty then
      for i <- set.neg.toList.sorted do
        cases += Eq(elem, IntConst(i))
    if set.pos.isFinite then
      for n <- set.pos.toFinSet.toList.sorted do
        cases += Eq(elem, IntConst(n))
    else
      cases += Le(IntConst(set.pos.min), elem)
    mkOr(cases.toList)

  private def inNatRange(elem: Expr, range: NatRange): Expr = range.max match
    case Some(max) => And(Le(IntConst(range.min), elem), Le(elem, IntConst(max)))
    case None => Le(IntConst(range.min), elem)
