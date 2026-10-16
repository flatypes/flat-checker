package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.ExprOps.conjuncts
import flat.checker.verif.Simplifier.simplify
import io.github.cvc5

import scala.Function.unlift

object Prover extends LazyLogging:
  def prove(goal: Goal, path: List[String] = Nil): Boolean =
    val simpGoal = simplify(goal)
    if trivial(simpGoal) then
      return true

    simpGoal.conclusion match
      case And(e1, e2) => // P => e1, P => e2
        prove(simpGoal.copy(conclusion = e1), path :+ "left")
          && prove(simpGoal.copy(conclusion = e2), path :+ "right")
      case Or(e1, e2) => // P and !e1 => e2
        prove(simpGoal.copy(conclusion = e2, premises = simpGoal.premises :+ Not(e1)), path)
      case Implies(e1, e2) => // P and e1 => e2
        prove(simpGoal.copy(conclusion = e2, premises = simpGoal.premises :+ e1), path)
      case _ =>
        if path.isEmpty then
          logger.debug("Simplified:\n{}", simpGoal.pp)
        else
          logger.debug("Case {}:\n{}", path.mkString(", "), simpGoal.pp)
        solve(simpGoal)
//
//    splitIf(simpGoal) match
//      case Some((trueGoal, trueCond), (falseGoal, falseCond)) =>
//        prove(trueGoal, s"$path, if (true) ${trueCond.show}")
//          && prove(falseGoal, s"$path, if (false) ${falseCond.show}")
//      case None =>
//        splitOr(simpGoal) match
//          case Some((leftGoal, leftCond), (rightGoal, rightCond)) =>
//            prove(leftGoal, s"$path, if (left) ${leftCond.show}")
//              && prove(rightGoal, s"$path, if (right) ${rightCond.show}")
//          case None =>
//            if path.isEmpty then
//              logger.debug("Simplified:\n{}", simpGoal.pp)
//            else
//              logger.debug("Case{}:\n{}", path, simpGoal.pp)
//            solve(simpGoal)

  private def trivial(goal: Goal): Boolean =
    goal.conclusion == BoolConst(true) || goal.premises.contains(goal.conclusion) || goal.premises.exists:
      case BoolConst(false) => true
      case StrIn(_, r) => r.isEmpty
      case Eq(e1, e2) => goal.premises.contains(Ne(e1, e2)())
      case Ne(e1, e2) => goal.premises.contains(Eq(e1, e2)())
      case Lt(e1, e2) => goal.premises.contains(Lt(e2, e1)())
      case Not(e) => goal.premises.contains(e)
      case _ => false

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
    goal.copy(conclusion = conclusion, premises = premises :+ cond.simplify)

  private def apply(expr: Expr, cond: Expr): Expr =
    val notCond = Not(cond).simplify
    expr.transform:
      case Implies(`cond`, e) => e
      case Implies(`notCond`, _) => BoolConst(true)
      case Ite(`cond`, e1, _) => e1
      case Ite(`notCond`, _, e2) => e2

  private def solve(goal: Goal): Boolean =
    val narrowedGoal = Narrower.narrow(goal)
    if trivial(narrowedGoal) then
      return true

    goal.conclusion match
      case StrIn(e, r) =>
        Inferer.infer(e)(using narrowedGoal) match
          case Domain.Str(r1) =>
            logger.debug("Inferred: {} ∈ {}", e.show, r1.pp)
            r1 == r || RESub.check(r1, r)
          case _ => throw IllegalStateException("type error")
      case b =>
        solveLogicalGoal(narrowedGoal)

  private def solveLogicalGoal(goal: Goal): Boolean =
    val lemmas = Synthesizer.synthesizeLemmas(goal)
    if lemmas.nonEmpty then
      logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
      if lemmas.contains(goal.conclusion) then
        return true

    val tm = new cvc5.TermManager
    val encoder = new SMTEncoder(goal.ctx, tm)
    val solver = new cvc5.Solver(tm)
    solver.setOption("tlimit-per", "3000")
    for e <- goal.premises ++ lemmas do
      val formula = encoder.encodeExpr(e, EncodingCtx.empty)
      solver.assertFormula(formula)
    val formula = encoder.encodeExpr(goal.conclusion, EncodingCtx.empty)
    solver.assertFormula(formula.notTerm)
    val result = solver.checkSat()
    result.isUnsat
