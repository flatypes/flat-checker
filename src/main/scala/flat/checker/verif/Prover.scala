package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.ExprOps.conjuncts
import flat.checker.verif.Simplifier.simplify
import flat.checker.verif.Type.*
import io.github.cvc5

import scala.Function.unlift
import scala.collection.mutable.ListBuffer

object Prover extends LazyLogging:
  def prove(goal: Goal, path: String = ""): Boolean =
    val simplifiedGoal = simplify(goal)
    logger.debug("Simplified:\n{}", simplifiedGoal.pp)
    if simplifiedGoal.conclusion == BoolConst(true)
      || simplifiedGoal.premises.contains(BoolConst(false))
      || simplifiedGoal.premises.contains(simplifiedGoal.conclusion) then
      return true

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
            if path.nonEmpty then
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
    if narrowedGoal.premises.exists(impliesFalse) then
      return true

    var failure = false
    val todos = ListBuffer.empty[Expr]
    goal.conclusion.conjuncts.foreach:
      case StrIn(e, r) =>
        val inferer = Inferer(narrowedGoal)
        inferer.infer(e) match
          case TStr(r1) =>
            logger.debug("Inferred: {} ∈ {}", e.show, r1.pp)
            if !(r1 == r || RESub.check(r1, r)) then
              failure = true
          case _ => throw IllegalStateException("type error")
      case _ =>
        todos ++= goal.conclusion.conjuncts

    if todos.isEmpty then
      !failure
    else
      !failure && solveLogicalGoal(narrowedGoal.copy(conclusion = mkAnd(todos.toList)))

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

  private def impliesFalse(expr: Expr): Boolean = expr match
    case StrIn(_, r) => r.isEmpty
    case _ => false