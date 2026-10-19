package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.ExprOps.conjuncts
import flat.checker.verif.Simplifier.simplify
import io.github.cvc5

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Prover extends LazyLogging:
  def prove(goal: Goal, path: List[String] = Nil): Boolean =
    val simpGoal = simplify(goal)
    if trivial(simpGoal) then
      logger.debug("Trivial")
      return true

    simpGoal.conclusion match
      case And(e1, e2) => // P => e1, P => e2
        prove(simpGoal.copy(conclusion = e1), path :+ "left")
          && prove(simpGoal.copy(conclusion = e2), path :+ "right")
      case Or(e1, e2) => // P and !e1 => e2
        prove(simpGoal.copy(conclusion = e2, premises = simpGoal.premises :+ Not(e1)), path)
      case Implies(e1, e2) => // P and e1 => e2
        prove(simpGoal.copy(conclusion = e2, premises = simpGoal.premises :+ e1), path)
      case e if e.collectFirst { case Ite(_, _, _) => true }.isDefined =>
        val cond = e.collectFirst { case Ite(b, _, _) => b }.get
        prove(simpGoal.copy(premises = simpGoal.premises :+ cond), path :+ s"if ${cond.show}")
          && prove(simpGoal.copy(premises = simpGoal.premises :+ Not(cond)), path :+ s"if not ${cond.show}")
      case _ => split(simpGoal) match
        case Some(((goal1, s1), (goal2, s2))) =>
          prove(goal1, path :+ s1) && prove(goal2, path :+ s2)
        case None =>
          if path.isEmpty then
            logger.debug("Simplified:\n{}", simpGoal.pp)
          else
            logger.debug("Case {}:\n{}", path.mkString(", "), simpGoal.pp)
          solve(simpGoal)

  private def simplify(goal: Goal): Goal =
    val premises = goal.premises.flatMap(_.simplify.conjuncts)
    val goal1 = goal.copy(
      conclusion = elim(goal.conclusion.simplify, premises),
      premises = premises.map(elim(_, premises)))
    unfold(subst(goal1))

  private def elim(expr: Expr, premises: List[Expr]): Expr =
    expr.transform:
      case Implies(e1, e2) if premises.contains(e1) => e2
      case Ite(e, e1, _) if premises.contains(e) => e1
      case Ite(e, _, e2) if premises.contains(Not(e).simplify) => e2

  private def subst(goal: Goal): Goal =
    val eqs = mutable.Map.empty[String, Expr]
    val used = ListBuffer.empty[Expr]
    goal.premises.foreach:
      case b@Le(Var(x), e) =>
        val bRev = Le(e, Var(x))()
        if goal.premises.contains(bRev) then
          eqs(x) = e
          used += b
          used += bRev
      case _ =>
    if eqs.nonEmpty then
      goal.copy(
        premises = goal.premises.filterNot(used.contains).map(_.subst(eqs.toMap)),
        conclusion = goal.conclusion.subst(eqs.toMap))
    else
      goal

  private def unfold(goal: Goal): Goal =
    goal.conclusion match
      case StrForall(Substr(e, IntConst(0), Add(Var(i), IntConst(1))), ef)
        if goal.premises.contains(StrForall(Substr(e, IntConst(0), Var(i)), ef)()) =>
        // e[:i + 1].forall(f) = e[:i].forall(f) && f(e[i])
        goal.copy(conclusion = Apply(ef, List(CharAt(e, Var(i)))).simplify)
      case _ => goal

  private def trivial(goal: Goal): Boolean =
    goal.conclusion == BoolConst(true)(noRange) || goal.premises.contains(goal.conclusion) || goal.premises.exists:
      case BoolConst(false) => true
      case StrIn(_, r) => r.isEmpty
      case Eq(e1, e2) => goal.premises.contains(Ne(e1, e2)())
      case Ne(e1, e2) => goal.premises.contains(Eq(e1, e2)())
      case Lt(e1, e2) => goal.premises.contains(Lt(e2, e1)())
      case Not(e) => goal.premises.contains(e)
      case _ => false

  private def split(goal: Goal): Option[((Goal, String), (Goal, String))] =
    goal.premises.zipWithIndex.collectFirst:
      case (e@Or(e1, e2), i) =>
        ((goal.copy(premises = goal.premises.updated(i, e1)), s"if ${e1.show}"),
          (goal.copy(premises = goal.premises.updated(i, And(Not(e1), e2))), s"if ${e2.show}"))
      case (e, i) if e.collectFirst { case Ite(_, _, _) => true }.isDefined =>
        val cond = e.collectFirst { case Ite(b, _, _) => b }.get
        ((goal.copy(premises = goal.premises :+ cond), s"if ${cond.show}"),
          (goal.copy(premises = goal.premises :+ Not(cond)), s"if ${Not(cond).simplify.show}"))

  private def vars(expr: Expr): Set[String] = expr.collect { case Var(x) => x }.toSet

  private def solve(goal: Goal): Boolean =
    val narrower = new Narrower(goal)
    val narrowedGoal = narrower.narrow
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
