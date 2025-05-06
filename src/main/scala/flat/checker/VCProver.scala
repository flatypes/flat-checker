package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Issuer
import flat.checker.Formula.*
import flat.checker.SolverResult.*
import flat.checker.core.*
import flat.util.tryAll

import scala.util.control.Breaks.{break, breakable}

class VCProver(using types: Types) extends LazyLogging:

  import Rewriter.*

  val issuer = new Issuer

  def prove(goal: Formula): Unit =
    proveGoal(goal)(using ProofCtx.empty)
    issuer.ensureNoError()

  private def proveGoal(conclusion: Formula)(using ctx: ProofCtx): Unit =
    conclusion match
      case True => // trivially hold
      case HasType(e, t, loc) =>
        logger.debug("")
        logger.debug("Goal " + ctx.assumptions.mkString(" ∧ ") + " ⇒ " + s"$e : $t")
        if ctx.impliesFalse then
          logger.debug("Goal proved")
        else checkType(e, t) match
          case Right(_) =>
            logger.debug("Goal proved")
          case Left(t1) =>
            issuer.report(TypeMayMismatch(t.toString, t1.toString, e.loc))
            logger.debug("Goal failed")
      case Goal(e, err) =>
        logger.debug("")
        logger.debug("Goal " + ctx.assumptions.mkString(" ∧ ") + " ⇒ " + e.toString)
        if checkProp(e) then
          logger.debug("Goal proved")
        else
          issuer.report(err)
          logger.debug("Goal failed")
      case LAnd(goal1, goal2) =>
        proveGoal(goal1)
        proveGoal(goal2)
      case LImp(e, c) =>
        val es = destructAnd(simplifyCond(e))
        proveGoal(c)(using ctx ++ es)

  private def checkType(expr: Expr, typ: Type)(using ctx: ProofCtx): Either[Type, Unit] = typ match
    case LangType(r2) =>
      val inferer = new HintSynth
      val r1 = inferer.inferLang(expr)
      logger.debug(s"infer $expr : $r1")
      if r1.subsetOf(r2) then Right(()) else Left(LangType(r1))
    case TupleType(ts) =>
      expr match
        case TupleExpr(es) =>
          assert(es.length == ts.length)
          val results = for (e, t) <- es zip ts yield (t, checkType(e, t))
          if results.forall(_._2.isRight) then Right(())
          else Left(TupleType(results.map {
            case (_, Left(t)) => t
            case (t, Right(_)) => t
          }))
        case _ => throw UnsupportedOperationException()
    case _ =>
      throw UnsupportedOperationException(s"check $expr : $typ")

  private def checkProp(prop: Expr)(using ctx: ProofCtx): Boolean =
    val tasks = prepareTasks(prop)
    var failure = false
    breakable:
      for (subGoal, ctx) <- tasks do
        if !proveSubGoal(subGoal)(using ctx) then
          failure = true
          break
    !failure

  private def prepareTasks(conclusion: Expr)(using ctx: ProofCtx): List[(Expr, ProofCtx)] =
    destructAnd(simplifyCond(conclusion)).flatMap {
      case e@Or(_, _) =>
        val disjuncts = destructOr(e)
        val newPremises = disjuncts.dropRight(1).map(Not.apply).map(simplifyCond).flatMap(destructAnd)
        prepareTasks(disjuncts.last)(using ctx ++ newPremises)
      case g =>
        ctx.assumptions.zipWithIndex.collectFirst { case (e@Or(_, _), i) => i -> destructOrClassical(e) } match {
          case Some(i, es) =>
            for e <- es yield
              val newPremises = destructAnd(simplifyCond(e))
              (g, ctx ++ newPremises)
          case None => List((g, ctx))
        }
    }

  private def proveSubGoal(goal: Expr)(using ctx: ProofCtx): Boolean =
    logger.debug("Sub Goal: " + ctx.assumptions.mkString(" ∧ ") + " ⇒ " + goal.toString)
    tryAll(
      () => proveByTrivial(goal),
      () => proveWithoutHints(goal),
      () => proveWithHints(goal),
      () => proveWithPremiseHints(goal)
    ) match
      case Some(_) =>
        logger.debug("Sub Goal proved")
        true
      case None =>
        logger.debug("X Sub Goal failed")
        false

  private def proveByTrivial(goal: Expr)(using ctx: ProofCtx): Option[Unit] =
    if ctx.impliesFalse || ctx.assumptions.contains(goal) then Some(()) else None

  private def proveWithoutHints(goal: Expr)(using ctx: ProofCtx): Option[Unit] =
    if SMTSolver.prove(goal) == Valid then Some(()) else None

  private def proveWithHints(goal: Expr)(using ctx: ProofCtx): Option[Unit] =
    solve(goal, goal)

  private def proveWithPremiseHints(goal: Expr)(using ctx: ProofCtx): Option[Unit] =
    tryAll(for e <- ctx.assumptions yield () => solve(goal, e))

  private def solve(goal: Expr, hintExpr: Expr)(using ctx: ProofCtx): Option[Unit] =
    goal match
      case TypeTest(e, t) =>
        checkType(e, t) match
          case Right(_) => Some(())
          case Left(ta) =>
            logger.debug(s"actual type: $ta")
            None
      case _ =>
        val synth = new HintSynth
        val hints = synth.collectHints(hintExpr)
        if hints.nonEmpty then
          logger.debug("  hints: " + hints.mkString(", "))
          SMTSolver.prove(goal)(using ctx.withHints(hints)) match
            case Valid => Some(())
            case Invalid(_) => None
        else None