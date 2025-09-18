package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.core.*
import flat.regex.RegEx
import flat.regex.RegEx.RENone

import scala.annotation.tailrec

final class Prover(using types: Types, config: Config) extends LazyLogging:
  def prove(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if smtSolver.canProve(Const(false))(using ctx) then
      logger.debug("PROVED by contradiction")
      return Right(())

    // Split the conclusion into multiple goals and prove each of them
    proveGoals(split(conclusion, ctx))

  def check(value: Expr, expected: Type, ctx: PrfCtx): Either[String, Unit] =
    prove(TypeTest(value, expected), ctx)

  def infer(value: Expr, ctx: PrfCtx): RegEx =
    val inferer = new Inferer(using ctx)
    inferer.inferLang(value)

  private def split(conclusion: Expr, ctx: PrfCtx): List[(Expr, PrfCtx)] = conclusion match
    case And(e1, e2) => split(e1, ctx) ++ split(e2, ctx)
    case Or(e1, e2) => split(e2, ctx + Not(e1))
    case e =>
      e.collectFirst { case ite: Ite => ite } match
        case Some(Ite(cond, _, _)) =>
          val (ctx1, ctx2) = ctx.destructIf(cond)
          val e1 = e.transform { case Ite(b, e, _) if b == cond => e }
          val e2 = e.transform { case Ite(b, _, e) if b == cond => e }
          split(e1, ctx1) ++ split(e2, ctx2)
        case None => List((e, ctx))

  @tailrec
  private def proveGoals(goals: List[(Expr, PrfCtx)], processed: Int = 0): Either[String, Unit] = goals match
    case Nil => Right(())
    case (e, ctx) :: rest =>
      logger.debug(s"Goal ${processed + 1}: ⇒ $e")
      proveGoal(e, ctx) match
        case Left(err) =>
          logger.debug(s"[X] Goal ${processed + 1} FAILED")
          Left(err)
        case Right(_) => proveGoals(rest, processed + 1)

  private def proveGoal(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if ctx.hypotheses.contains(conclusion) then
      logger.debug("PROVED trivially")
      return Right(())

    if smtSolver.canProve(conclusion)(using ctx) then
      logger.debug("PROVED by SMT")
      return Right(())

    if ctx.destructCandidates.isEmpty then proveCase(conclusion, ctx) else destructAndProve(conclusion, ctx)

  private def destructAndProve(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    val m = ctx.destructCandidates.groupBy(similarity(conclusion, _))
    val hs = m(m.keySet.max)
    logger.debug("Destruct: " + hs.mkString(", "))
    proveCases(ctx.destruct(hs))(using conclusion)

  private def similarity(conclusion: Expr, hypothesis: Expr): Int =
    val xs = conclusion.collectVars
    (hypothesis.collectVars & xs).size

  @tailrec
  private def proveCases(cases: List[PrfCtxCase])(using conclusion: Expr): Either[String, Unit] = cases match
    case Nil => Right(())
    case PrfCtxCase(ctx, labels) :: rest =>
      logger.debug("Case " + labels.mkString(", ") + ":")
      var result = proveCase(conclusion, ctx)
      if result.isLeft && ctx.destructCandidates.nonEmpty then
        logger.debug("Try destruct more hypotheses")
        result = destructAndProve(conclusion, ctx)
      result match
        case Left(msg) =>
          logger.debug(s"[X] Case FAILED")
          Left(msg)
        case Right(_) => proveCases(rest)

  private def proveCase(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if ctx.hypotheses.contains(conclusion) then
      logger.debug("PROVED trivially")
      return Right(())

    if smtSolver.canProve(conclusion)(using ctx) then
      logger.debug("PROVED by SMT")
      return Right(())

    val ctx1 = Refiner.refine(ctx)
    val noneStr = ctx1.hypotheses.collectFirst { case TypeTest(e, LangType(RENone)) => e }
    if noneStr.isDefined then
      logger.debug(s"PROVED by ${noneStr.get} : ∅ after type narrowing")
      return Right(())

    conclusion match
      case TypeTest(e, t) =>
        checkType(e, t)(using ctx1) match
          case Left(ta) => Left(ta.toString)
          case Right(_) => Right(())
      case _ =>
        proveWithLemmas(conclusion, List(conclusion), ctx1)
          .orElse(proveWithLemmas(conclusion, ctx1.assumptions, ctx1))

  private def checkType(expr: Expr, typ: Type)(using ctx: PrfCtx): Either[Type, Unit] = typ match
    case LangType(r2) =>
      val inferer = new Inferer
      val r1 = inferer.inferLang(expr)
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
      throw UnsupportedOperationException(s"checkType $expr : $typ")

  private val smtSolver = new SMTSolver

  private def proveWithLemmas(conclusion: Expr, seeds: List[Expr], ctx: PrfCtx): Either[String, Unit] =
    val synth = new HintSynth(using ctx)
    val lemmas = seeds.flatMap(synth.collectHints).distinct
    if lemmas.nonEmpty then
      if lemmas.contains(conclusion) then
        logger.debug(s"PROVED by the exact lemma $conclusion" +
          (if lemmas.length > 1 then s" (${lemmas.length - 1} unused lemmas)" else ""))
        return Right(())

      if smtSolver.canProve(conclusion)(using ctx ++ lemmas) then
        logger.debug(s"PROVED by lemmas + SMT (lemmas: ${lemmas.mkString(", ")})")
        return Right(())

    // Otherwise: not proved
    Left("")