package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.conjuncts
import flat.checker.core.*
import flat.regex.{CharSet, RegEx}
import flat.{Config, Ops}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Prover: prove verification conditions, or answer type queries. */
final class Prover(using config: Config, types: Types) extends LazyLogging:
  /** Proves that `conclusion` is valid under `ctx`. */
  def prove(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if smtProves(Const(false))(using ctx) then
      logger.debug("PROVED by contradiction")
      return Right(())

    // Split the conclusion into multiple goals and prove each of them
    proveGoals(split(conclusion, ctx, Nil))

  private val smtSolver = new SMTSolver

  private def smtProves(conclusion: Expr)(using ctx: PrfCtx): Boolean =
    for mc <- config.metrics do
      mc.timeStart("time/verif/smt")
    val isValid = smtSolver.proves(conclusion)(using ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/smt")
      mc.count("smt queries")
    isValid

  /** Proves that `value` has the `expected` type under `ctx`. */
  def check(value: Expr, expected: Type, ctx: PrfCtx): Either[String, Unit] =
    prove(TypeTest(value, expected), ctx)

  /** Infers the type of `value` under `ctx`. */
  def infer(value: Expr, ctx: PrfCtx): RegEx =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = ctx)
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    r

  private def split(conclusion: Expr, ctx: PrfCtx, labels: List[Expr]): List[(Expr, PrfCtx, List[Expr])] =
    conclusion match
      case And(e1, e2) => split(e1, ctx, labels) ++ split(e2, ctx, labels)
      case Or(e1, e2) => split(e2, ctx + Not(e1), labels :+ Not(e1))
      case e =>
        e.collectFirst { case ite: Ite => ite } match
          case Some(Ite(b, _, _)) =>
            val List(ctx1 -> ls1, ctx2 -> ls2) = ctx.destructIf(b)
            val e1 = e.transform { case Ite(e0, e1, _) if e0 == b => e1 }
            val e2 = e.transform { case Ite(e0, _, e2) if e0 == b => e2 }
            split(e1, ctx1, labels ++ ls1) ++ split(e2, ctx2, labels ++ ls2)
          case None => List((e, ctx, labels))

  @tailrec
  private def proveGoals(goals: List[(Expr, PrfCtx, List[Expr])]): Either[String, Unit] = goals match
    case Nil => Right(())
    case (e, ctx, ls) :: rest =>
      logger.debug("Goal: " + ls.mkString(", ") + (if ls.isEmpty then "" else " ") + s"⇒ $e")
      proveGoal(e, ctx) match
        case Left(err) =>
          logger.debug("[X] Goal FAILED")
          Left(err)
        case Right(_) => proveGoals(rest)

  private def proveGoal(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if ctx.premises.contains(conclusion) then
      logger.debug("PROVED trivially")
      return Right(())

    if smtProves(conclusion)(using ctx) then
      logger.debug("PROVED by SMT")
      return Right(())

    if ctx.destructCandidates.isEmpty then proveCase(conclusion, ctx) else destructAndProve(conclusion, ctx)

  private def destructAndProve(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    val candidates = ctx.destructCandidates
    val similarities = candidates.map(similarity(conclusion, _))
    val y = similarities.max
    val hs =
      for
        (h, x) <- candidates.zip(similarities)
        if x == y
      yield h
    logger.debug("Destruct: " + hs.mkString(", "))
    proveCases(ctx.destruct(hs))(using conclusion)

  private def similarity(conclusion: Expr, hypothesis: Expr): Int =
    (hypothesis.collectVars & conclusion.collectVars).size

  @tailrec
  private def proveCases(cases: List[(PrfCtx, List[Expr])])(using conclusion: Expr): Either[String, Unit] = cases match
    case Nil => Right(())
    case (ctx, ls) :: rest =>
      logger.debug("Case " + ls.mkString(", ") + ":")
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
    if ctx.premises.contains(conclusion) then
      logger.debug("PROVED trivially")
      return Right(())

    if smtProves(conclusion)(using ctx) then
      logger.debug("PROVED by SMT")
      return Right(())

    for mc <- config.metrics do
      mc.timeStart("time/verif/narrow")
    val narrower = new Narrower
    val ctx1 = narrower.narrow(ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/narrow")
    val noneStr = ctx1.premises.collectFirst { case TypeTest(e, LangType(RegEx.RENone)) => e }
    if noneStr.isDefined then
      logger.debug(s"PROVED by ${noneStr.get} : ∅ after type narrowing")
      return Right(())

    conclusion match
      case TypeTest(e, t) =>
        for mc <- config.metrics do
          mc.timeStart("time/verif/type")
        val result = checkType(e, t)(using ctx1)
        for mc <- config.metrics do
          mc.timePause("time/verif/type")
        result match
          case Left(ta) => Left(ta.toString)
          case Right(_) => Right(())
      case Const(false) =>
        proveWithLemmas(conclusion, ctx1.premises, ctx1)
      case _ =>
        proveWithLemmas(conclusion, List(conclusion), ctx1)
          .orElse(proveWithLemmas(conclusion, ctx1.premises, ctx1))

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

  private val syn = new LemmaSynth

  private def proveWithLemmas(conclusion: Expr, seeds: List[Expr], ctx: PrfCtx): Either[String, Unit] =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val sketches = seeds.flatMap(collectSketches(_, ctx)).distinct
    val lemmas = syn.synth(sketches)(using ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")

    if lemmas.nonEmpty then
      logger.debug("Lemmas: " + lemmas.mkString(", "))
      for mc <- config.metrics do
        mc.count("lemmas", lemmas.length)

      if lemmas.flatMap(conjuncts).contains(conclusion) then
        logger.debug(s"PROVED by lemmas")
        return Right(())

      if smtProves(conclusion)(using ctx ++ lemmas) then
        logger.debug(s"PROVED by lemmas + SMT")
        return Right(())

    // Otherwise: not proved
    Left("")

  private def collectSketches(seed: Expr, ctx: PrfCtx): List[syn.Sketch] =
    val ss = ListBuffer.empty[syn.Sketch]
    seed.collect:
      case Cmp(op@(EQ | NE), ec@CharAt(es, ei@Var(_)), Const(t: String)) if t.length == 1 =>
        val c = t.head
        val cs = op match
          case EQ => CharSet(c)
          case NE => CharSet.not(c)
        ss += syn.InferLang(ec, target = Some(t))
        ss += syn.InferIndexCharAt(es, ei, cs)
      case Cmp(EQ | NE, es, Const(t: String)) =>
        ss += syn.InferLang(es, target = Some(t))
      case Cmp(EQ | NE, es1, es2) if es1.sort == Sort.S && es2.sort == Sort.S =>
        ss += syn.InferLang(es1)
        ss += syn.InferLang(es2)
      case Cmp(_, ei@Var(_), Find(es, Const(t: String))) if t.length == 1 =>
        ss += syn.InferIndexCmpFind(ei, es, t.head)
      case t: StrTest =>
        ss += syn.InferTest(t)
      case Length(es) =>
        ss += syn.InferLength(es)
      case Find(es, Const(t: String)) =>
        ss += syn.InferFind(es, t)
    ss.toList
