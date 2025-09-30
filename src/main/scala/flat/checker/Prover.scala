package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.conjuncts
import flat.checker.core.*
import flat.regex.{CharSet, RegEx}
import flat.{Config, Ops}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Prover: prove verification conditions, or answer type queries. */
final class Prover(using config: Config, types: Types) extends LazyLogging:
  /** Proves that `conclusion` is valid under `ctx`. */
  def prove(conclusion: Expr, ctx: PrfCtx): Boolean =
    // Above all, try naive prover.
    naive(conclusion)(using ctx) match
      case Right(kind) =>
        logger.debug("PROVED by " + (if kind == 0 then "naive" else "SMT"))
        return true
      case _ =>

    // Split the conclusion into multiple goals and try naive prover on each of them.
    val attempts = for (c, ctx, ls) <- split(conclusion, ctx, Nil) yield (c, ctx, ls, naive(c)(using ctx))
    val (success, failure) = attempts.partition(_._4.isRight)
    for (c, ctx, ls, res) <- success do
      logger.debug("Goal " + ls.mkString(", ") + s" ⇒ $c")
      logger.debug("PROVED by " + (if res == Right(0) then "naive" else "SMT" + " after split"))

    // Try destruct and lemmas for each failing goals.
    val results = for (c, ctx, ls, _) <- failure yield
      logger.debug("Goal " + ls.mkString(", ") + s" ⇒ $c")
      if ctx.destructCandidates.nonEmpty then destruct(c, ctx) else narrowAndInfer(c, ctx)
    results.forall(_ == true)

  private val smtSolver = new SMTSolver

  private def naive(conclusion: Expr)(using ctx: PrfCtx): Either[Unit, Int] =
    if ctx.premises.contains(conclusion) then
      return Right(0)

    for mc <- config.metrics do
      mc.count("smt queries")
      mc.timeStart("time/verif/smt")
    val valid = smtSolver.proves(conclusion)(using ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/smt")
    if valid then Right(1) else Left(())

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

  private def destruct(conclusion: Expr, ctx: PrfCtx): Boolean =
    // Decide which premises to destruct.
    val ps = ctx.destructCandidates
    val ys = ps.map(similarity(conclusion, _))
    val y = ys.max
    val dps =
      for
        (p, x) <- ps.zip(ys)
        if x == y
      yield p

    // Destruct the context into multiple and try naive prover on each.
    logger.debug("Destruct: " + dps.mkString(", "))
    val attempts = for (ctx, ls) <- ctx.destruct(dps) yield (ctx, ls, naive(conclusion)(using ctx))
    val (success, failure) = attempts.partition(_._3.isRight)
    for (ctx, ls, res) <- success do
      logger.debug("Case " + ls.mkString(", ") + ":")
      logger.debug("PROVED by " + (if res == Right(0) then "naive" else "SMT" + " after destruct"))

    // Try type narrowing and inference for each failing goals.
    val attempts1 = for (ctx, ls, _) <- failure yield (ctx, ls, narrowAndInfer(conclusion, ctx))
    val (success1, failure1) = attempts1.partition(_._3 == true)
    for (ctx, ls, _) <- success1 do
      logger.debug("Case " + ls.mkString(", ") + ":")
      logger.debug("PROVED by lemmas")

    // If there are still failing goals, try destruct again.
    val results = for (ctx, ls, _) <- failure1 yield
      logger.debug("Case " + ls.mkString(", ") + ":")
      if ctx.destructCandidates.nonEmpty then
        logger.debug("Try destruct more hypotheses")
        destruct(conclusion, ctx)
      else
        logger.debug(s"[X] Case FAILED")
        false
    results.forall(_ == true)

  private def similarity(conclusion: Expr, hypothesis: Expr): Int =
    (hypothesis.collectVars & conclusion.collectVars).size

  private def narrowAndInfer(conclusion: Expr, ctx: PrfCtx): Boolean =
    // Perform type narrowing.
    for mc <- config.metrics do
      mc.timeStart("time/verif/narrow")
    val narrower = new Narrower
    val ctx1 = narrower.narrow(ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/narrow")

    // Try finding inconsistency.
    val noneStr = ctx1.premises.collectFirst { case TypeTest(e, LangType(RegEx.RENone)) => e }
    if noneStr.isDefined then
      logger.debug(s"PROVED by ${noneStr.get} : ∅ after type narrowing")
      return true

    conclusion match
      case TypeTest(e, t) =>
        for mc <- config.metrics do
          mc.timeStart("time/verif/type")
        val result = checkType(e, t)(using ctx1)
        for mc <- config.metrics do
          mc.timePause("time/verif/type")
        result.isRight
      case Const(false) =>
        withLemmas(conclusion, ctx1.premises, ctx1)
      case _ =>
        withLemmas(conclusion, List(conclusion), ctx1) || withLemmas(conclusion, ctx1.premises, ctx1)

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

  private def withLemmas(conclusion: Expr, seeds: List[Expr], ctx: PrfCtx): Boolean =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val sketches = seeds.flatMap(collectSketches(_, ctx)).distinct
    val lemmas = syn.synth(sketches)(using ctx)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")

    if lemmas.nonEmpty then
      //      logger.debug("Lemmas: " + lemmas.mkString(", "))
      for mc <- config.metrics do
        mc.count("lemmas", lemmas.length)

      if lemmas.flatMap(conjuncts).contains(conclusion) then
        //        logger.debug(s"PROVED by lemmas")
        return true

      if naive(conclusion)(using ctx ++ lemmas).isRight then
        //        logger.debug(s"PROVED by lemmas + SMT")
        return true

    // Otherwise: not proved
    false

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

  /** Proves that `value` has the `expected` type under `ctx`. */
  def check(value: Expr, expected: Type, ctx: PrfCtx): Either[String, Unit] =
    if prove(TypeTest(value, expected), ctx) then
      return Right(())

    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = ctx)
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    Left(r.toString)

  /** Infers the type of `value` under `ctx`. */
  def infer(value: Expr, ctx: PrfCtx): RegEx =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = ctx)
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    r
