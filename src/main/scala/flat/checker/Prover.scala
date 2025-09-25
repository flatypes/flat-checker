package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.conjuncts
import flat.checker.core.*
import flat.regex.RegEx

import scala.annotation.tailrec

/** Prover: prove verification conditions, or answer type queries. */
final class Prover(using config: Config, types: Types) extends LazyLogging:
  /** Proves that `conclusion` is valid under `ctx`. */
  def prove(conclusion: Expr, ctx: PrfCtx): Either[String, Unit] =
    if smtSolver.canProve(Const(false))(using ctx) then
      logger.debug("PROVED by contradiction")
      return Right(())

    // Split the conclusion into multiple goals and prove each of them
    proveGoals(split(conclusion, ctx))

  /** Proves that `value` has the `expected` type under `ctx`. */
  def check(value: Expr, expected: Type, ctx: PrfCtx): Either[String, Unit] =
    prove(TypeTest(value, expected), ctx)

  /** Infers the type of `value` under `ctx`. */
  def infer(value: Expr, ctx: PrfCtx): RegEx =
    val inferer = new Inferer(using ctx = ctx)
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

    val narrower = new Narrower
    val ctx1 = narrower.narrow(ctx)
    val noneStr = ctx1.hypotheses.collectFirst { case TypeTest(e, LangType(RegEx.RENone)) => e }
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
          .orElse(proveWithLemmas(conclusion, ctx1.hypotheses, ctx1))

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
  private val syn = new LemmaSynth

  private def proveWithLemmas(conclusion: Expr, seeds: List[Expr], ctx: PrfCtx): Either[String, Unit] =
    val sketches = seeds.flatMap(collectSketches(_, ctx)).distinct
    val lemmas = syn.synth(sketches)(using ctx)
    if lemmas.nonEmpty then
      logger.debug("Lemmas: " + lemmas.mkString(", "))
      if lemmas.flatMap(_.conjuncts).contains(conclusion) then
        logger.debug(s"PROVED by lemmas")
        return Right(())

      if smtSolver.canProve(conclusion)(using ctx ++ lemmas) then
        logger.debug(s"PROVED by lemmas + SMT")
        return Right(())

    // Otherwise: not proved
    Left("")

  extension (expr: Expr)
    // NOTE: incomplete
    def getSort: Sort = expr match
      case CharAt(_, _) => Sort.String
      case Substr(_, _, _) => Sort.String
      case _ => Sort.Bot

  private def collectSketches(seed: Expr, ctx: PrfCtx): List[syn.Sketch] =
    val ss = seed.collect:
      case Cmp(op@(EQ | NE), ec@CharAt(es, ei@Var(_)), Const(t: String)) if t.length == 1 =>
        val c = t.head
        List(syn.InferLang(ec, target = Some(t)), syn.InferIndex(op, es, ei, c))
      case Cmp(EQ | NE, es, Const(t: String)) => List(syn.InferLang(es, target = Some(t)))
      case Cmp(EQ | NE, es1, es2) if es1.getSort == Sort.String && es2.getSort == Sort.String =>
        List(syn.InferLang(es1), syn.InferLang(es2))
      case Cmp(_, ei@Var(_), Find(es, Const(t: String))) if t.length == 1 =>
        List(syn.InferIndexCmpFind(ei, es, t.head))
      case e@PrefixOf(_, _) => List(syn.InferTest(e))
      case e@InfixOf(_, _) => List(syn.InferTest(e))
      case e@SuffixOf(_, _) => List(syn.InferTest(e))
      case Length(es) => List(syn.InferLength(es))
      case Find(es, Const(t: String)) => List(syn.InferFirstIndexOf(es, t))
    ss.flatten