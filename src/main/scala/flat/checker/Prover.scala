package flat.checker

import flat.Config
import flat.checker.core.*

import scala.annotation.tailrec

class Prover(path: os.Path)(using types: Types, config: Config) extends VCPrf(path):
  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    // Trivial case: context is inconsistent
    if smtSolver.canProve(Const(false)) then
      return Right(Setting(withSMT = true))

    // Split the conclusion into multiple subgoals and prove each
    proveSubgoals(split(conclusion, ctx), 1, Setting())

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
  private def proveSubgoals(subgoals: List[(Expr, PrfCtx)],
                            start: Int, acc: Setting): Either[String, Setting] = subgoals match
    case Nil => Right(acc)
    case (e, ctx) :: rest =>
      logger.debug(s"Subgoal $start ⇒ $e")
      proveSubgoal(e)(using ctx) match
        case Left(err) =>
          logger.debug(s"[X] FAILED")
          Left(err)
        case Right(setting) => proveSubgoals(rest, start + 1, acc | setting)

  private def proveSubgoal(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    // Immediate: conclusion already occurs as a hypothesis
    if ctx.hypotheses.contains(conclusion) then
      return Right(Setting())
    // Trivial for SMT solving
    if smtSolver.canProve(conclusion) then
      return Right(Setting(withSMT = true))
    // Nontrivial
    proveNontrivial(conclusion, ctx)

  private def proveNontrivial(conclusion: Expr, ctx: PrfCtx): Either[String, Setting] =
    val candidates = ctx.destructCandidates
    if candidates.isEmpty then
      return proveCase(conclusion)(using ctx)

    val m = candidates.groupBy(similarity(conclusion, _))
    val bs = m(m.keySet.max)
    logger.debug("Destruct: " + bs.mkString(", "))
    val cases = ctx.destruct(bs)
    proveCases(conclusion, cases, 1, Setting())

  private def similarity(conclusion: Expr, hypothesis: Expr): Int =
    val xs = conclusion.collectVars
    (hypothesis.collectVars & xs).size

  @tailrec
  private def proveCases(conclusion: Expr, ctxs: List[PrfCtx],
                         start: Int, acc: Setting): Either[String, Setting] = ctxs match
    case Nil => Right(acc)
    case ctx :: rest =>
      logger.debug(s"Case $start: $ctx ⇒ $conclusion")
      proveCase(conclusion)(using ctx) match
        case Left(err) =>
          // Try destruct more
          val destructMore = ctx.hypotheses.exists:
            case _: Or | Ite => true
            case _ => false
          if destructMore then
            logger.debug("destruct more")
            proveNontrivial(conclusion, ctx) match
              case Left(err) =>
                logger.debug(s"[X] FAILED")
                Left(err)
              case Right(setting) => Right(setting)
          else
            logger.debug(s"[X] FAILED")
            Left(err)
        case Right(setting) => proveCases(conclusion, rest, start + 1, acc | setting)

  private def proveCase(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    // Trivial case: conclusion already occurs as a hypothesis
    if ctx.hypotheses.contains(conclusion) then
      return Right(Setting())
    // Trivial case: simple LIA problem
    if smtSolver.canProve(conclusion) then
      return Right(Setting(withSMT = true))
    // Type narrowing
    val ctx1 = Refiner.refine(ctx)
    if ctx1.canTriviallyProve(conclusion) then
      return Right(Setting(withHints = true))
    // Nontrivial
    conclusion match
      case TypeTest(e, t) => check(e, t)(using ctx1)
      case e =>
        proveWithLemmas(conclusion, List(conclusion))(using ctx1) match
          case Right(setting) => Right(setting)
          case Left(_) => proveWithLemmas(conclusion, ctx1.assumptions)(using ctx1)

  private def check(expr: Expr, expected: Type)(using ctx: PrfCtx): Either[String, Setting] =
    checkType(expr, expected)(using ctx) match
      case Right(_) => Right(Setting(withHints = true))
      case Left(actual) => Left(actual.toString)

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
      throw UnsupportedOperationException(s"check $expr : $typ")

  override def inferType(goal: Formula.InferType)(using ctx: PrfCtx): Unit =
    val inferer = new Inferer
    val r = inferer.inferLang(goal.value)
    issuer.report(TypeInferred(r.toString, goal.loc))

  private val smtSolver = new SMTSolver

  private def proveWithLemmas(conclusion: Expr, seeds: List[Expr])(using ctx: PrfCtx): Either[String, Setting] =
    val synth = new HintSynth
    val lemmas = seeds.flatMap(synth.collectHints).distinct
    if lemmas.nonEmpty then
      val ctx1 = ctx ++ lemmas
      logger.debug("lemmas: " + lemmas.mkString(" ∧ "))
      if ctx1.canTriviallyProve(conclusion) then
        return Right(Setting(withHints = true))
      if smtSolver.canProve(conclusion)(using ctx1) then
        return Right(Setting(withHints = true, withSMT = true))

    // Otherwise: not proved
    Left("")