package flat.checker

import flat.Config
import flat.checker.core.*

class Prover(path: os.Path)(using types: Types, config: Config) extends VCPrf(path):
  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    if ctx.canTriviallyProve(conclusion) then
      return Right(Setting())

    conclusion match
      case And(e1, e2) =>
        for config1 <- process(e1); config2 <- process(e2) yield config1 | config2
      case Or(e1, e2) =>
        process(e2)(using ctx + Not(e1))
      case e =>
        e.collectFirst { case ite: Ite => ite } match
          case Some(Ite(cond, _, _)) =>
            val (ctx1, ctx2) = ctx.destruct(cond)
            val e1 = e.transform { case Ite(b, e, _) if b == cond => e }
            val e2 = e.transform { case Ite(b, _, e) if b == cond => e }
            for config1 <- process(e1)(using ctx1); config2 <- process(e2)(using ctx2) yield config1 | config2
          case None => processAtomic(e)

  private def processAtomic(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    logger.debug(s"SubGoal: $ctx ⇒ $conclusion")
    val result = conclusion match
      case TypeTest(e, t) => processTypeTest(e, t)
      case e => processProp(e)
    val result1 = result match
      case Left(_) =>
        logger.debug("Try destruct")
        ctx.tryDestruct match
          case Some((ctx1, ctx2)) =>
            for
              config1 <- processAtomic(conclusion)(using ctx1)
              config2 <- processAtomic(conclusion)(using ctx2)
            yield config1 | config2
          case None => result
      case _ => result
    result1 match
      case Left(_) => logger.debug("SubGoal FAILED")
      case Right(_) => logger.debug("SubGoal proved")
    result1

  private def processTypeTest(expr: Expr, expected: Type)(using ctx: PrfCtx): Either[String, Setting] =
    if ctx.canTriviallyProve(Const(false)) then
      return Right(Setting())

    val ctx1 = Refiner.refine(ctx)
    if ctx1.canTriviallyProve(Const(false)) then
      return Right(Setting(withHints = true))

    checkType(expr, expected)(using ctx1) match
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

  private def processProp(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    // First try: without any hints
    if ctx.canTriviallyProve(conclusion) then
      return Right(Setting())
    if smtSolver.canProve(conclusion) then
      return Right(Setting(withSMT = true))

    // Second try: with refinement
    val ctx1 = Refiner.refine(ctx)
    if ctx1.canTriviallyProve(conclusion) then
      return Right(Setting(withHints = true))
    if smtSolver.canProve(conclusion)(using ctx1) then
      return Right(Setting(withHints = true, withSMT = true))

    // Last try: with hints
    val seeds = collectSeeds(conclusion).distinct
    val synth = HintSynth(using ctx1)
    val hints = seeds.flatMap(synth.collectHints).distinct
    if hints.nonEmpty then
      val ctx2 = ctx1 ++ hints
      logger.debug("hints: " + hints.mkString(" ∧ "))
      if ctx2.canTriviallyProve(conclusion) then
        return Right(Setting(withHints = true))
      if smtSolver.canProve(conclusion)(using ctx2) then
        return Right(Setting(withHints = true, withSMT = true))

    // Otherwise: not proved
    Left("")

  private def collectSeeds(conclusion: Expr)(using ctx: PrfCtx): List[Expr] = conclusion :: ctx.assumptions