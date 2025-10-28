package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.Printer.*
import flat.checker.ast.*
import flat.regex.{CharSet, RegEx}
import flat.{Config, Issuer, Ops}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Verifier: prove goals or answer type queries. */
final class Verifier(using config: Config, types: Types, issuer: Issuer) extends LazyLogging:
  def verify(vc: VC): Boolean = if config.nonInc then verifyNonInc(vc, Nil) else verifyInc(vc)

  private var nextGoal = 1

  private def verifyInc(vc: VC): Boolean = vc match
    case VCTrue => true
    case VCImp(b, vc) => assume(b); verifyInc(vc)
    case VCGroup(sides, mains) =>
      // Prove all side conditions first: if any fails, immediately give up this group.
      val result1 = sides.forall(vc => locally { verifyInc(vc) })
      if !result1 then
        return false
      // Prove all main goals: even if any fails, still try others to collect more error messages.
      mains.map(vc => locally { verifyInc(vc) }).forall(_ == true)
    case _ => verifyGoal(vc)

  private def verifyNonInc(vc: VC, premises: List[Expr]): Boolean = vc match
    case VCTrue => true
    case VCImp(b, vc) => verifyNonInc(vc, b :: premises)
    case VCGroup(sides, mains) =>
      // Prove all side conditions first: if any fails, immediately give up this group.
      val result1 = sides.forall(vc => verifyNonInc(vc, premises))
      if !result1 then
        return false
      // Prove all main goals: even if any fails, still try others to collect more error messages.
      mains.map(vc => verifyNonInc(vc, premises)).forall(_ == true)
    case _ =>
      locally { premises.foreach(assume); verifyGoal(vc) }

  private def verifyGoal(vc: VC): Boolean = vc match
    case vc: VCInfer =>
      logger.info("")
      logger.info("Goal {}: {} ⇒ {} : ?", nextGoal, ppCtx(getCtx), ppExpr(vc.value))
      val r = infer(vc.value)
      issuer.report(TypeInferred(ppRE(r), vc.loc))
      nextGoal += 1
      true
    case vc: VCType =>
      logger.info("")
      logger.info("Goal {}: {} ⇒ {} : {}", nextGoal, ppCtx(getCtx), ppExpr(vc.value), ppType(vc.expected))
      for mc <- config.metrics do
        mc.push("goals")
        mc.put("#", nextGoal)
        mc.put("kind", vc.getClass.toString)
        mc.timeStart("time/verif")
      val succeed = check(vc.value, vc.expected) match
        case Right(_) => true
        case Left(msg) =>
          issuer.report(vc.diagnostic(msg))
          logger.info(s"Goal {} NOT PROVED", nextGoal)
          false
      for mc <- config.metrics do
        mc.timePause("time/verif")
        mc.put("succeed", succeed)
        mc.pop()
      nextGoal += 1
      succeed
    case g: VCGoal =>
      logger.info("")
      logger.info(s"Goal {}: {} ⇒ {}", nextGoal, ppCtx(getCtx), ppVCGoal(g))
      for mc <- config.metrics do
        mc.push("goals")
        mc.put("#", nextGoal)
        mc.put("kind", g.getClass.toString)
        mc.timeStart("time/verif")
      val succeed = prove(g.cond)
      for mc <- config.metrics do
        mc.timePause("time/verif")
        mc.put("succeed", succeed)
        mc.pop()
      if !succeed then
        issuer.report(g.diagnostic(""))
        logger.info(s"Goal {} NOT PROVED", nextGoal)
      nextGoal += 1
      succeed
    case _ => assert(false)

  private val smtSolver = new SMTSolver
  private var mostRecentCtx = PrfCtx.empty(using types, smtSolver)
  private val cachedPremises = ListBuffer.empty[Expr]
  private val ctxStack = mutable.Stack.empty[PrfCtx]

  /** Gets the current proof context. */
  private def getCtx: PrfCtx =
    if cachedPremises.nonEmpty then
      mostRecentCtx = mostRecentCtx ++ cachedPremises.toList
      cachedPremises.clear()
    mostRecentCtx

  /** Command push. */
  def push(): Unit =
    ctxStack.push(getCtx)
    smtSolver.push()

  /** Command pop. */
  def pop(): Unit =
    cachedPremises.clear()
    mostRecentCtx = ctxStack.pop()
    smtSolver.pop()

  private inline def locally[T](f: => T): T =
    push()
    val result = f
    pop()
    result

  /** Command assume. */
  def assume(cond: Expr): Unit =
    cachedPremises += cond
    for mc <- config.metrics do
      mc.timeStart("time/verif/smt/assume")
    smtSolver.assume(cond)
    for mc <- config.metrics do
      mc.timePause("time/verif/smt/assume")

  /** Infers the type of `value`. */
  def infer(value: Expr): RegEx =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = getCtx)()
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    r

  /** Proves that `value` has the `expected` type. */
  def check(value: Expr, expected: Type): Either[String, Unit] =
    if prove(TypeTest(value, expected)) then
      return Right(())

    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = getCtx)()
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    Left(r.toString)

  /** Proves that `conclusion` is valid. */
  def prove(conclusion: Expr): Boolean =
    tryNaive(conclusion) match
      case Right(msg) => logger.debug(s"PROVED by $msg"); true
      case _ => trySplit(conclusion)

  private def tryNaive(conclusion: Expr): Either[Unit, String] =
    if getCtx.premises.contains(conclusion) then
      return Right("naive")

    conclusion match
      case Cmp(op, Const(n1: Int), Const(n2: Int)) =>
        if op.eval(n1, n2) then
          return Right("naive")
      case _ =>

    for mc <- config.metrics do
      mc.count("smt queries")
      mc.timeStart("time/verif/smt/prove")
    val valid = smtSolver.proves(conclusion)
    for mc <- config.metrics do
      mc.timePause("time/verif/smt/prove")
    if valid then Right("SMT") else Left(())

  private def trySplit(conclusion: Expr): Boolean =
    val goals = split(conclusion)
    if goals.length == 1 then tryDestruct(conclusion, 1) else goals.forall(proveCGoal(_, 1))

  private enum Guard:
    case If(cond: Expr, value: Boolean)
    case Case(or: Or, choice: Int)

    override def toString: String = this match
      case If(b, true) => "if " + ppExpr(b)
      case If(b, false) => "if not " + ppExpr(b)
      case Case(or, k) => "case " + ppExpr(or.disjuncts(k)) + s" ($k)"

  private def assume(guard: Guard): Unit = guard match
    case Guard.If(cond, isTrue) =>
      mostRecentCtx = getCtx.caseIf(cond, isTrue)
      smtSolver.assume(if isTrue then cond else Not(cond))
    case Guard.Case(or, k) =>
      mostRecentCtx = getCtx.caseOr(or, k)
      val bs = or.disjuncts
      val assumptions = bs(k) :: bs.take(k).map(Not(_))
      assumptions.foreach(smtSolver.assume)

  private enum CGoal:
    case Just(conclusion: Expr)
    case Imp(conds: List[Expr], goals: List[CGoal])
    case Guarded(guard: Guard, goals: List[CGoal])

  private def split(conclusion: Expr): List[CGoal] = conclusion match
    case and: And => and.conjuncts.flatMap(split)
    case or: Or =>
      val bs = or.disjuncts
      List(CGoal.Imp(bs.dropRight(1).map(Not(_)), split(bs.last)))
    case _ =>
      conclusion.collectFirst { case ite: Ite => ite } match
        case Some(Ite(b, _, _)) =>
          val c1 = conclusion.transform { case Ite(e0, e1, _) if e0 == b => e1 }
          val c2 = conclusion.transform { case Ite(e0, _, e2) if e0 == b => e2 }
          List(CGoal.Guarded(Guard.If(b, true), split(c1)), CGoal.Guarded(Guard.If(b, false), split(c2)))
        case None => List(CGoal.Just(conclusion))

  private def proveCGoal(goal: CGoal, level: Int): Boolean = goal match
    case CGoal.Just(c) =>
      logger.debug("⇒ {}", ppExpr(c))
      tryNaive(c) match
        case Right(msg) => logger.debug(s"PROVED by $msg after split"); true
        case Left(_) => tryDestruct(c, level)
    case CGoal.Imp(bs, gs) =>
      logger.debug("{} assume {}", "-" * level, bs.map(ppExpr).mkString(" ∧ "))
      locally { bs.foreach(assume); gs.forall(proveCGoal(_, level)) }
    case CGoal.Guarded(guard, gs) =>
      logger.debug("{} {}", "-" * level, guard)
      locally { assume(guard); gs.forall(proveCGoal(_, level + 1)) }

  private def tryDestruct(conclusion: Expr, level: Int): Boolean =
    val candidates = getCtx.destructibleCandidates
    if candidates.isEmpty then
      return tryNarrowAndLemmas(conclusion).isRight
    // Decide which premises to perform case analysis.
    val similarities = candidates.map(similarity(conclusion, _))
    val maxSimilarity = similarities.max
    val selected =
      for
        (p, x) <- candidates.zip(similarities)
        if x == maxSimilarity
      yield p
    // Collect goals
    val guardGroups = ListBuffer.empty[List[Guard]]
    selected.foreach:
      case or: Or => guardGroups += or.disjuncts.indices.map(k => Guard.Case(or, k)).toList
      case b => b.traverse { case Ite(cond, _, _) => guardGroups += List(Guard.If(cond, true), Guard.If(cond, false)) }
    proveCases(conclusion, guardGroups.toList, level)

  private def similarity(conclusion: Expr, premise: Expr): Int =
    (premise.collectVars & conclusion.collectVars).size

  private def proveCases(conclusion: Expr, guardGroups: List[List[Guard]], level: Int): Boolean = guardGroups match
    case Nil =>
      tryNaive(conclusion) match
        case Right(msg) =>
          logger.debug(s"PROVED by $msg after destruct"); true
        case Left(_) =>
          tryNarrowAndLemmas(conclusion) match
            case Right(msg) =>
              logger.debug(s"PROVED by $msg")
              true
            case Left(_) if getCtx.destructibleCandidates.nonEmpty =>
              // Try destruct again.
              logger.debug("Try destruct more hypotheses")
              tryDestruct(conclusion, level)
            case _ =>
              logger.debug(s"[X] Case FAILED")
              false
    case group :: rest =>
      group.forall: guard =>
        logger.debug("{} {}", "-" * level, guard)
        locally { assume(guard); proveCases(conclusion, rest, level + 1) }

  private def tryNarrowAndLemmas(conclusion: Expr): Either[Unit, String] =
    // Perform type narrowing.
    logger.trace("narrowing")
    for mc <- config.metrics do
      mc.timeStart("time/verif/narrow")
    val narrower = new Narrower
    val newTypes = narrower.narrow(getCtx)
    for mc <- config.metrics do
      mc.timePause("time/verif/narrow")
    // Try finding inconsistency.
    val noneStr = newTypes.collectFirst { case TypeTest(e, LangType(RegEx.RENone)) => e }
    if noneStr.isDefined then
      return Right(s"contradiction: ${noneStr.get} : ∅")
    // Update context.
    newTypes.foreach(assume)
    conclusion match
      case TypeTest(e, t) =>
        for mc <- config.metrics do
          mc.timeStart("time/verif/type")
        val result = checkType(e, t)
        for mc <- config.metrics do
          mc.timePause("time/verif/type")
        result match
          case Left(_) => Left(())
          case Right(value) => Right("type checking")
      case Const(false) =>
        logger.trace("lemmas (all premises)")
        withLemmas(conclusion, getCtx.premises)
      case _ =>
        logger.trace("lemmas (conclusion)")
        withLemmas(conclusion, List(conclusion)).orElse:
          logger.trace("lemmas (all)")
          withLemmas(conclusion, getCtx.premises)

  private def checkType(expr: Expr, typ: Type): Either[Type, Unit] = typ match
    case LangType(r2) =>
      val inferer = new Inferer(using ctx = getCtx)()
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

  private def withLemmas(conclusion: Expr, seeds: List[Expr]): Either[Unit, String] =
    // Synthesize lemmas.
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val sketches = seeds.flatMap(collectSketches).distinct
    val lemmas = syn.synth(sketches)(using getCtx)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    // Try to prove using these lemmas.
    if lemmas.nonEmpty then
      logger.trace("lemmas: {}", lemmas.map(ppExpr).mkString(", "))
      for mc <- config.metrics do
        mc.count("lemmas", lemmas.length)
      if lemmas.exists(_.conjuncts.contains(conclusion)) then
        return Right("lemmas")
      // Update context.
      lemmas.foreach(assume)
      tryNaive(conclusion) match
        case Right(msg) =>
          return Right(s"lemmas + $msg")
        case _ =>
    Left(())

  private def collectSketches(seed: Expr): List[syn.Sketch] =
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
