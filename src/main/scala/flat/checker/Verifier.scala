package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.ast.*
import flat.regex.{CharSet, RegEx}
import flat.{Config, Ops}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Verifier: prove goals or answer type queries. */
final class Verifier(using config: Config, types: Types) extends LazyLogging:
  private val smtSolver = new SMTSolver
  private var mostRecentCtx = PrfCtx.empty(using types, smtSolver)
  private val cachedPremises = ListBuffer.empty[Expr]
  private val ctxStack = mutable.Stack.empty[PrfCtx]

  /** Gets the current proof context. */
  def getCtx: PrfCtx =
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

  inline def locally[T](f: => T): T =
    push()
    val result = f
    pop()
    result

  /** Command assume. */
  def assume(cond: Expr): Unit =
    cachedPremises += cond
    smtSolver.assume(cond)

  /** Infers the type of `value` under `ctx`. */
  def infer(value: Expr): RegEx =
    for mc <- config.metrics do
      mc.timeStart("time/verif/type")
    val inferer = new Inferer(using ctx = getCtx)()
    val r = inferer.inferLang(value)
    for mc <- config.metrics do
      mc.timePause("time/verif/type")
    r

  /** Proves that `value` has the `expected` type under `ctx`. */
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

  /** Proves that `conclusion` is valid under `ctx`. */
  def prove(conclusion: Expr): Boolean =
    tryNaive(conclusion) match
      case Right(msg) =>
        logger.debug(s"PROVED by $msg")
        true
      case _ =>
        trySplit(conclusion)

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
      mc.timeStart("time/verif/smt")
    val valid = smtSolver.proves(conclusion)
    for mc <- config.metrics do
      mc.timePause("time/verif/smt")
    if valid then Right("SMT") else Left(())

  private def trySplit(conclusion: Expr): Boolean = conclusion match
    case And(b1, b2) =>
      val result1 = trySplit(b1)
      if !result1 then
        return false
      trySplit(b2)
    case or: Or =>
      val bs = or.disjuncts
      for b <- bs.dropRight(1) do assume(Not(b))
      trySplit(bs.last)
    case _ =>
      conclusion.collectFirst { case ite: Ite => ite } match
        case Some(Ite(b, _, _)) =>
          // Case 1: b is true
          val e1 = conclusion.transform { case Ite(e0, e1, _) if e0 == b => e1 }
          val result1 = locally { caseIf(b, true); trySplit(e1) }
          if !result1 then
            return false
          // Case 2: b is false
          val e2 = conclusion.transform { case Ite(e0, _, e2) if e0 == b => e2 }
          locally { caseIf(b, false); trySplit(e2) }
        case None =>
          tryNaive(conclusion) match
            case Right(msg) =>
              logger.debug(s"PROVED by $msg after split")
              true
            case Left(_) =>
              if getCtx.destructibleCandidates.isEmpty then
                tryNarrowAndLemmas(conclusion) match
                  case Right(msg) =>
                    logger.debug(s"PROVED by $msg")
                    true
                  case Left(_) => false
              else
                tryCases(conclusion)

  private enum Pat:
    case IfPat(cond: Expr)
    case OrPat(or: Or)

  import Pat.*

  private def tryCases(conclusion: Expr): Boolean =
    // Decide which premises to perform case analysis.
    val candidates = getCtx.destructibleCandidates
    val similarities = candidates.map(similarity(conclusion, _))
    val maxSimilarity = similarities.max
    val selected =
      for
        (p, x) <- candidates.zip(similarities)
        if x == maxSimilarity
      yield p
    // Collect patterns and destruct the cases.
    val cases = ListBuffer.empty[Pat]
    selected.foreach:
      case or: Or => cases += OrPat(or)
      case b => b.traverse { case Ite(cond, _, _) => cases += IfPat(cond) }
    tryDestruct(cases.toList, conclusion)

  private def similarity(conclusion: Expr, premise: Expr): Int =
    (premise.collectVars & conclusion.collectVars).size

  private def tryDestruct(pats: List[Pat], conclusion: Expr): Boolean = pats match
    case Nil =>
      tryNaive(conclusion) match
        case Right(msg) =>
          logger.debug(s"PROVED by $msg after split")
          true
        case Left(_) =>
          tryNarrowAndLemmas(conclusion) match
            case Right(msg) =>
              logger.debug(s"PROVED by $msg")
              true
            case Left(_) if getCtx.destructibleCandidates.nonEmpty =>
              // Try destruct again.
              logger.debug("Try destruct more hypotheses")
              tryCases(conclusion)
            case _ =>
              logger.debug(s"[X] Case FAILED")
              false
    case IfPat(b) :: rest =>
      // Case 1: b is true
      val result1 = locally { caseIf(b, true); tryDestruct(rest, conclusion) }
      if !result1 then
        return false
      // Case 2: b is false
      locally { caseIf(b, false); tryDestruct(rest, conclusion) }
    case OrPat(or) :: rest =>
      or.disjuncts.indices.forall(k => locally { caseOr(or, k); tryDestruct(rest, conclusion) })

  private inline def caseIf(cond: Expr, isTrue: Boolean): Unit =
    mostRecentCtx = getCtx.caseIf(cond, isTrue)
    smtSolver.assume(if isTrue then cond else Not(cond))

  private inline def caseOr(or: Or, k: Int): Unit =
    mostRecentCtx = getCtx.caseOr(or, k)
    val bs = or.disjuncts
    val assumptions = bs.take(k).map(Not(_)) :+ bs(k)
    assumptions.foreach(smtSolver.assume)

  private def tryNarrowAndLemmas(conclusion: Expr): Either[Unit, String] =
    // Perform type narrowing.
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
        withLemmas(conclusion, getCtx.premises)
      case _ =>
        withLemmas(conclusion, List(conclusion))
          .orElse(withLemmas(conclusion, getCtx.premises))

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
