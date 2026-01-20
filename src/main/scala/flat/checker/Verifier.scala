package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.Printer.*
import flat.checker.ast.*
import flat.regex.{CharSet, RegEx}
import flat.util.allRight
import flat.{Config, Issuer, Ops}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class VarCtx private(vars: Map[String, Sort]):
  def getSort(name: String): Sort =
    val i = name.indexOf('@')
    val x = if i >= 0 then name.substring(0, i) else name
    vars(x)

  def strVars: Set[String] = vars.filter(_._2.base == StrSort).keySet

object VarCtx:
  def from(funDef: FunDef): VarCtx = VarCtx(funDef.lCtx.view.mapValues(_.base).toMap)

  def from(vars: (String, Sort)*): VarCtx = VarCtx(vars.toMap)

/** Proof Context. Recently added premises first. */
class PrfCtx private(val premises: List[Expr])(using prover: Verifier#Prover) extends LazyLogging:
  val varCtx: VarCtx = prover.varCtx

  /** Returns the RE of the given `str`. */
  def getLang(str: Expr): RegEx =
    val result = premises.collectFirst:
      case TypeTest(e, LangType(r)) if e == str => r
      case StrIn(e, r) if e == str => r
    result.getOrElse:
      str match
        case Var(x) if prover.varCtx.getSort(x) == StrSort => RegEx.all
        case _ => throw IllegalArgumentException(s"regex not found: $str")

  /** If there is a premise that tells the RE of some suffix of `str`, i.e., `str[i:]` has type `r`,
   * then returns this base index `i` and the RE `r`. */
  def lookupSuffixLang(str: Expr): Option[(Expr, RegEx)] =
    premises.collectFirst:
      case TypeTest(suffix@Substr(e1, ei, Length(e2)), LangType(r)) if e1 == str && e2 == str => (ei, r)
      case StrIn(suffix@Substr(e1, ei, Length(e2)), r) if e1 == str && e2 == str => (ei, r)

  /** Creates a new proof context with given conditions added. */
  def ++(conds: List[Expr]): PrfCtx = PrfCtx(conds.map(simpl).flatMap(conjuncts) ++ premises)

  inline def +(cond: Expr): PrfCtx = ++(List(cond))

  def isValid(conclusion: Expr): Boolean = premises.contains(conclusion) || prover.naive(conclusion)

object PrfCtx:
  /** The empty proof context. */
  def empty(using prover: Verifier#Prover) = PrfCtx(Nil)

  def from(conds: List[Expr])(using rover: Verifier#Prover): PrfCtx =
    PrfCtx(conds.map(simpl).flatMap(conjuncts))

/** Verifier: prove goals or answer type queries. */
final class Verifier(using config: Config, issuer: Issuer) extends LazyLogging:
  def verify(vc: VC)(using varCtx: VarCtx): Unit =
    count = 0
    discharge(vc, Nil)

  private var count = 0

  private def discharge(vc: VC, premises: List[Expr])(using varCtx: VarCtx): Boolean = vc match
    case VCTrue => true
    case VCImp(b, vc) => discharge(vc, premises :+ b)
    case VCGroup(sides, mains) =>
      // Prove all side conditions first: if any fails, immediately give up this group.
      val result1 = sides.zipWithIndex.forall { (vc, i) => discharge(vc, premises) }
      if !result1 then
        return false
      // Prove all main goals: even if any fails, still try others to collect more error messages.
      val results = for (vc, i) <- mains.zipWithIndex yield discharge(vc, premises)
      results.forall(_ == true)
    case vc@VCInfer(e) =>
      val prover = new Prover
      premises.foreach(prover.assume)
      count += 1
      logger.info("")
      logger.info(s"Goal {} (VCInfer): {}", count, ppGoal(prover.getCtx.premises, e))
      prover.narrow()
      val inferer = new Inferer(using config, prover.getCtx)
      val r = inferer.inferLang(e)
      issuer.report(TypeInferred(ppRE(r), vc.loc))
      true
    case goal: VCGoal =>
      val prover = new Prover
      premises.foreach(prover.assume)
      count += 1
      logger.info("")
      logger.info(s"Goal {} ({}): {}", count, goal.getClass.getSimpleName, ppGoal(prover.getCtx.premises, goal.cond))
      prover.prove(goal.cond) match
        case Right(_) => true
        case Left(msg) => issuer.report(goal.diagnostic(msg)); false

  private def ppGoal(hypotheses: List[Expr], conclusion: Expr): String =
    val left = if hypotheses.isEmpty then "⊤" else hypotheses.map(ppExpr).mkString(" ∧ ")
    val right = " ⇒ " + ppExpr(conclusion)
    if left.length + right.length <= 65 then left + right // inline
    else if left.length + right.length <= 118 then "\n  " + left + right // one-line
    else if left.length <= 118 then "\n  " + left + "\n  " + right // two-line
    else // multi-line
      var start = 0
      val lines = ListBuffer.empty[String]
      while start + 118 < left.length do
        val s = left.substring(start, start + 118)
        val width = s.lastIndexOf('∧') + 2
        lines += s.take(width)
        start += width
      lines += left.substring(start)
      lines.map("\n  " + _).mkString + "\n  " + right

  final class Prover(using val varCtx: VarCtx):
    private val cachedHypotheses = ListBuffer.empty[Expr]
    private val smtSolver = new SMTSolver(using extractMode = config.extractMode)

    /** Adds the given `hypothesis`. */
    def assume(hypothesis: Expr): Unit =
      cachedHypotheses += hypothesis
      for mc <- config.metrics do
        mc.timeStart("time/verif/smt/assume")
      smtSolver.assume(hypothesis)
      for mc <- config.metrics do
        mc.timePause("time/verif/smt/assume")

    /** Proves the validity of the given `conclusion`. If fails, provides an error message. */
    def prove(conclusion: Expr): Either[String, Unit] =
      if naive(conclusion) then
        logger.debug("PROVED by naive")
        return Right(())

      if canSplit(conclusion) then splitAndProve(conclusion) else tryDestructAndProve(conclusion)

    private var ctx = PrfCtx.empty(using this)

    /** Gets the current proof context. */
    def getCtx: PrfCtx =
      if cachedHypotheses.nonEmpty then
        ctx ++= cachedHypotheses.toList
        cachedHypotheses.clear()
      ctx

    private val ctxStack = mutable.Stack.empty[PrfCtx]

    private def locally[T](f: => T): T =
      ctxStack.push(getCtx)
      smtSolver.push()
      val result = f
      cachedHypotheses.clear()
      ctx = ctxStack.pop()
      smtSolver.pop()
      result

    private def decide(cond: Expr, choice: Boolean): Unit =
      logger.debug("{} decide: {} {}", "-" * ctxStack.size, if choice then "if" else "if not", ppExpr(cond))
      val hs = getCtx.premises.map(_.transform { case Ite(b, e1, e2) if b == cond => if choice then e1 else e2 })
      val hCond = if choice then cond else Not(cond)
      ctx = PrfCtx.from(hs :+ hCond)(using this)
      smtSolver.assume(hCond)

    private def decide(or: Or, choice: Int): Unit =
      val bs = or.disjuncts
      logger.debug("{} decide: case ({}) {}", "-" * ctxStack.size, choice, ppExpr(bs(choice)))
      val hNew = mkAnd(bs(choice) :: bs.take(choice).map(Not(_)))
      val hs = getCtx.premises.map { h => if h == or then hNew else h }
      ctx = PrfCtx.from(hs)(using this)
      smtSolver.assume(bs(choice))

    def naive(conclusion: Expr): Boolean =
      if getCtx.premises.contains(conclusion) then
        return true

      for mc <- config.metrics do
        mc.count("smt queries")
        mc.timeStart("time/verif/smt/prove")
      val valid = smtSolver.proves(conclusion)
      for mc <- config.metrics do
        mc.timePause("time/verif/smt/prove")
      valid

    private def canSplit(conclusion: Expr): Boolean = conclusion match
      case _: And | _: Or => true
      case _ => conclusion.collectFirst { case ite: Ite => ite }.isDefined

    private def splitAndProve(conclusion: Expr): Either[String, Unit] = conclusion match
      case and: And => allRight(and.conjuncts, splitAndProve)
      case or: Or =>
        val bs = or.disjuncts
        locally:
          for b <- bs.init do assume(Not(b))
          splitAndProve(bs.last)
      case _ =>
        conclusion.collectFirst { case ite: Ite => ite } match
          case Some(Ite(b, _, _)) =>
            locally:
              decide(b, true)
              val c = conclusion.transform { case Ite(e0, e1, _) if e0 == b => e1 }
              splitAndProve(c)
            locally:
              decide(b, false)
              val c = conclusion.transform { case Ite(e0, _, e2) if e0 == b => e2 }
              splitAndProve(c)
          case None =>
            logger.debug("⇒ {}", ppExpr(conclusion))
            if naive(conclusion) then
              logger.debug("PROVED by naive after split")
              return Right(())
            tryDestructAndProve(conclusion)

    private def tryDestructAndProve(conclusion: Expr): Either[String, Unit] =
      // Check if there is any destructible hypothesis.
      val hs = getCtx.premises.filter:
        case _: Or => true
        case b => b.collectFirst { case _: Ite => () }.isDefined
      if hs.isEmpty then
        if narrow() then
          logger.debug("PROVED by type narrowing after destruct")
          return Right(())
        return synthAndProve(conclusion)

      // Select those most similar to the conclusion.
      val scores = hs.map(similarity(conclusion, _))
      val scoreMax = scores.max
      val candidates = for (h, score) <- hs.zip(scores); if score == scoreMax yield h
      val conds = mutable.Set.empty[Expr]
      val ors = mutable.Set.empty[Or]
      candidates.foreach:
        case or: Or => ors += or
        case h => h.traverse { case Ite(b, _, _) => conds += b }
      destructAndProve(conds.toList, ors.toList, conclusion)

    private def similarity(conclusion: Expr, premise: Expr): Int =
      (premise.collectVars & conclusion.collectVars).size

    private def destructAndProve(conds: List[Expr], ors: List[Or], conclusion: Expr): Either[String, Unit] =
      (conds, ors) match
        case (Nil, Nil) =>
          if naive(conclusion) then
            logger.debug("PROVED by naive after destruct")
            return Right(())
          if narrow() then
            logger.debug("PROVED by type narrowing after destruct")
            return Right(())
          synthAndProve(conclusion)
            .orElse(tryDestructAndProve(conclusion))
        case (Nil, or :: rest) =>
          allRight(or.disjuncts.indices.toList,
            choice => locally { decide(or, choice); destructAndProve(Nil, rest, conclusion) })
        case (b :: rest, _) =>
          allRight(List(true, false),
            choice => locally { decide(b, choice); destructAndProve(rest, ors, conclusion) })

    /** Performs type narrowing. Returns if there is any inconsistency (str : ∅). */
    def narrow(): Boolean =
      for mc <- config.metrics do
        mc.timeStart("time/verif/narrow")
      val narrower = new Narrower
      val newTypes = narrower.narrow(getCtx)
      for mc <- config.metrics do
        mc.timePause("time/verif/narrow")
      newTypes.foreach(assume)
      newTypes.exists:
        case TypeTest(_, LangType(RegEx.RENone)) => true
        case _ => false

    private def synthAndProve(conclusion: Expr): Either[String, Unit] = conclusion match
      case TypeTest(e, t) =>
        for mc <- config.metrics do
          mc.timeStart("time/verif/type")
        val result = checkType(e, t)
        for mc <- config.metrics do
          mc.timePause("time/verif/type")
        result match
          case Right(_) =>
            logger.debug("PROVED by type checking")
            Right(())
          case Left(actual) =>
            Left("actual type: " + ppType(actual))
      case Const(false) => proveWithLemmas(conclusion, getCtx.premises)
      case _ =>
        proveWithLemmas(conclusion, List(conclusion))
          .orElse(proveWithLemmas(conclusion, getCtx.premises))

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
      case ListType(LangType(r2)) =>
        expr match
          case Split(str, Const(t: String)) if t.length == 1 =>
            val inferer = new Inferer(using ctx = getCtx)()
            val r1 = inferer.inferSplitLang(str, t.head)
            if r1.subsetOf(r2) then Right(()) else Left(ListType(LangType(r1)))
          case _ => throw UnsupportedOperationException(s"checkType $expr : $typ")
      case _ =>
        throw UnsupportedOperationException(s"checkType $expr : $typ")

    private val syn = new LemmaSynth

    private def proveWithLemmas(conclusion: Expr, seeds: List[Expr]): Either[String, Unit] =
      // Synthesize lemmas.
      for mc <- config.metrics do
        mc.timeStart("time/verif/type")
      val sketches = seeds.flatMap(collectSketches).distinct
      logger.trace("Sketches: {}", sketches.map(_.toString).mkString(", "))
      val lemmas = syn.synth(sketches)(using getCtx)
      for mc <- config.metrics do
        mc.timePause("time/verif/type")
      // Try to prove using these lemmas.
      if lemmas.nonEmpty then
        logger.trace("lemmas: {}", lemmas.map(ppExpr).mkString(", "))
        for mc <- config.metrics do
          mc.count("lemmas", lemmas.length)
        if lemmas.exists(_.conjuncts.contains(conclusion)) then
          logger.debug("PROVED by an exact lemma")
          return Right(())
        locally:
          lemmas.foreach(assume)
          if naive(conclusion) then
            logger.debug("PROVED by lemmas")
            Right(())
          else
            logger.debug("FAILED")
            Left("")
      else
        logger.debug("FAILED: no lemmas synthesized")
        Left("no lemma synthesized")

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
        case MapContains(_, ec@CharAt(_, _)) =>
          ss += syn.InferLang(ec)
        case Cmp(EQ | NE, es, Const(t: String)) =>
          ss += syn.InferLang(es, target = Some(t))
        case Cmp(EQ | NE, es1, es2) if es1.sort == StrSort && es2.sort == StrSort =>
          ss += syn.InferLang(es1)
          ss += syn.InferLang(es2)
        case Cmp(_, ei@Var(_), Find(es, Const(t: String))) if t.length == 1 =>
          ss += syn.InferIndexCmpFind(ei, es, t.head)
        case t: StrTest =>
          ss += syn.InferTest(t)
        case Length(es) =>
          ss += syn.InferLength(es)
        case ListLen(Split(es, Const(t: String))) if t.length == 1 =>
          ss += syn.InferSplitLength(es, t.head)
        case Find(es, Const(t: String)) =>
          ss += syn.InferFind(es, t)
        case StrToInt(es, Const(n: Int)) =>
          ss += syn.InferToNumber(es, n)
        case StrToSet(es) =>
          ss += syn.InferToSet(es)
      ss.toList
