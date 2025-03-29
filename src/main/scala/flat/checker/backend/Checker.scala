package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.Bound.*
import flat.checker.backend.SolverResult.{Invalid, Valid}
import flat.checker.backend.core.*
import flat.checker.backend.core.CmpOp.{EQ, LE, NE}

import scala.annotation.tailrec
import scala.collection.mutable

final class Types(store: Map[String, Type]):
  def apply(name: String): Type =
    val i = name.indexOf('@')
    val x = if i >= 0 then name.substring(0, i) else name
    store(x)

object Types:
  def from(it: IterableOnce[(String, Type)]) = Types(Map.from(it))

class Checker extends LazyLogging:
  given issuer: Issuer = new Issuer

  private enum Formula:
    case True
    case HasType(value: Expr, typ: Type, loc: Location)
    case Goal(boolExpr: Expr, err: TypeError)
    case LAnd(left: Formula, right: Formula)
    case LImp(premise: Expr, conclusion: Formula)

    def subst(mappings: Map[String, Expr]): Formula = this match
      case True => True
      case HasType(e, t, loc) => HasType(e.subst(mappings), t, loc)
      case Goal(e, err) => Goal(e.subst(mappings), err)
      case LAnd(phi1, phi2) => LAnd(phi1.subst(mappings), phi2.subst(mappings))
      case LImp(e, phi) => LImp(e.subst(mappings), phi.subst(mappings))

    override def toString: String = this match
      case True => "⊤"
      case HasType(e, t, _) => s"($e : $t)"
      case Goal(e, _) => s"$e"
      case LAnd(phi1, phi2) => s"($phi1 ∧ $phi2)"
      case LImp(e, phi) => s"$e ⇒ $phi"

  import Formula.*

  private def mkLAnd(formulas: List[Formula]): Formula =
    val nontrivial = formulas.filter {
      case True | LImp(_, True) => false
      case _ => true
    }
    if nontrivial.isEmpty then True else nontrivial.reduceRight(LAnd.apply)

  private def mkLAnd(formulas: Formula*): Formula = mkLAnd(formulas.toList)

  def check(program: Program): Unit =
    given Types = Types.from(program.vars)

    given Fresher = new Fresher
    val pre = wlp(program.body, True, True)(using pReturn = True)
    logger.debug(s"Overall Goal: $pre")
    discharge(pre, Nil)
    issuer.ensureNoError()

  class Fresher:
    private val latest = mutable.Map.empty[String, Int]

    def fresh(name: String): String =
      require(!name.contains('@'))
      val k = latest.getOrElse(name, 0)
      latest(name) = k + 1
      s"$name@${k + 1}"

  /** Compute the weakest liberal pre of a statement `stmt` and a post condition `post`. */
  private def wlp(stmt: Stmt, post: Formula, body: List[Stmt], pNext: Formula)
                 (using types: Types, pReturn: Formula, fresher: Fresher): Formula =
    stmt match
      case Assign(x, e) =>
        val pSide = mkLAnd(collectSideGoals(e))
        val t = types(x)
        val b: Type = t.toSort
        val pAssign = if t == b then True else HasType(e, t, e.loc)
        mkLAnd(pSide, pAssign, post.subst(Map(x -> e)))
      case Assert(cond) =>
        val pSide = mkLAnd(collectSideGoals(cond))
        mkLAnd(pSide, Goal(cond, AssertionMayFail(cond.loc)), post)
      case IfStmt(b, s1, s2) =>
        val pSide = mkLAnd(collectSideGoals(b))
        mkLAnd(pSide, LImp(b, wlp(s1, post, pNext)), LImp(Not(b).copyLocation(b), wlp(s2, post, pNext)))
      case whileStmt@While(b, s, userInv) =>
        val inv =
          if userInv.isEmpty then Analyzer.guessLoopInv(whileStmt, body).map(_.copyLocation(b)) else userInv
        if userInv.isEmpty then
          logger.debug(s"Guessing invariants: ${inv.mkString(", ")}")
        val pInv = mkLAnd(inv.flatMap(collectSideGoals) ++ (for e <- inv yield Goal(e, InvariantMayViolate(e.loc))))
        val pSide = mkLAnd(collectSideGoals(b))
        val pEnter = (b :: inv).foldRight(wlp(s, pInv, post))(LImp.apply)
        val pExit = (Not(b).copyLocation(b) :: inv).foldRight(post)(LImp.apply)
        val pLoop = mkLAnd(pEnter, pExit)
        val m = Map.from(for x <- Analyzer.getModifiedVars(whileStmt) yield x -> Var(fresher.fresh(x)))
        mkLAnd(pInv, pSide, pLoop.subst(m))
      case Break() => pNext
      case Return(_) => pReturn

  @tailrec
  private def wlp(body: List[Stmt], post: Formula, pNext: Formula)
                 (using types: Types, pReturn: Formula, fresher: Fresher): Formula =
    if body.isEmpty then post
    else wlp(body.dropRight(1), wlp(body.last, post, body, pNext), pNext)

  private def collectSideGoals(expr: Expr): List[Formula] = expr.walkAndCollect {
    case StrAt(str, index) => // 0 <= index <= |str|
      Goal(And(LE(0, index), LE(index, StrLen(str))), IndexMayOutOfBounds(index.loc))
    case CharToCode(str) => // |str| == 1
      Goal(EQ(StrLen(str), 1), TypeMayMismatch("char (string of length 1)", "string", str.loc))
    case CharFromCode(int) => // 0 <= int <= 0x2FFFF
      Goal(And(LE(0, int), LE(int, 0x2FFFF)), IndexMayOutOfBounds(int.loc))
    case StrToInt(str) => // str in number
      HasType(str, LangType(ReLang.number), str.loc)
  }

  private def collectVars(formula: Formula): Set[String] =
    formula match
      case True => Set.empty
      case HasType(e, _, _) => e.collectVars
      case Goal(e, _) => e.collectVars
      case LAnd(phi1, phi2) => collectVars(phi1) | collectVars(phi2)
      case LImp(e, phi) => e.collectVars | collectVars(phi)

  /** Discharge a proof goal encoded as a formula `phi`, under `premises`. */
  private def discharge(goal: Formula, premises: List[Expr])(using types: Types): Unit =
    goal match
      case True => // trivially hold
      case HasType(e, t, loc) =>
        t match
          case LangType(r2) =>
            val inferer = Inferer(types, premises)
            e.accept(inferer, ()) match
              case LangType(r1) if ReLangSub.check(r1, r2) =>
              case actual => issuer.report(TypeMayMismatch(r2.toString, actual.toString, loc))
          case _ => // NOTE: should be well-sorted
      case Goal(cond, err) =>
        logger.debug("")
        logger.debug("Goal: " + premises.mkString(" ∧ ") + " ⇒ " + goal.toString)
        prove(cond, premises) match
          case Valid => logger.debug("Goal proved")
          case Invalid(cm) =>
            issuer.report(err)
            logger.error("Goal failed")
      case LAnd(goal1, goal2) =>
        discharge(goal1, premises)
        discharge(goal2, premises)
      case LImp(cond, goal) =>
        discharge(goal, destruct(cond) ++ premises)

  private def destruct(premise: Expr): List[Expr] = premise match
    case And(e1, e2) => destruct(e1) ++ destruct(e2)
    case _ => List(premise)

  @tailrec
  private def prove(goal: Expr, premises: List[Expr])(using types: Types): SolverResult =
    // First try: consider only key expressions in goal
    tryProve(goal, premises, List(goal)) match
      case Valid => Valid
      case _ =>
        // Second try: consider key expressions in all premises and goal
        tryProve(goal, premises, goal :: premises) match
          case Valid => Valid
          case result =>
            goal match
              case Or(p, q) => // Third try: P or Q <=> not p => Q
                prove(q, Not(p) :: premises)
              case _ => result

  private def tryProve(goal: Expr, premises: List[Expr], keyExprs: List[Expr])
                      (using types: Types): SolverResult =
    val es = keyExprs.flatMap {
      _.collect {
        case e if e.productPrefix.startsWith("Str") || e.productPrefix.startsWith("Char") => List(e)
        case e@Cmp(EQ | NE, Var(x), Const(_: String)) if types(x).toSort == Sort.String => List(e, Var(x))
      }.flatten
    }
    val inferer = Inferer(types, premises)
    val hints =
      for
        e <- es
        t = e.accept(inferer, ())
        c = encodeHasType(e, t)
        if c != Const(true)
      yield c

    if hints.nonEmpty then
      logger.debug("  hints: " + hints.mkString(", "))
    SMTSolver.prove(goal, premises ++ hints)

  @tailrec
  private def encodeHasType(value: Expr, typ: Type): Expr = typ match
    case HintType(Pred(_, p)) => p(value)
    case HintType(hint) =>
      encodeHasType(value, hint.toType)
    case IntervalType(interval) =>
      interval match
        case Interval(NegInf, PosInf) => true
        case Interval(NegInf, Fin(k)) => LE(value, k)
        case Interval(Fin(k1), Fin(k2)) => And(LE(k1, value), LE(value, k2))
        case Interval(Fin(k), PosInf) => LE(k, value)
        case _ => assert(false)
    case TernaryType(b) =>
      b match
        case Ternary.Bot => assert(false)
        case Ternary.True => value
        case Ternary.False => Not(value)
        case Ternary.Maybe => true
    case LangType(r) if r.isSmall =>
      val ss = r.getLang
      mkOr(for s <- ss yield EQ(value, s))
    case LangType(ReLang.ReChars(cs)) =>
      if cs.polarity then mkOr(for c <- cs.chars yield EQ(value, c.toString))
      else mkAnd(for c <- cs.chars yield NE(value, c.toString))
    case _ => true