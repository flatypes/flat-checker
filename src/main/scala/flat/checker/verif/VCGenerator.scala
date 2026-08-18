package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.Show.show
import flat.checker.flan.TypeOps.isSort
import flat.checker.flan.tpd.*
import flat.checker.verif.ExprOps.conjuncts
import flat.checker.verif.Simplifier.simplify
import org.eclipse.lsp4j.Range

import java.util.concurrent.atomic.AtomicInteger

trait VC:
  def subst(m: Map[String, Expr]): VC

  def &(that: VC): VC = (this, that) match
    case (VCTrue, v) => v
    case (v, VCTrue) => v
    case _ => VCAnd(this, that)

  def &&(that: VC): VC = (this, that) match
    case (VCTrue, v) => v
    case (v, VCTrue) => v
    case _ => VCAndSC(this, that)

case object VCTrue extends VC:
  override def subst(m: Map[String, Expr]): VC = this

final case class VCAnd(left: VC, right: VC) extends VC:
  override def subst(m: Map[String, Expr]): VC = VCAnd(left.subst(m), right.subst(m))

def mkVCAnd(vcs: List[VC]): VC = if vcs.isEmpty then VCTrue else vcs.reduce(_ & _)

/** Short-circuiting version of VCAnd: not process right if left is disproved. */
final case class VCAndSC(left: VC, right: VC) extends VC:
  override def subst(m: Map[String, Expr]): VC = VCAndSC(left.subst(m), right.subst(m))

final case class VCImplies(left: Expr, right: VC) extends VC:
  override def subst(m: Map[String, Expr]): VC = VCImplies(left.subst(m), right.subst(m))

extension (cond: Expr)
  def ->(right: VC): VC = VCImplies(cond, right)

final case class VCAssert(cond: Expr, error: VerifError) extends VC:
  override def subst(m: Map[String, Expr]): VC = copy(cond.subst(m))

def mkVCAssert(cond: Expr, error: Expr => VerifError): VC =
  val conjuncts = cond.conjuncts
  if conjuncts.isEmpty then VCTrue else conjuncts.map { e => VCAssert(e, error(e)) }.reduce(_ & _)

class VCGenerator(types: Map[String, Type], methodInfos: Map[String, MethodInfo]):
  def wlp(body: List[Stmt], post: VC, returnPost: VC, breakPost: VC, continuePost: VC): VC = body match
    case Nil => post
    case stmt :: rest =>
      wlp(stmt, wlp(rest, post, returnPost, breakPost, continuePost), returnPost, breakPost, continuePost)

  def wlp(stmt: Stmt, post: VC, returnPost: VC, breakPost: VC, continuePost: VC): VC =
    stmt match
      // Assignments
      case Assign(x, Some(e)) => safetyCheck(e) && typeCheck(e, types(x)) && post.subst(Map(x -> e))
      case Assign(x, None) => post.subst(Map(x -> Var(fresh(x))))
      case ExprStmt(e) => safetyCheck(e) & post
      // Proof derivatives
      case Assume(e) => safetyCheck(e) && e -> post
      case Assert(e) => (safetyCheck(e) && mkVCAssert(e, e => AssertNotProvedError(e.range))) & post
      case s@Abort() => VCAssert(BoolLit(false), AssertNotProvedError(s.range))
      // Conditional
      case If(e, thenBody, elseBody) =>
        val wlpThen = wlp(thenBody, post, returnPost, breakPost, continuePost)
        val wlpElse = wlp(elseBody, post, returnPost, breakPost, continuePost)
        safetyCheck(e) && (e -> wlpThen & Not(e) -> wlpElse)
      // Loops
      case While(e, inv, loopBody) =>
        val loopPost = mkVCAssert(inv, e => InvNotMaintainedError(e.range))
        val wlpLoop = wlp(loopBody, loopPost, returnPost, post, loopPost)
        val m = Map.from[String, Expr](for x <- collectModifiedVars(loopBody) yield x -> Var(fresh(x)))
        (safetyCheck(e) & safetyCheck(inv)) &&
          (mkVCAssert(inv, e => InvNotProvedOnEntryError(e.range)) &
            (inv -> (e -> wlpLoop & Not(e) -> post)).subst(m))
      case For(x, e, inv, loopBody) =>
        val loopPost = mkVCAssert(inv, e => InvNotMaintainedError(e.range))
        val wlpLoop = wlp(loopBody, loopPost, returnPost, post, loopPost)
        val m = Map.from[String, Expr](for x <- collectModifiedVars(loopBody) yield x -> Var(fresh(x)))
        (safetyCheck(e) & safetyCheck(inv)) &&
          (mkVCAssert(inv, e => InvNotProvedOnEntryError(e.range)) &
            (inv -> (ListContains(e, Var(x)) -> wlpLoop & post)).subst(m))
      // Jumps
      case Return() => returnPost
      case Break() => breakPost
      case Continue() => continuePost

  private def safetyCheck(expr: Expr): VC = expr match
    // Boolean
    case And(e1, e2) => safetyCheck(e1) && e1 -> safetyCheck(e2)
    case Or(e1, e2) => safetyCheck(e1) && Not(e1) -> safetyCheck(e2)
    case Ite(e, e1, e2) => safetyCheck(e1) && (e -> safetyCheck(e1) & Not(e) -> safetyCheck(e2))
    // List
    case ListAt(e, ei) =>
      VCAssert(And(Le(IntLit(0), ei), Lt(ei, SeqLength(e))), IndexOutOfBoundsError(ei.range))
    case SeqSlice(_, ei, ej) =>
      VCAssert(Le(IntLit(0), ei), IndexNegError(ei.range)) & VCAssert(Le(IntLit(0), ej), IndexNegError(ej.range))

    // Method call
    case Apply(MethodRef(f), es) =>
      val m = methodInfos(f)
      val vcType = mkVCAnd(for (e, VarDecl(_, t)) <- es zip m.params yield typeCheck(e, t))
      val pre = mkAnd(m.requires).subst((m.paramNames zip es).toMap)
      val vcPre = mkVCAssert(pre, e => PreNotProvedError(e.range))
      vcType & vcPre

    // others
    case _ =>
      expr.subtrees match
        case Nil => VCTrue
        case es => es.map(safetyCheck).reduce(_ & _)

  private def typeCheck(expr: Expr, typ: Type): VC = (expr, typ) match
    case (Var(x), _) if types(x) == typ => VCTrue
    case (_, t) if t.isSort => VCTrue
    case (_, RefinedType(_, reft)) => VCAssert(reft.subst(Map("_" -> expr)), ReftNotProvedError(expr.range))
    case _ => throw UnsupportedOperationException(s"Cannot type check ${expr.show}: ${typ.show}")

  private def collectModifiedVars(body: List[Stmt]): List[String] =
    val vars = body.flatMap:
      case Assign(x, _) => List(x)
      case Havoc(x) => List(x)
      case If(_, b1, b2) => collectModifiedVars(b1) ++ collectModifiedVars(b2)
      case While(_, _, b) => collectModifiedVars(b)
      case _ => Nil
    vars.distinct

  private val counters = (for x <- types.keys yield x -> new AtomicInteger(0)).toMap

  private def fresh(x: String): String =
    val counter = counters(x)
    val n = counter.getAndIncrement()
    s"$x:$n"

class VCDischarger(methodInfos: Map[String, MethodInfo])(using reporter: Reporter) extends LazyLogging:
  private val prover = Prover()

  def discharge(vc: VC, types: Map[String, Type], premises: List[Expr]): Boolean = vc match
    case VCTrue => true
    case VCAnd(vc1, vc2) => discharge(vc1, types, premises) & discharge(vc2, types, premises)
    case VCAndSC(vc1, vc2) => discharge(vc1, types, premises) && discharge(vc2, types, premises)
    case VCImplies(e, vc) =>
      val cond = e.simplify
      val newPremises = cond.conjuncts ++ extractPost(cond)
      val newTypes = newPremises.collect { case Ne(Var(x), NullLit()) => x -> narrowNotNull(types(x)) }
      discharge(vc, types ++ newTypes, premises ++ newPremises)
    case VCAssert(e, error) =>
      val cond = e.simplify
      assert(cond, types, premises ++ extractPost(cond), error)

  private def extractPost(expr: Expr): List[Expr] = expr match
    case Apply(MethodRef(f), es) =>
      val m = methodInfos(f)
      mkAnd(m.ensures) match
        case BoolLit(true) => Nil
        case e =>
          val m1 = (for (x, e) <- m.paramNames zip es yield x -> e).toMap
          val m2 = m.returnNames match
            case List(y) => Map(y -> expr)
            case ys => Map.from[String, Expr](for i <- ys.indices yield ys(i) -> TupleSelect(i, expr))
          e.subst(m1 ++ m2).simplify.conjuncts
    case _ =>
      expr.subtrees.flatMap(extractPost)

  private def narrowNotNull(typ: Type): Type = typ match
    case NullableType(t) => t
    case _ => typ

  private def splitAndAssert(cond: Expr, types: Map[String, Type], premises: List[Expr],
                             error: Expr => VerifError): Boolean =
    val results = for e <- cond.conjuncts yield assert(e, types, premises, error(e))
    results.forall(_ == true)

  private val goalCounter = new AtomicInteger(0)

  private def assert(cond: Expr, types: Map[String, Type], premises: List[Expr],
                     error: => VerifError): Boolean =
    val conclusion = cond.simplify
    logger.info("Goal {}:\n{}", goalCounter.incrementAndGet(), showGoal(premises, conclusion))
    true
//    if prover.prove(Goal(premises, conclusion.simplify)) then
//      logger.info("PROVED")
//      true
//    else
//      reporter.report(AssertNotProvedError(conclusion.range))
//      logger.warn("❌ NOT PROVED")
//      false

  private def showGoal(premises: List[Expr], conclusion: Expr): String =
    val premiseStr = (for e <- premises yield s"  ${e.show}\n").mkString
    s"$premiseStr ⇒ ${conclusion.show}"
