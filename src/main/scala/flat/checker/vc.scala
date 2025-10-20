package flat.checker

import flat.Diagnostic
import flat.Ops.CmpOp.*
import flat.checker.Analyzer.{collectBreakConds, collectModifiedVars, guessInvariants}
import flat.checker.ExprOps.conjuncts
import flat.checker.Printer.ppType
import flat.checker.ast.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait VC:
  def subst(m: Map[String, Expr]): VC

case object VCTrue extends VC:
  def subst(m: Map[String, Expr]): VC = this

final case class VCInfer(value: Expr) extends VC:
  def subst(m: Map[String, Expr]): VC = VCInfer(value.subst(m))

final case class VCImp(left: Expr, right: VC) extends VC:
  def subst(m: Map[String, Expr]): VC = VCImp(left.subst(m), right.subst(m))

final case class VCAnd(left: VC, right: VC) extends VC:
  def subst(m: Map[String, Expr]): VC = VCAnd(left.subst(m), right.subst(m))

def mkVCAnd(conjuncts: List[VC]): VC =
  conjuncts.filter(_ != VCTrue) match
    case Nil => VCTrue
    case List(vc) => vc
    case _ => conjuncts.reduceRight(VCAnd(_, _))

def mkVCAnd(conjuncts: VC*): VC = mkVCAnd(conjuncts.toList)

sealed trait VCGoal extends VC:
  def cond: Expr

  def diagnostic(msg: String): Diagnostic

final case class VCType(value: Expr, expected: Type) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(value = value.subst(m))

  def cond: Expr = TypeTest(value, expected)

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Type may mismatch", s"expected: ${ppType(expected)}\n" + msg)

final case class VCAssert(cond: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCAssert(cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Assertion may fail", msg)

final case class VCInvPre(cond: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCInvPre(cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Invariant may not hold at the entry point", msg)

final case class VCInvPost(cond: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCInvPost(cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Invariant may not hold again after an iteration", msg)

final case class VCIdxInBound(idx: Expr, str: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCIdxInBound(idx.subst(m), str.subst(m))

  def cond: Expr = And(LE(0, idx), LT(idx, Length(str)))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Index may be out of bound", msg)

final case class VCIdxNonneg(idx: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCIdxNonneg(idx.subst(m))

  def cond: Expr = GE(idx, 0)

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Index may be negative", msg)

final case class VCPre(cond: Expr) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = VCPre(cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(cond.loc, "Pre-condition may fail", msg)

object VCGenerator:
  def generate(body: Stmt, types: Types): VC =
    guessInvariants(body)
    val fresher = new Fresher
    wlp(body, VCTrue)(using types, fresher, VCTrue)

  private def wlp(stmt: Stmt, post: VC)(using types: Types, fresher: Fresher, vcInv: VC): VC = stmt match
    case Skip() => post
    case SeqStmt(s1, s2) => wlp(s1, wlp(s2, post))
    case Assume(b) =>
      mkVCAnd(checkSide(b), VCImp(b, post))
    case Assign(x, e) =>
      val t = types(x)
      val vcType = if t == ast.fromSort(t.toSort) then VCTrue else VCType(e, t)
      mkVCAnd(checkSide(e), vcType, post.subst(Map(x -> e)))
    case Assert(b) =>
      mkVCAnd(checkSide(b), VCAssert(b), post)
    case ShowType(e) =>
      mkVCAnd(checkSide(e), VCInfer(e), post)
    case IfStmt(b, s1, s2) =>
      val bs = b.conjuncts
      val vcSide = mkVCAnd(bs.indices.map(i => VCImp(mkAnd(bs.take(i)), checkSide(bs(i)))).toList)
      val vcThen = VCImp(b, wlp(s1, post))
      val vcElse = VCImp(Not(b), wlp(s2, post))
      mkVCAnd(vcSide, vcThen, vcElse)
    case loop@While(b, s) =>
      val bis = loop.invariants.toList
      val m = Map.from(for x <- collectModifiedVars(s) yield x -> Var(fresher.fresh(x)))
      val vcInvSide = mkVCAnd(bis.indices.map(i => VCImp(mkAnd(bis.take(i)), checkSide(bis(i)))).toList).subst(m)
      val vcInvPre = mkVCAnd(bis.map(VCInvPre(_)))
      val vcSide = checkSide(b).subst(m)
      val vcInv = mkVCAnd(bis.map(VCInvPost(_)))
      val vcEnter = VCImp(mkAnd(b :: bis), wlp(s, vcInv)(using vcInv = vcInv)).subst(m)
      val vcExit = VCImp(mkAnd(mkOr(Not(b) :: collectBreakConds(s)) :: bis), post).subst(m)
      mkVCAnd(vcInvSide, vcInvPre, vcSide, vcEnter, vcExit)
    case Return() => VCTrue
    case Break() => vcInv // NOTE: the loop invariant must hold immediately before the break statement

  private def checkSide(expr: Expr): VC =
    val goals = ListBuffer.empty[VCGoal]
    expr.traverse:
      case CharAt(es, ei) =>
        goals += VCIdxInBound(ei, es)
      case Substr(es, ei, ej) =>
        if ei != Const(0) then
          goals += VCIdxNonneg(ei)
        if ej != Length(es) then
          goals += VCIdxNonneg(ej)
    mkVCAnd(goals.toList)

class Fresher:
  private val latest = mutable.Map.empty[String, Int]

  def fresh(name: String): String =
    require(!name.contains('@'))
    val k = latest.getOrElse(name, 0)
    latest(name) = k + 1
    s"$name@${k + 1}"

trait VCRunner:
  def run(vc: VC): Unit = vc match
    case VCTrue => // ignore trivial
    case VCInfer(e) => infer(e)
    case VCImp(b, vc) => assume(b); run(vc)
    case VCAnd(vc1, vc2) => push(); run(vc1); pop(); push(); run(vc2); pop()
    case g: VCGoal => prove(g)

  protected def push(): Unit

  protected def pop(): Unit

  protected def assume(cond: Expr): Unit

  protected def infer(value: Expr): Unit

  protected def prove(goal: VCGoal): Unit