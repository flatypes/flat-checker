package flat.checker

import flat.Ops.CmpOp.*
import flat.checker.Analyzer.{collectBreakConds, collectModifiedVars, guessInvariants}
import flat.checker.ExprOps.conjuncts
import flat.checker.ast.*
import flat.checker.ast.Printer.ppDomain
import flat.regex.Domain
import flat.{Diagnostic, Location}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait VC:
  def subst(m: Map[String, Expr]): VC

case object VCTrue extends VC:
  def subst(m: Map[String, Expr]): VC = this

final case class VCImp(left: Expr, right: VC) extends VC:
  def subst(m: Map[String, Expr]): VC = copy(left = left.subst(m), right = right.subst(m))

final case class VCGroup(sides: List[VC] = Nil, mains: List[VC] = Nil) extends VC:
  def subst(m: Map[String, Expr]): VC = copy(sides = sides.map(_.subst(m)), mains = mains.map(_.subst(m)))

def mkVCGroup(sides: List[VC], mains: VC*): VCGroup = VCGroup(sides, mains.toList)

final case class VCInfer(value: Expr)(using val loc: Location) extends VC:
  def subst(m: Map[String, Expr]): VC = copy(value = value.subst(m))

sealed trait VCGoal extends VC:
  def cond: Expr

  def diagnostic(msg: String): Diagnostic

final case class VCType(value: Expr, expected: Domain)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(value = value.subst(m))

  def cond: Expr = RefinedBy(value, expected)

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Type may mismatch", s"expected: ${ppDomain(expected)}\n" + msg)

final case class VCAssert(cond: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(cond = cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Assertion may fail", msg)

final case class VCHint(cond: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(cond = cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Hint may be wrong", msg)

final case class VCInvPre(cond: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(cond = cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Invariant may not hold at the entry point", msg)

final case class VCInvPost(cond: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(cond = cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Invariant may not hold again after an iteration", msg)

final case class VCIdxInBound(idx: Expr, str: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(idx = idx.subst(m), str = str.subst(m))

  def cond: Expr = And(LE(Const(0), idx), LT(idx, StringLength(str)))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Index may be out of bound", msg)

final case class VCIdxNonneg(idx: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(idx = idx.subst(m))

  def cond: Expr = GE(idx, Const(0))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Index may be negative", msg)

final case class VCPre(cond: Expr)(using loc: Location) extends VCGoal:
  def subst(m: Map[String, Expr]): VC = copy(cond = cond.subst(m))

  def diagnostic(msg: String): Diagnostic =
    Diagnostic(loc, "Pre-condition may fail", msg)

final class Fresh:
  private val latest = mutable.Map.empty[String, Int]

  def apply(name: String): String =
    require(!name.contains('@'))
    val k = latest.getOrElse(name, 0)
    latest(name) = k + 1
    s"$name@${k + 1}"

object VCGenerator:
  def generate(funDef: FunDef): VC =
    guessInvariants(funDef.body)
    val post = mkVCGroup(Nil, funDef.ensures.map(e => VCAssert(e)(using e.loc)) *)
    val vc = wlp(funDef.body, post)(using funDef.lCtx, new Fresh, VCTrue)
    VCImp(mkAnd(funDef.requires), vc)

  private def wlp(stmt: Stmt, post: VC)(using ctx: Map[String, Sort], fresh: Fresh, vcInv: VC): VC = stmt match
    case Skip() => post
    case SeqStmt(s1, s2) => wlp(s1, wlp(s2, post))
    case Assume(b) =>
      mkVCGroup(checkSides(b), VCImp(b, post))
    case Assign(x, e) =>
      val t = ctx(x)
      mkVCGroup(checkSides(e), post.subst(Map(x -> e)))
    case Assert(b) =>
      mkVCGroup(checkSides(b), VCAssert(b)(using b.loc), post)
    case Hint(b) =>
      mkVCGroup(checkSides(b), VCHint(b)(using b.loc), VCImp(b, post))
    case ShowType(e) =>
      mkVCGroup(checkSides(e), VCInfer(e)(using e.loc), post)
    case IfStmt(b, s1, s2) =>
      val bs = b.conjuncts
      val vcSides = bs.indices.map(i => VCImp(mkAnd(bs.take(i)), VCGroup(sides = checkSides(bs(i))))).toList
      val vcThen = VCImp(b, wlp(s1, post))
      val vcElse = VCImp(Not(b), wlp(s2, post))
      mkVCGroup(vcSides, vcThen, vcElse)
    case loop@While(b, s) =>
      val bis = loop.invariants.toList
      val m = Map.from(for x <- collectModifiedVars(s) yield x -> Var(fresh(x))(ctx(x).base))
      val vcInvSides =
        bis.indices.map(i => VCImp(mkAnd(bis.take(i)), VCGroup(sides = checkSides(bis(i)))).subst(m)).toList
      val vcSides = checkSides(b).map(_.subst(m))
      val vcInvPres = bis.map(bi => VCInvPre(bi)(using bi.loc))
      val vcInv = VCGroup(mains = bis.map(bi => VCInvPost(bi)(using bi.loc)))
      val vcEnter = VCImp(mkAnd(b :: bis), wlp(s, vcInv)(using vcInv = vcInv)).subst(m)
      val vcExit = VCImp(mkAnd(mkOr(Not(b) :: collectBreakConds(s)) :: bis), post).subst(m)
      VCGroup(vcInvSides ++ vcSides, vcInvPres :+ vcEnter :+ vcExit)
    case Return() => VCTrue
    case Break() => vcInv // NOTE: the loop invariant must hold immediately before the break statement

  private def checkSides(expr: Expr): List[VC] =
    val goals = ListBuffer.empty[VCGoal]
    expr.traverse:
      case CharAt(es, ei) =>
        goals += VCIdxInBound(ei, es)(using ei.loc)
      case Substring(es, ei, ej) =>
        if ei != Const(0) then
          goals += VCIdxNonneg(ei)(using ei.loc)
        if ej != StringLength(es) then
          goals += VCIdxNonneg(ej)(using ej.loc)
    goals.toList
