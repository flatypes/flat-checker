package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Location
import flat.checker.ast.*
import flat.checker.ast.CmpOp.{GE, LT}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum VC:
  case True
  case HasType(value: Expr, typ: Type, loc: Location)
  case InferType(value: Expr, loc: Location)
  case Goal(boolExpr: Expr, err: TypeError)
  case LAnd(left: VC, right: VC)
  case LImp(premise: Expr, conclusion: VC)

  def subst(mappings: Map[String, Expr]): VC = this match
    case True => True
    case HasType(e, t, loc) => HasType(e.subst(mappings), t, loc)
    case InferType(e, loc) => InferType(e.subst(mappings), loc)
    case Goal(e, err) => Goal(e.subst(mappings), err)
    case LAnd(phi1, phi2) => LAnd(phi1.subst(mappings), phi2.subst(mappings))
    case LImp(e, phi) => LImp(e.subst(mappings), phi.subst(mappings))

  override def toString: String = this match
    case True => "⊤"
    case HasType(e, t, _) => s"($e : $t)"
    case InferType(e, _) => s"($e : ?)"
    case Goal(e, _) => s"$e"
    case LAnd(phi1, phi2) => s"($phi1 ∧ $phi2)"
    case LImp(e, phi) => s"$e ⇒ $phi"

object VC:
  def mkLAnd(formulas: List[VC]): VC =
    val nontrivial = formulas.filter {
      case True | LImp(_, True) => false
      case _ => true
    }
    if nontrivial.isEmpty then True else nontrivial.reduceRight(LAnd.apply)

  def mkLAnd(formulas: VC*): VC = mkLAnd(formulas.toList)

  def mkLImp(premises: List[Expr], conclusion: VC): VC = premises match
    case Nil => conclusion
    case _ => LImp(premises.reduce(And(_, _)), conclusion)

final class Types(store: Map[String, Type]):
  def apply(name: String): Type =
    val i = name.indexOf('@')
    val x = if i >= 0 then name.substring(0, i) else name
    store(x)

  def strVars: Set[String] = store.filter(_._2.toSort == Sort.S).keySet

  def intVars: Set[String] = store.filter(_._2.toSort == Sort.I).keySet

object Types:
  def from(it: IterableOnce[(String, Type)]) = Types(Map.from(it))

class Fresher:
  private val latest = mutable.Map.empty[String, Int]

  def fresh(name: String): String =
    require(!name.contains('@'))
    val k = latest.getOrElse(name, 0)
    latest(name) = k + 1
    s"$name@${k + 1}"

object VCGen extends LazyLogging:

  import VC.*

  def generate(types: Types, body: Stmt): VC =
    Analyzer.guessInvariants(body, Map.from(for x <- types.intVars yield x -> Analyzer.Value.Rel(0)))
    wlp(body, True, body.toBlock, True)(using types, True, new Fresher)

  /** Compute the weakest liberal pre of a statement `stmt` and a post condition `post`. */
  private def wlp(stmt: Stmt, post: VC, body: List[Stmt], pInv: VC)
                 (using types: Types, pReturn: VC, fresher: Fresher): VC =
    stmt match
      case Skip() => post
      case SeqStmt(s1, s2) => wlp(s1, wlp(s2, post, body, pInv), body, pInv)
      case Assign(x, e) =>
        val sides = collectSideGoals(e)
        val t = types(x)
        val b: Type = t.toSort
        val pAssign = if t == b then True else HasType(e, t, e.loc)
        mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(sides.map(_._1), pAssign), post.subst(Map(x -> e)))
      case Assert(cond) =>
        val sides = collectSideGoals(cond)
        mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(sides.map(_._1), Goal(cond, AssertionMayFail(cond.loc))), post)
      case IfStmt(b, s1, s2) =>
        val conds = destruct(b)
        val body1 = s1.toBlock
        val pTrue = conds.foldRight(wlp(s1, post, body1, pInv)) { case (e, p) =>
          val sides = collectSideGoals(e)
          mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(e :: sides.map(_._1), p))
        }
        val body2 = s2.toBlock
        val pFalse = LImp(Not(b).copyLocation(b), wlp(s2, post, body2, pInv))
        mkLAnd(pTrue, pFalse)
      case whileStmt@While(b, s) =>
        val inv = whileStmt.invariants.toList
        val pInv = mkLAnd(for e <- inv yield Goal(e, InvariantMayViolate(e.loc)))
        val sides = collectSideGoals(b)
        val body1 = s.toBlock
        val pEnter = (b :: sides.map(_._1) ++ inv).foldRight(wlp(s, pInv, body1, pInv))(LImp.apply)
        val exitCond = mkOr(
          mkAnd(Not(b).copyLocation(b) :: sides.map(_._1)) ::
            Analyzer.collectBreakConds(s).map(e => mkAnd(e :: collectSideGoals(e).map(_._1))))
        val pExit = (exitCond :: inv).foldRight(post)(LImp.apply)
        val pLoop = mkLAnd(pEnter, pExit)
        val m = Map.from(for x <- Analyzer.collectModifiedVars(whileStmt) yield x -> Var(fresher.fresh(x)))
        mkLAnd(pInv, mkLAnd(sides.map(Goal(_, _))), pLoop.subst(m))
      case Break() => pInv
      case Return() => pReturn
      case ShowType(e) =>
        val sides = collectSideGoals(e)
        mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(sides.map(_._1), InferType(e, e.loc)), post)

  //  @tailrec
  //  private def wlp(body: List[Stmt], post: VC, pInv: VC)
  //                 (using types: Types, pReturn: VC, fresher: Fresher): VC =
  //    if body.isEmpty then post else wlp(body.dropRight(1), wlp(body.last, post, body, pInv), pInv)

  private def destruct(cond: Expr): List[Expr] = cond match
    case And(e1, e2) => destruct(e1) ++ destruct(e2)
    case Not(Or(e1, e2)) => destruct(Not(e1)) ++ destruct(Not(e2))
    case Not(And(e1, e2)) => List(Or(Not(e1), Not(e2)))
    case _ => List(cond)

  private def collectSideGoals(expr: Expr): List[(Expr, TypeError)] =
    val buf = ListBuffer.empty[(Expr, TypeError)]
    expr.traverse:
      case CharAt(e, ei) => // 0 <= ei < |e|
        buf += (And(GE(ei, 0), LT(ei, Length(e))) -> IndexMayOutOfBounds(ei.loc))
      case Substr(_, ei, ej) => // both indices are non-negative
        buf += (And(GE(ei, 0), GE(ej, 0)) -> IndexMayOutOfBounds(expr.loc))
    buf.toList