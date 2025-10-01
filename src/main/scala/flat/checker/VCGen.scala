package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Location
import flat.checker.core.*
import flat.checker.core.CmpOp.{GE, LT}

import scala.annotation.tailrec
import scala.collection.mutable

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

  def generate(program: Program): VC =
    given Types = Types.from(program.vars)

    given Fresher = new Fresher

    wlp(program.body, True, True)(using pReturn = True)

  /** Compute the weakest liberal pre of a statement `stmt` and a post condition `post`. */
  private def wlp(stmt: Stmt, post: VC, body: List[Stmt], pInv: VC)
                 (using types: Types, pReturn: VC, fresher: Fresher): VC =
    stmt match
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
        val pTrue = conds.foldRight(wlp(s1, post, pInv)) { case (e, p) =>
          val sides = collectSideGoals(e)
          mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(e :: sides.map(_._1), p))
        }
        val pFalse = mkLImp(Not(b).copyLocation(b) :: collectSideGoals(b).map(_._1), wlp(s2, post, pInv))
        mkLAnd(pTrue, pFalse)
      case whileStmt@While(b, s, userInv) =>
        val inv =
          if userInv.isEmpty then Analyzer.guessLoopInv(whileStmt, body).map(_.copyLocation(b)) else userInv
        if userInv.isEmpty then
          logger.debug(s"Guessing invariants: ${inv.mkString(", ")}")
        val pInv = mkLAnd(for e <- inv yield Goal(e, InvariantMayViolate(e.loc)))
        val sides = collectSideGoals(b)
        val pEnter = (b :: sides.map(_._1) ++ inv).foldRight(wlp(s, pInv, pInv))(LImp.apply)
        val exitCond = mkOr(
          mkAnd(Not(b).copyLocation(b) :: sides.map(_._1)) ::
            collectBreakCond(s).map(e => mkAnd(e :: collectSideGoals(e).map(_._1))))
        val pExit = (exitCond :: inv).foldRight(post)(LImp.apply)
        val pLoop = mkLAnd(pEnter, pExit)
        val m = Map.from(for x <- Analyzer.getModifiedVars(whileStmt) yield x -> Var(fresher.fresh(x)))
        mkLAnd(pInv, mkLAnd(sides.map(Goal(_, _))), pLoop.subst(m))
      case Break() => pInv
      case Return() => pReturn
      case ShowType(e) =>
        val sides = collectSideGoals(e)
        mkLAnd(mkLAnd(sides.map(Goal(_, _))), mkLImp(sides.map(_._1), InferType(e, e.loc)), post)

  @tailrec
  private def wlp(body: List[Stmt], post: VC, pInv: VC)
                 (using types: Types, pReturn: VC, fresher: Fresher): VC =
    if body.isEmpty then post else wlp(body.dropRight(1), wlp(body.last, post, body, pInv), pInv)

  private def destruct(cond: Expr): List[Expr] = cond match
    case And(e1, e2) => destruct(e1) ++ destruct(e2)
    case Not(Or(e1, e2)) => destruct(Not(e1)) ++ destruct(Not(e2))
    case Not(And(e1, e2)) => List(Or(Not(e1), Not(e2)))
    case _ => List(cond)

  private def collectBreakCond(body: List[Stmt]): List[Expr] =
    body.collect {
      case IfStmt(cond, List(Break()), _) => cond
    }

  private def collectSideGoals(expr: Expr): List[(Expr, TypeError)] = expr.walkAndCollect {
    case CharAt(str, index) => // 0 <= index < |str|
      (And(GE(index, 0), LT(index, Length(str))), IndexMayOutOfBounds(index.loc))
    case Substr(_, fromIndex, untilIndex) => // both indices are non-negative
      (And(GE(fromIndex, 0), GE(untilIndex, 0)), IndexMayOutOfBounds(expr.loc))
    //    case StrToCode(str) => // |str| == 1
    //      Goal(EQ(StrLen(str), 1), TypeMayMismatch("char (string of length 1)", "string", str.loc))
    //    case StrFromCode(int) => // 0 <= int <= 0x2FFFF
    //      Goal(And(GE(int, 0), LE(int, 0x2FFFF)), IndexMayOutOfBounds(int.loc))
    //    case StrToInt(str) => // str in number
    //      HasType(str, LangType(RegExpr.number), str.loc)
  }