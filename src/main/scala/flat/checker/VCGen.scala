package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Location
import flat.checker.core.*
import flat.checker.core.CmpOp.{EQ, GE, LE, LT}
import flat.regex.RegExpr

import scala.annotation.tailrec
import scala.collection.mutable

enum Formula:
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

object Formula:
  def mkLAnd(formulas: List[Formula]): Formula =
    val nontrivial = formulas.filter {
      case True | LImp(_, True) => false
      case _ => true
    }
    if nontrivial.isEmpty then True else nontrivial.reduceRight(LAnd.apply)

  def mkLAnd(formulas: Formula*): Formula = mkLAnd(formulas.toList)

final class Types(store: Map[String, Type]):
  def apply(name: String): Type =
    val i = name.indexOf('@')
    val x = if i >= 0 then name.substring(0, i) else name
    store(x)

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

  import Formula.*

  def generate(program: Program): Formula =
    given Types = Types.from(program.vars)

    given Fresher = new Fresher

    wlp(program.body, True, True)(using pReturn = True)

  /** Compute the weakest liberal pre of a statement `stmt` and a post condition `post`. */
  private def wlp(stmt: Stmt, post: Formula, body: List[Stmt], pInv: Formula)
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
        val conds = destruct(b)
        val pTrue = conds.foldRight(wlp(s1, post, pInv)) { case (e, p) =>
          val pSide = mkLAnd(collectSideGoals(e))
          mkLAnd(pSide, LImp(e, p))
        }
        val pFalse = LImp(Not(b).copyLocation(b), wlp(s2, post, pInv))
        mkLAnd(pTrue, pFalse)
      case whileStmt@While(b, s, userInv) =>
        val inv =
          if userInv.isEmpty then Analyzer.guessLoopInv(whileStmt, body).map(_.copyLocation(b)) else userInv
        if userInv.isEmpty then
          logger.debug(s"Guessing invariants: ${inv.mkString(", ")}")
        val pInv = mkLAnd(inv.flatMap(collectSideGoals) ++ (for e <- inv yield Goal(e, InvariantMayViolate(e.loc))))
        val pSide = mkLAnd(collectSideGoals(b))
        val pEnter = (b :: inv).foldRight(wlp(s, pInv, pInv))(LImp.apply)
        val exitCond = mkOr(Not(b).copyLocation(b) :: collectBreakCond(s))
        val pExit = (exitCond :: inv).foldRight(post)(LImp.apply)
        val pLoop = mkLAnd(pEnter, pExit)
        val m = Map.from(for x <- Analyzer.getModifiedVars(whileStmt) yield x -> Var(fresher.fresh(x)))
        mkLAnd(pInv, pSide, pLoop.subst(m))
      case Break() => pInv
      case Return() => pReturn

  @tailrec
  private def wlp(body: List[Stmt], post: Formula, pInv: Formula)
                 (using types: Types, pReturn: Formula, fresher: Fresher): Formula =
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

  private def collectSideGoals(expr: Expr): List[Formula] = expr.walkAndCollect {
    case StrAt(str, index) => // 0 <= index < |str|
      Goal(And(GE(index, 0), LT(index, StrLen(str))), IndexMayOutOfBounds(index.loc))
    case StrToCode(str) => // |str| == 1
      Goal(EQ(StrLen(str), 1), TypeMayMismatch("char (string of length 1)", "string", str.loc))
    case StrFromCode(int) => // 0 <= int <= 0x2FFFF
      Goal(And(GE(int, 0), LE(int, 0x2FFFF)), IndexMayOutOfBounds(int.loc))
    case StrToInt(str) => // str in number
      HasType(str, LangType(RegExpr.number), str.loc)
  }