package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.ExprOps.*
import flat.checker.ast.*
import flat.regex.RegEx

final class Types(store: Map[String, Type]):
  def apply(name: String): Type =
    val i = name.indexOf('@')
    val x = if i >= 0 then name.substring(0, i) else name
    store(x)

  def strVars: Set[String] = store.filter(_._2.toSort == Sort.S).keySet

  def intVars: Set[String] = store.filter(_._2.toSort == Sort.I).keySet

object Types:
  def from(it: IterableOnce[(String, Type)]) = Types(Map.from(it))

/** Proof Context. Recently added premises first. */
class PrfCtx private(val premises: List[Expr])
                    (using val types: Types, smtSolver: SMTSolver) extends LazyLogging:
  /** Returns the RE of the given `str`. */
  def getLang(str: Expr): RegEx =
    premises.collectFirst { case TypeTest(e, LangType(r)) if e == str => r }
      .getOrElse:
        str match
          case Var(x) => types(x).asInstanceOf[LangType].re
          case _ => throw IllegalArgumentException(s"regex not found: $str")

  /** If there is a premise that tells the RE of some suffix of `str`, i.e., `str[i:]` has type `r`,
   * then returns this base index `i` and the RE `r`. */
  def lookupSuffixLang(str: Expr): Option[(Expr, RegEx)] =
    premises.collectFirst:
      case TypeTest(suffix@Substr(e1, ei, Length(e2)), LangType(r)) if e1 == str && e2 == str => (ei, r)

  /** Creates a new proof context with given conditions added. */
  def ++(conds: List[Expr]): PrfCtx = PrfCtx(conds.map(simpl).flatMap(conjuncts) ++ premises)

  inline def +(cond: Expr): PrfCtx = ++(List(cond))

  def destructibleCandidates: List[Expr] =
    premises.filter:
      case _: Or => true
      case b => b.collectFirst { case _: Ite => () }.isDefined

  def caseIf(cond: Expr, isTrue: Boolean): PrfCtx =
    val (ifs, others) = premises.partition(_.collectFirst { case Ite(b, _, _) if b == cond => () }.isDefined)
    val ctx = PrfCtx(others)
    val ps = ifs.map(_.transform { case Ite(b, e1, e2) if b == cond => if isTrue then e1 else e2 })
    ctx ++ ((if isTrue then cond else Not(cond)) :: ps)

  def caseOr(or: Or, k: Int): PrfCtx =
    val others = premises.filter(_ != or)
    if others.length == premises.length then
      return this // not found
    val bs = or.disjuncts
    val ctx = PrfCtx(others)
    ctx ++ (bs(k) :: bs.take(k).map(Not(_)))

  def proves(conclusion: Expr): Boolean = premises.contains(conclusion) || smtSolver.proves(conclusion)

  override def toString: String = premises.mkString(" ∧ ")

object PrfCtx:
  /** The empty proof context. */
  def empty(using types: Types, smtSolver: SMTSolver) = PrfCtx(Nil)
