package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.ExprOps.*
import flat.checker.core.*
import flat.regex.RegEx

/** Proof Context. */
class PrfCtx private(private val stablePremises: List[Expr], private val unstablePremises: List[Expr])
                    (using val types: Types) extends LazyLogging:
  /** Returns all premises in this proof context. */
  def premises: List[Expr] = stablePremises ++ unstablePremises

  /** Returns the RE of the given `str`. */
  def getLang(str: Expr): RegEx =
    stablePremises.reverse.collectFirst { case TypeTest(e, LangType(r)) if e == str => r }
      .getOrElse:
        str match
          case Var(x) => types(x).asInstanceOf[LangType].re
          case _ => throw IllegalArgumentException(s"regex not found: $str")

  /** If there is a premise that tells the RE of some suffix of `str`, i.e., `str[i:]` has type `r`,
   * then returns this base index `i` and the RE `r`. */
  def lookupSuffixLang(str: Expr): Option[(Expr, RegEx)] =
    stablePremises.reverse.collectFirst:
      case TypeTest(suffix@Substr(e1, ei, Length(e2)), LangType(r)) if e1 == str && e2 == str => (ei, r)

  def destructCandidates: List[Expr] = unstablePremises //.distinct

  /** Creates a new proof context with given conditions added. */
  def ++(conds: List[Expr]): PrfCtx =
    val (bs1, bs2) = conds.map(simpl).flatMap(conjuncts).partition:
      case _: Or => false
      case b => b.collectFirst { case _: Ite => () }.isEmpty
    PrfCtx(stablePremises ++ bs1, unstablePremises ++ bs2)

  inline def +(cond: Expr): PrfCtx = ++(List(cond))

  private def destruct(dp: Expr): List[(PrfCtx, List[Expr])] =
    dp match
      case _: Or =>
        assert(unstablePremises.contains(dp))
        val bs = dp.disjuncts
        val others = unstablePremises.filter(_ != dp)
        val ctx = PrfCtx(stablePremises, others)
        for i <- bs.indices.toList yield
          val ps = bs(i) :: bs.take(i).map(Not(_))
          ctx ++ ps -> List(bs(i))
      case _ =>
        val b = dp.collectFirst { case Ite(b, _, _) => b }.get
        destructIf(b)

  /** Splits this proof context by the value of the given ''disjunctive premises'' that are either logical-ORs
   * or contain `Ite`s.
   * For a logical-OR with `k` disjuncts, splits into `k` contexts, each of which holds one disjunctive case.
   * For an `Ite`, splits into two contexts: one assumes that the if-condition is true, and the other false.
   */
  def destruct(dps: List[Expr]): List[(PrfCtx, List[Expr])] = dps match
    case Nil => throw IllegalArgumentException("no candidate")
    case List(p) => destruct(p)
    case p :: rest =>
      for
        ctx1 -> ps1 <- destruct(p)
        ctx -> ps <- ctx1.destruct(rest)
      yield ctx -> (ps1 ++ ps)

  /** Splits this proof context by the value of the given `cond` into two contexts: one assumes that `cond` is true,
   * and the other false. Only `Ite`s with this `cond` will be replaced by its `then` or `else` branch.
   */
  def destructIf(cond: Expr): List[(PrfCtx, List[Expr])] =
    val (ifs, others) = unstablePremises.partition(_.collectFirst { case Ite(e, _, _) if e == cond => () }.isDefined)
    val ctx = PrfCtx(stablePremises, others)
    val ps1 = cond :: ifs.map(_.transform { case Ite(e, e1, _) if e == cond => e1 }) // if true
    val ps2 = Not(cond) :: ifs.map(_.transform { case Ite(e, _, e2) if e == cond => e2 }) // if false
    List(ctx ++ ps1 -> List(cond), ctx ++ ps2 -> List(Not(cond)))

  override def toString: String = premises.mkString(" ∧ ")

object PrfCtx:
  /** The empty proof context. */
  def empty(using types: Types) = PrfCtx(Nil, Nil)
