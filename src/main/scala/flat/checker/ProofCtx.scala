package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.{RERefiner, RegExpr}

final class ProofCtx(val assumptions: List[Expr], langs: Map[String, RegExpr])
                    (using val types: Types) extends LazyLogging:

  import CmpOp.*
  import Direction.*
  import RegExpr.*

  def impliesFalse: Boolean =
    assumptions.contains(Const(false)) || langs.exists(_._2 == RENone)

  def getLang(varName: String): RegExpr =
    langs.getOrElse(varName, types(varName).asInstanceOf[LangType].re)

  def +(newAssumption: Expr): ProofCtx =
    val res1 =
      if impliesFalse then langs
      else refine(newAssumption) match
        case Some(x -> r) if r != getLang(x) =>
          logger.debug(s"Refine $x => $r (by $newAssumption)")
          langs + (x -> r)
        case _ => langs
    ProofCtx(assumptions :+ newAssumption, res1)

  def ++(newAssumptions: List[Expr]): ProofCtx = newAssumptions.foldLeft(this)(_ + _)

  def withHints(hints: List[Expr]): ProofCtx = ProofCtx(assumptions ++ hints, langs)

  private def refine(bool: Expr): Option[(String, RegExpr)] = bool match
    case Cmp(op, StrLen(Var(x)), Const(n: Int)) =>
      val r = getLang(x)
      Some(x -> RERefiner.refineByLen(r, (op, n)))
    case Cmp(op@(EQ | NE), StrAt(Var(x), ei), Const(s: String)) =>
      val c = ensureChar(s)
      val r = getLang(x)
      val indexSolver = new IndexSolver(using this)
      indexSolver.solve(ei, Var(x)) match
        case IndexAt(_, L, k) => Some(x -> RERefiner.refineByCharAt(r, k, (op, c)))
        case IndexAt(_, R, k) if k > 0 => Some(x -> RERefiner.refineByCharAt(r.reverse, k - 1, (op, c)).reverse)
        case IndexRange(_, _) if op == NE =>
          val refiner = SuffixRefiner(using this)
          val c = ensureChar(s)
          val r = getLang(x)
          Some(x -> refiner.refineBySomeCharNotEqual(r, c))
        case _ => None
    case Cmp(op, StrFind(Var(x), Const(s: String)), Const(n: Int)) if s.length == 1 =>
      val c = ensureChar(s)
      val r = getLang(x)
      Some(x -> RERefiner.refineByIndexOf(r, c, (op, n)))
    case Cmp(NE, Var(x), Const(s: String)) =>
      val refiner = SuffixRefiner(using this)
      val r = getLang(x)
      Some(x -> refiner.refineByNotEqual(r, s))
    case _ => None

  private def ensureChar(s: String): Char =
    require(s.length == 1)
    s.head

object ProofCtx:
  def empty(using types: Types): ProofCtx = ProofCtx(Nil, Map.empty)