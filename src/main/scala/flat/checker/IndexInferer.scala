package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*
import flat.regex.*

import scala.collection.mutable.ListBuffer

final case class IndexShifted(base: BasicIndex, offset: Int) extends Index

final case class IndexInterval(lb: Index, ub: Index) extends Index

extension (index: Index)
  def shift(offset: Int): Index =
    index match
      case IndexL(k) => IndexL(k + offset)
      case IndexR(k) => IndexR((k - offset) max 0)
      case b: BasicIndex => if offset == 0 then b else IndexShifted(b, offset)
      case IndexShifted(b, k) => if k + offset == 0 then b else IndexShifted(b, k + offset)
      case IndexInterval(lb, ub) => IndexInterval(lb.shift(offset), ub.shift(offset))

  def concretize(str: Expr): Expr = index match
    case IndexL(k) => k
    case IndexR(k) => SUB(Length(str), k)
    case IndexAt(t) => Find(str, t)
    case IndexShifted(b, k) => ADD(b.concretize(str), k)
    case _: IndexInterval => throw IllegalArgumentException(index.toString)

/** Index Inference. */
class IndexInferer(using config: Config, ctx: PrfCtx) extends LazyLogging:
  /** Infer an abstract index for a given `idx` of `str`. */
  def infer(idx: Expr, str: Expr): Index = idx match
    case Const(n: Int) if n >= 0 => IndexL(n)
    case Length(e) if e == str => IndexR(0)
    case Arith(SUB, Length(e), Const(n: Int)) if e == str && n > 0 => IndexR(n)
    case Find(e, Const(t: String)) if e == str => IndexAt(t)
    case Arith(op, Find(e, Const(t: String)), Const(k: Int)) if e == str =>
      IndexShifted(IndexAt(t), if op == ADD then k else -k)
    case Var(x) => solve(x, str)
    case Arith(ADD, Var(x), Const(k: Int)) => solve(x, str).shift(k)
    case Arith(SUB, Var(x), Const(k: Int)) => solve(x, str).shift(-k)
    case _ => throw UnsupportedOperationException(idx.toString)

  private def solve(x: String, str: Expr): Index =
    val idx = Var(x)
    val constraints = ctx.premises.collect { case c@Cmp(op, _, _) if op != NE && c.collectVars.contains(x) => c }
    val solver = LPSolver(constraints)
    // High priority: IndexAt (with potential shift)
    val finds = constraints.flatMap(c => List(c.left, c.right)).collect:
      case f@Find(e, Const(t: String)) if e == str => (f, t)
    val lbAs = ListBuffer.empty[Index]
    val ubAs = ListBuffer.empty[Index]
    for (f, t) <- finds.distinct do
      val (minK, maxK) = solver.solve(SUB(idx, f))
      for k <- minK do lbAs += IndexAt(t).shift(k)
      for k <- maxK do ubAs += IndexAt(t).shift(k)
    val lbA = lbAs.length match
      case 0 => None
      case 1 => Some(lbAs.head)
      case _ => throw UnsupportedOperationException(s"solve $x: ambiguous choice of lbA from " + lbAs.mkString(", "))
    val ubA = ubAs.length match
      case 0 => None
      case 1 => Some(ubAs.head)
      case _ => throw UnsupportedOperationException(s"solve $x: ambiguous choice of lbA from " + lbAs.mkString(", "))
    if lbA.isDefined && lbA == ubA then
      return lbA.get
    // Middle priority: IndexL
    val (minL, maxL) = solver.solve(idx)
    if minL.isDefined && minL == maxL then
      return IndexL(minL.get)
    val (lbL, ubL) = (minL.map(IndexL(_)), maxL.map(IndexL(_)))
    // Low priority: IndexR
    val (minR, maxR) = solver.solve(SUB(Length(str), idx))
    if minR.isDefined && minR == maxR then
      return IndexR(minR.get)
    val (lbR, ubR) = (maxR.map(IndexR(_)), minR.map(IndexR(_)))
    // Choose interval bounds
    val lb = (lbA, lbL, lbR) match
      case (Some(i), None | Some(IndexL(0)), None) => i
      case (None, Some(i), None) => i
      case (None, None | Some(IndexL(0)), Some(i)) => i
      case (None, None, None) => IndexL(0)
      case _ => throw UnsupportedOperationException(s"solve $x: ambiguous choice of lb from $lbA, $lbL, $lbR")
    val ub = (ubA, ubL, ubR) match
      case (Some(i), None, None | Some(IndexR(1))) => i
      case (None, Some(i), None | Some(IndexR(1))) => i
      case (None, None, Some(i)) => i
      case (None, None, None) => IndexR(0)
      case _ => throw UnsupportedOperationException(s"solve $x: ambiguous choice of ub from $ubA, $ubL, $ubR")
    IndexInterval(lb, ub)
