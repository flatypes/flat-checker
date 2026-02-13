package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.summands
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*
import flat.regex.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class IndexShifted(base: Index, offset: Int) extends Index

final case class IndexInterval(lb: Index, ub: Index) extends Index

final case class ListIndexAt(t: String, fromLeft: Int, untilRight: Int) extends Index

extension (index: Index)
  def shift(offset: Int): Index =
    index match
      case IndexL(k) => IndexL(k + offset)
      case IndexR(k) => IndexR((k - offset) max 0)
      case b: (BasicIndex | ListIndexAt) => if offset == 0 then b else IndexShifted(b, offset)
      case IndexShifted(b, k) => if k + offset == 0 then b else IndexShifted(b, k + offset)
      case IndexInterval(lb, ub) => IndexInterval(lb.shift(offset), ub.shift(offset))

  def concretize(str: Expr): Expr = index match
    case IndexL(k) => Const(k)
    case IndexR(k) => SUB(StringLength(str), Const(k))
    case IndexAt(t) => StringIndexOf(str, Const(t))
    case IndexShifted(b, k) => ADD(b.concretize(str), Const(k))
    case _: IndexInterval => throw IllegalArgumentException(index.toString)

/** Index Inference. */
class IndexInferer(using config: Config, ctx: PrfCtx) extends LazyLogging:
  /** Infer an abstract index for a given `idx` of either a string or list. */
  def infer(idx: Expr, lstLike: Expr, isStr: Boolean = true, preferIndexL: Boolean = true): Index = idx match
    case Const(n: Int) if n >= 0 => IndexL(n)
    case StringLength(e) if e == lstLike => IndexR(0)
    case SeqLength(e) if e == lstLike => IndexR(0)
    case Arith(SUB, StringLength(e), Const(n: Int)) if e == lstLike && n > 0 => IndexR(n)
    case Arith(SUB, SeqLength(e), Const(n: Int)) if e == lstLike && n > 0 => IndexR(n)
    case StringIndexOf(e, Const(t: String)) if e == lstLike => IndexAt(t)
    case Arith(op, StringIndexOf(e, Const(t: String)), Const(k: Int)) if e == lstLike =>
      IndexShifted(IndexAt(t), if op == ADD then k else -k)
    case _ =>
      val vars = idx.summands.collect:
        case Var(x) => x
        case Negate(Var(x)) => x
      if vars.toSet.size == 1 then
        val x = vars.head
        solve(idx, x, lstLike, isStr, preferIndexL)
      else
        throw IllegalArgumentException("cannot infer index for: " + idx.toString)

  private def solve(idx: Expr, x: String, lstLike: Expr, isStr: Boolean, preferIndexL: Boolean): Index =
    val constraints = collectRelevantConstraints(x)
    val solver = LPSolver(constraints)
    // High priority: IndexAt (with potential shift)
    val finds = constraints.flatMap(c => List(c.left, c.right)).collect:
      case f@StringIndexOf(e, Const(t: String)) if e == lstLike => (f, IndexAt(t))
      case f@SeqIndexOf(e, Const(t: String), Const(i: Int), SeqLength(e1))
        if e == lstLike && e1 == e && i >= 0 => (f, ListIndexAt(t, i, 0))
      case f@SeqIndexOf(e, Const(t: String), Const(i: Int), Arith(SUB, SeqLength(e1), Const(j: Int)))
        if e == lstLike && e1 == e && i >= 0 && j >= 0 => (f, ListIndexAt(t, i, j))
    val lbAs = ListBuffer.empty[Index]
    val ubAs = ListBuffer.empty[Index]
    for (f, index) <- finds.distinct do
      val (minK, maxK) = solver.solve(SUB(idx, f))
      for k <- minK do lbAs += index.shift(k)
      for k <- maxK do ubAs += index.shift(k)
    val lbA = lbAs.length match
      case 0 => None
      case 1 => Some(lbAs.head)
      case _ =>
        logger.trace("simplify choose head of lbAs: {}", lbAs.mkString(", "))
        Some(lbAs.head)
    val ubA = ubAs.length match
      case 0 => None
      case 1 => Some(ubAs.head)
      case _ =>
        logger.trace("simplify choose head of ubAs: {}", ubAs.mkString(", "))
        Some(ubAs.head)
    if lbA.isDefined && lbA == ubA then
      return lbA.get
    // Equal priority: IndexL
    val (minL, maxL) = solver.solve(idx)
    val isConstLeft = minL.isDefined && minL == maxL
    val (lbL, ubL) = (minL.map(IndexL(_)), maxL.map(IndexL(_)))
    // Equal priority: IndexR
    val len = if isStr then StringLength(lstLike) else SeqLength(lstLike)
    val (minR1, maxR1) = solver.solve(SUB(len, idx))
    val minR = minR1 match
      case Some(n) if n >= 0 => Some(n)
      case _ => None
    val maxR = maxR1 match
      case Some(n) if n >= 0 => Some(n)
      case _ => None
    val isConstRight = minR.isDefined && minR == maxR
    val (lbR, ubR) = (maxR.map(IndexR(_)), minR.map(IndexR(_)))
    // Decide
    if isConstLeft && !isConstRight then
      return IndexL(minL.get)
    if isConstRight && !isConstLeft then
      return IndexR(minR.get)
    if isConstLeft && isConstRight then
      return if preferIndexL then IndexL(minL.get) else IndexR(minR.get)

    // Choose interval bounds
    logger.trace("choose lb from: {}, {}, {}", lbA, lbL, lbR)
    logger.trace("choose ub from: {}, {}, {}", ubA, ubL, ubR)
    val lb = (lbA, lbL, lbR) match
      case (Some(i), None | Some(IndexL(0)), None) => i
      case (None, Some(i), None) => i
      case (None, None | Some(IndexL(0)), Some(i)) => i
      case (None, Some(i), Some(_)) => i // for finite list, prefer IndexL
      case (Some(index@IndexShifted(ListIndexAt(_, i, _), k)), Some(IndexL(i1)), _) =>
        if i + k >= i1 then index // relative position >= absolute position, prefer relative
        else IndexL(i1) // otherwise prefer absolute
      case (None, None, None) => IndexL(0)
      case _ =>
        logger.warn("choose lb from: {}, {}, {}", lbA, lbL, lbR)
        throw UnsupportedOperationException(s"solve $x: ambiguous choice of lb")
    val ub = (ubA, ubL, ubR) match
      case (Some(i), None, None | Some(IndexR(1))) => i
      case (None, Some(i), None | Some(IndexR(1))) => i
      case (None, None, Some(i)) => i
      case (None, Some(_), Some(i)) => i // for finite list, prefer IndexR
      case (Some(index@IndexShifted(ListIndexAt(_, i, _), k)), Some(indexL@IndexL(i1)), indexR) =>
        // relative position >= absolute position, prefer absolute
        if i + k >= i1 then if indexR.isDefined then indexR.get else indexL
        else index
      case (None, None, None) => IndexR(0)
      case _ =>
        logger.warn("choose ub from: {}, {}, {}", ubA, ubL, ubR)
        throw UnsupportedOperationException(s"solve $x: ambiguous choice of ub")
    IndexInterval(lb, ub)

  private def collectRelevantConstraints(x: String): List[Cmp] =
    val candidates = ctx.premises.collect { case c@Cmp(op, _, _) if op != NE => c }
    val selected = mutable.Set.empty[Int]
    val consideredVars = mutable.Set.empty[String]
    var newVars = Set(x)
    while newVars.nonEmpty do
      val thisRoundSelected = mutable.Set.empty[Int]
      for
        i <- candidates.indices
        if !selected.contains(i)
        if (candidates(i).collectVars & newVars).nonEmpty
      do
        thisRoundSelected += i
      selected ++= thisRoundSelected
      consideredVars ++= newVars
      newVars = thisRoundSelected.flatMap(candidates(_).collectVars).toSet -- consideredVars
    selected.toList.map(candidates)