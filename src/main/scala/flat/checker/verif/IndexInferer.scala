package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.{AIndex, AIndexRange}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable

object IndexInferer extends LazyLogging:
  def inferStrIndex(idx: Expr, str: Expr, goal: Goal): AIndex | AIndexRange = idx match
    case IntConst(i) if i >= 0 => AIndex.Const(i.toInt) // i
    case StrLen(`str`) => AIndex.End // |str|
    case Sub(StrLen(`str`), IntConst(i)) if i > 0 => AIndex.Const(-i.toInt) // |str| - i
    case StrIndexOf(`str`, StrConst(s), IntConst(i)) if i >= 0 => // str.indexOf(s, i)
      AIndex.Of(s, i.toInt, 0)
    case Add(StrIndexOf(`str`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // str.indexOf(s, i) + k
      AIndex.Of(s, i.toInt, k.toInt)
    case Sub(StrIndexOf(`str`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // str.indexOf(s, i) - k
      AIndex.Of(s, i.toInt, -k.toInt)
    case Var(x) =>
      solveIndex(x, StrLen(str), collectStrRelatives(str), strRelativeToIndex, goal)
    case Add(Var(x), IntConst(k)) =>
      val range = solveIndex(x, StrLen(str), collectStrRelatives(str), strRelativeToIndex, goal)
      shiftIndexRange(range, k.toInt)
    case Sub(Var(x), IntConst(k)) =>
      val range = solveIndex(x, StrLen(str), collectStrRelatives(str), strRelativeToIndex, goal)
      shiftIndexRange(range, -k.toInt)
    case _ =>
      logger.debug(s"$idx, $str")
      throw new IllegalArgumentException(idx.show)

  private def shiftIndex(index: AIndex, offset: Int): AIndex = index match
    case AIndex.Const(i) => AIndex.Const(i + offset)
    case AIndex.Of(s, i, k) => AIndex.Of(s, i, k + offset)
    case AIndex.End => if offset < 0 then AIndex.Const(-offset) else AIndex.End

  private def shiftIndexRange(range: AIndexRange, offset: Int): AIndexRange =
    AIndexRange(shiftIndex(range.from, offset), shiftIndex(range.until, offset))

  private def collectStrRelatives(str: Expr): Expr => List[Expr] =
    _.collect:
      case e@StrIndexOf(`str`, CharConst(_) | StrConst(_), IntConst(i)) if i >= 0 => e

  private def strRelativeToIndex(expr: Expr, offset: Int): AIndex = expr match
    case StrIndexOf(_, StrConst(s), IntConst(i)) => AIndex.Of(s, i.toInt, offset)
    case _ => throw new IllegalArgumentException(expr.show)

  def inferListIndex(idx: Expr, lst: Expr, goal: Goal): AIndex | AIndexRange = idx match
    case IntConst(i) if i >= 0 => AIndex.Const(i.toInt) // i
    case ListLen(`lst`) => AIndex.End // |lst|
    case Sub(ListLen(`lst`), IntConst(i)) if i > 0 => AIndex.Const(-i.toInt) // |lst| - i
    case ListIndexOf(`lst`, StrConst(s), IntConst(i)) if i >= 0 => // lst.indexOf(s, i)
      AIndex.Of(s, i.toInt, 0)
    case Add(ListIndexOf(`lst`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // lst.indexOf(s, i) + k
      AIndex.Of(s, i.toInt, k.toInt)
    case Sub(ListIndexOf(`lst`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // lst.indexOf(s, i) - k
      AIndex.Of(s, i.toInt, -k.toInt)
    case Var(x) =>
      solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
    case Add(Var(x), IntConst(k)) =>
      val range = solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
      shiftIndexRange(range, k.toInt)
    case Sub(Var(x), IntConst(k)) =>
      val range = solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
      shiftIndexRange(range, -k.toInt)
    case _ => throw new IllegalArgumentException(idx.show)

  private def collectListRelatives(lst: Expr): Expr => List[Expr] =
    _.collect:
      case e@ListIndexOf(`lst`, StrConst(_), IntConst(i)) if i >= 0 => e

  private def listRelativeToIndex(expr: Expr, offset: Int): AIndex = expr match
    case ListIndexOf(_, StrConst(s), IntConst(i)) => AIndex.Of(s, i.toInt, offset)
    case _ => throw new IllegalArgumentException(expr.show)

  private def solveIndex(idxVar: String, len: Expr, collectRelatives: Expr => List[Expr],
                         relativeToIndex: (Expr, Int) => AIndex, goal: Goal): AIndexRange =
    val inequalities = collectInequalities(idxVar, goal)
    val relatives = inequalities.flatMap(collectRelatives).distinct
    val solver = new LPSolver(inequalities)
    val start = solveRangeStart(solver, Var(idxVar), len, relatives, relativeToIndex)
    val end = solveRangeEnd(solver, Var(idxVar), len, relatives, relativeToIndex)
    AIndexRange(start, end)

  private def collectInequalities(variable: String, goal: Goal): List[Eq | Le | Lt] =
    val candidates: List[Eq | Le | Lt] = goal.premises.collect:
      case e: Eq => e
      case e: Le => e
      case e: Lt => e
    val selected = mutable.Set.empty[Int]
    val consideredVars = mutable.Set.empty[String]
    var newVars = Set(variable)
    while newVars.nonEmpty do
      val thisRoundSelected = mutable.Set.empty[Int]
      for
        i <- candidates.indices
        if !selected.contains(i)
        if (candidates(i).collect { case Var(x) => x }.toSet & newVars).nonEmpty
      do
        thisRoundSelected += i
      selected ++= thisRoundSelected
      consideredVars ++= newVars
      newVars = thisRoundSelected.flatMap(candidates(_).collect { case Var(x) => x }).toSet -- consideredVars
    selected.toList.map(candidates)

  private def solveRangeStart(solver: LPSolver, idx: Expr, len: Expr, relatives: List[Expr],
                              relativeToIndex: (Expr, Int) => AIndex): AIndex =
    // idx >= e + k ==> idx - e >= k
    val candidates = for e <- relatives; k <- solver.solveMin(Sub(idx, e)) yield relativeToIndex(e, k.toInt)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx >= i
    solver.solveMin(idx) match
      case Some(i) if i > 0 =>
        return AIndex.Const(i.toInt)
      case _ =>

    // idx >= len - i ==> len - idx <= i
    solver.solveMax(Sub(len, idx)) match
      case Some(i) if i > 0 => AIndex.Const(-i.toInt)
      case _ => AIndex.Const(0) // default

  private def solveRangeEnd(solver: LPSolver, idx: Expr, len: Expr, relatives: List[Expr],
                            relativeToIndex: (Expr, Int) => AIndex): AIndex =
    // idx < e + k ==> idx - e < k ==> idx - e <= k - 1
    val candidates = for e <- relatives; k <- solver.solveMax(Sub(idx, e)) yield relativeToIndex(e, k.toInt + 1)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx < len - i ==> len - idx > i ==> len - idx >= i + 1
    solver.solveMin(Sub(len, idx)) match
      case Some(i) if i > 1 =>
        return AIndex.Const(-(i.toInt - 1))
      case _ =>

    // idx < i
    solver.solveMax(idx) match
      case Some(i) if i >= 0 => AIndex.Const(i.toInt + 1)
      case _ => AIndex.End // default
