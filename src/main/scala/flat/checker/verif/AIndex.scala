package flat.checker.verif

import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable

enum AIndex[T]:
  /** Left-to-right index. First element has index 0. */
  case Left(k: Int)
  /** Right-to-left index. End has index 0. Last element has index 1. */
  case Right(k: Int)
  /** Index of the first occurrence of the slice. */
  case Of(slice: List[T], start: Int, offset: Int)

type StrIndex = AIndex[Char]
type StrListIndex = AIndex[String]

final case class AIndexRange[T](from: AIndex[T], until: AIndex[T])

type StrIndexRange = AIndexRange[Char]
type StrListIndexRange = AIndexRange[String]

object IndexInferer:
  def inferStrIndex(idx: Expr, str: Expr, goal: Goal): StrIndex | StrIndexRange = idx match
    case IntConst(i) if i >= 0 => AIndex.Left(i.toInt) // i
    case StrLength(`str`) => AIndex.Right(0) // |str|
    case Sub(StrLength(`str`), IntConst(i)) if i > 0 => AIndex.Right(i.toInt) // |str| - i
    case StrIndexOf(`str`, StrConst(s), IntConst(i)) if i >= 0 => // str.indexOf(s, i)
      AIndex.Of(s.toList, i.toInt, 0)
    case Add(StrIndexOf(`str`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // str.indexOf(s, i) + k
      AIndex.Of(s.toList, i.toInt, k.toInt)
    case Sub(StrIndexOf(`str`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // str.indexOf(s, i) - k
      AIndex.Of(s.toList, i.toInt, -k.toInt)
    case Var(x) =>
      solveIndex(x, StrLength(str), collectStrRelatives(str), strRelativeToIndex, goal)
    case _ => throw new IllegalArgumentException(idx.show)

  private def collectStrRelatives(str: Expr): Expr => List[Expr] =
    _.collect:
      case e@StrIndexOf(`str`, CharConst(_) | StrConst(_), IntConst(i)) if i >= 0 => e

  private def strRelativeToIndex(expr: Expr, offset: Int): StrIndex = expr match
    case StrIndexOf(_, StrConst(s), IntConst(i)) => AIndex.Of(s.toList, i.toInt, offset)
    case _ => throw new IllegalArgumentException(expr.show)

  def inferListIndex(idx: Expr, lst: Expr, goal: Goal): StrListIndex | StrListIndexRange = idx match
    case IntConst(i) if i >= 0 => AIndex.Left(i.toInt) // i
    case ListLength(`lst`) => AIndex.Right(0) // |lst|
    case Sub(ListLength(`lst`), IntConst(i)) if i > 0 => AIndex.Right(i.toInt) // |lst| - i
    case ListIndexOf(`lst`, StrConst(s), IntConst(i)) if i >= 0 => // lst.indexOf(s, i)
      AIndex.Of(List(s), i.toInt, 0)
    case Add(ListIndexOf(`lst`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // lst.indexOf(s, i) + k
      AIndex.Of(List(s), i.toInt, k.toInt)
    case Sub(ListIndexOf(`lst`, StrConst(s), IntConst(i)), IntConst(k)) if i >= 0 && k > 0 => // lst.indexOf(s, i) - k
      AIndex.Of(List(s), i.toInt, -k.toInt)
    case Var(x) =>
      solveIndex(x, ListLength(lst), collectListRelatives(lst), listRelativeToIndex, goal)
    case _ => throw new IllegalArgumentException(idx.show)

  private def collectListRelatives(lst: Expr): Expr => List[Expr] =
    _.collect:
      case e@ListIndexOf(`lst`, StrConst(_), IntConst(i)) if i >= 0 => e

  private def listRelativeToIndex(expr: Expr, offset: Int): StrListIndex = expr match
    case ListIndexOf(_, StrConst(s), IntConst(i)) => AIndex.Of(List(s), i.toInt, offset)
    case _ => throw new IllegalArgumentException(expr.show)

  private def solveIndex[T](idxVar: String, len: Expr, collectRelatives: Expr => List[Expr],
                            relativeToIndex: (Expr, Int) => AIndex[T], goal: Goal): AIndexRange[T] =
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

  private def solveRangeStart[T](solver: LPSolver, idx: Expr, len: Expr, relatives: List[Expr],
                                 relativeToIndex: (Expr, Int) => AIndex[T]): AIndex[T] =
    // idx >= e + k ==> idx - e >= k
    val candidates = for e <- relatives; k <- solver.solveMin(Sub(idx, e)) yield relativeToIndex(e, k)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx >= i
    solver.solveMin(idx) match
      case Some(i) if i > 0 =>
        return AIndex.Left(i)
      case _ =>

    // idx >= len - i ==> len - idx <= i
    solver.solveMax(Sub(len, idx)) match
      case Some(i) if i > 0 => AIndex.Right(i)
      case _ => AIndex.Left(0) // default

  private def solveRangeEnd[T](solver: LPSolver, idx: Expr, len: Expr, relatives: List[Expr],
                               relativeToIndex: (Expr, Int) => AIndex[T]): AIndex[T] =
    // idx < e + k ==> idx - e < k ==> idx - e <= k - 1
    val candidates = for e <- relatives; k <- solver.solveMax(Sub(idx, e)) yield relativeToIndex(e, k + 1)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx < i
    solver.solveMax(idx) match
      case Some(i) if i >= 0 =>
        return AIndex.Left(i + 1)
      case _ =>

    // idx < len - i ==> len - idx > i ==> len - idx >= i + 1
    solver.solveMin(Sub(len, idx)) match
      case Some(i) if i > 1 => AIndex.Right(i - 1)
      case _ => AIndex.Right(0) // default