package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable

type StrIndex = Index[Char]
type StrListIndex = Index[List[Char]]

final case class IndexRange[T](from: Index[T], until: Index[T])

type StrIndexRange = IndexRange[Char]
type StrListIndexRange = IndexRange[List[Char]]

object IndexInferer extends LazyLogging:
  given Conversion[String, List[Char]] = _.toList

  given Conversion[BigInt, Int] = _.toInt

  def matchStrIndex(idx: Expr, str: Expr, goal: Goal): Option[StrIndex] = idx match
    case IntConst(i) if i >= 0 => Some(ILeft(i)) // i
    case StrLen(`str`) => Some(IRight(0)) // |str|
    case Add(StrLen(`str`), IntConst(i)) if i > 0 => Some(IRight(0)) // |str| + i ==> |str|
    case Sub(StrLen(`str`), IntConst(i)) if i > 0 => Some(IRight(i)) // |str| - i
    case StrIndexOf(`str`, StrConst(s), ei) => // str.indexOf(s, i)
      for start <- matchStrIndex(ei, str, goal) yield IFirst(s, start, 0)
    case Add(StrIndexOf(`str`, StrConst(s), ei), IntConst(k)) if k > 0 => // str.indexOf(s, i) + k
      for start <- matchStrIndex(ei, str, goal) yield IFirst(s, start, k)
    case Sub(StrIndexOf(`str`, StrConst(s), ei), IntConst(k)) if k > 0 => // str.indexOf(s, i) - k
      for start <- matchStrIndex(ei, str, goal) yield IFirst(s, start, -k)
    case _ => None

  def solveStrIndexRange(idx: Expr, str: Expr, goal: Goal): StrIndexRange = idx match
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

  def inferStrIndex(idx: Expr, str: Expr, goal: Goal): StrIndex | StrIndexRange =
    matchStrIndex(idx, str, goal)
      .getOrElse(solveStrIndexRange(idx, str, goal))

  private def shiftIndex[T](index: Index[T], offset: Int): Index[T] = index match
    case ILeft(i) => ILeft(i + offset)
    case IRight(0) => if offset < 0 then IRight(-offset) else IRight(0)
    case IRight(i) => IRight(i - offset)
    case IFirst(s, i, k) => IFirst(s, i, k + offset)

  private def shiftIndexRange[T](range: IndexRange[T], offset: Int): IndexRange[T] =
    IndexRange(shiftIndex(range.from, offset), shiftIndex(range.until, offset))

  private def collectStrRelatives(str: Expr): Expr => List[Expr] =
    _.collect:
      case e@StrIndexOf(`str`, CharConst(_) | StrConst(_), IntConst(i)) if i >= 0 => e

  private def strRelativeToIndex(expr: Expr, offset: Int): StrIndex = expr match
    case StrIndexOf(_, StrConst(s), IntConst(i)) => IFirst(s, ILeft(i), offset)
    case _ => throw new IllegalArgumentException(expr.show)

  def matchListIndex(idx: Expr, lst: Expr, goal: Goal): Option[StrListIndex] = idx match
    case IntConst(i) if i >= 0 => Some(ILeft(i)) // i
    case ListLen(`lst`) => Some(IRight(0)) // |lst|
    case Add(ListLen(`lst`), IntConst(i)) if i > 0 => Some(IRight(0)) // |lst| + i ==> |lst|
    case Sub(ListLen(`lst`), IntConst(i)) if i > 0 => Some(IRight(i)) // |lst| - i
    case ListIndexOf(`lst`, StrConst(s), ei) => // lst.indexOf(s, i)
      for start <- matchListIndex(ei, lst, goal) yield IFirst(List(s), start, 0)
    case Add(ListIndexOf(`lst`, StrConst(s), ei), IntConst(k)) if k > 0 => // lst.indexOf(s, i) + k
      for start <- matchListIndex(ei, lst, goal) yield IFirst(List(s), start, k)
    case Sub(ListIndexOf(`lst`, StrConst(s), ei), IntConst(k)) if k > 0 => // lst.indexOf(s, i) - k
      for start <- matchListIndex(ei, lst, goal) yield IFirst(List(s), start, -k)
    case _ => None

  def solveListIndexRange(idx: Expr, lst: Expr, goal: Goal): StrListIndexRange = idx match
    case Var(x) =>
      solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
    case Add(Var(x), IntConst(k)) =>
      val range = solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
      shiftIndexRange(range, k.toInt)
    case Sub(Var(x), IntConst(k)) =>
      val range = solveIndex(x, ListLen(lst), collectListRelatives(lst), listRelativeToIndex, goal)
      shiftIndexRange(range, -k.toInt)
    case _ => throw new IllegalArgumentException(idx.show)

  def inferListIndex(idx: Expr, lst: Expr, goal: Goal): StrListIndex | StrListIndexRange =
    matchListIndex(idx, lst, goal)
      .getOrElse(solveListIndexRange(idx, lst, goal))

  private def collectListRelatives(lst: Expr): Expr => List[Expr] =
    _.collect:
      case e@ListIndexOf(`lst`, StrConst(_), IntConst(i)) if i >= 0 => e

  private def listRelativeToIndex(expr: Expr, offset: Int): StrListIndex = expr match
    case ListIndexOf(_, StrConst(s), IntConst(i)) => IFirst(List(s), ILeft(i), offset)
    case _ => throw new IllegalArgumentException(expr.show)

  private def solveIndex[T](idxVar: String, len: Expr, collectRelatives: Expr => List[Expr],
                            relativeToIndex: (Expr, Int) => Index[T], goal: Goal): IndexRange[T] =
    val inequalities = collectInequalities(idxVar, goal)
    val relatives = inequalities.flatMap(collectRelatives).distinct
    val solver = new LPSolver(inequalities)
    val start = solveRangeStart(solver, Var(idxVar), len, relatives, relativeToIndex)
    val end = solveRangeEnd(solver, Var(idxVar), len, relatives, relativeToIndex)
    IndexRange(start, end)

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
                                 relativeToIndex: (Expr, Int) => Index[T]): Index[T] =
    // idx >= e + k ==> idx - e >= k
    val candidates = for e <- relatives; k <- solver.solveMin(Sub(idx, e)) yield relativeToIndex(e, k.toInt)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx >= i
    solver.solveMin(idx) match
      case Some(i) if i > 0 =>
        return ILeft(i.toInt)
      case _ =>

    // idx >= len - i ==> len - idx <= i
    solver.solveMax(Sub(len, idx)) match
      case Some(i) if i > 0 => IRight(i.toInt)
      case _ => ILeft(0) // default

  private def solveRangeEnd[T](solver: LPSolver, idx: Expr, len: Expr, relatives: List[Expr],
                               relativeToIndex: (Expr, Int) => Index[T]): Index[T] =
    // idx < e + k ==> idx - e < k ==> idx - e <= k - 1
    val candidates = for e <- relatives; k <- solver.solveMax(Sub(idx, e)) yield relativeToIndex(e, k.toInt + 1)
    candidates match
      case List(index) =>
        return index
      case _ =>

    // idx < len - i ==> len - idx > i ==> len - idx >= i + 1
    solver.solveMin(Sub(len, idx)) match
      case Some(i) if i > 1 =>
        return IRight(i.toInt - 1)
      case _ =>

    // idx < i
    solver.solveMax(idx) match
      case Some(i) if i >= 0 => ILeft(i.toInt + 1)
      case _ => IRight(0) // default
