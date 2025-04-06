package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Bound.*
import flat.checker.ReLang.*
import flat.checker.backend.core.*
import flat.checker.backend.core.ArithOp.SUB
import flat.checker.backend.core.CmpOp.*
import flat.checker.{*, given}

import scala.collection.mutable.ListBuffer

object Refiner extends LazyLogging:
  def refine(base: ReLang, withRefine: ReLangRefine)(using types: Types): ReLang = withRefine(base)

  def refine(str: Expr, declared: ReLang, premises: List[Expr])(using types: Types): ReLang =
    var r = declared
    for f <- collectRefiners(str, premises) do
      val old = r
      r = refine(r, f).normalize
      if r != old then
        logger.debug(s"refine $str : $old as $r by $f")
    r

  private def collectRefiners(str: Expr, premises: List[Expr])(using types: Types): List[ReLangRefine] =
    val refiners = ListBuffer.empty[ReLangRefine]
    val (baseStr, baseIndex) = str match
      case StrSlice(s, i, StrLen(s1)) if s1 == s => (s, i)
      case s => (s, Const(0))

    if premises.exists(_.collect { case StrLen(s) if s == baseStr => s }.nonEmpty) then
      val (lb, ub) = RangeSolver.solve(SUB(StrLen(baseStr), baseIndex), premises)(using types)
      if lb >= 0 && lb <= ub then
        refiners += LenIn(Interval(lb, ub))

    val chars = premises.flatMap(_.collect {
      case StrFind(s, Const(c: String)) if s == str && c.length == 1 => c.head
    })
    for c <- chars do
      val (lb, _) = RangeSolver.solve(StrFind(str, c.toString), premises)
      if lb >= 0 then
        refiners += Contain(c)
      else
        val (_, ub) = RangeSolver.solve(StrFind(str, c.toString), premises)
        if ub <= 0 then
          refiners += NotContain(c)

    premises.foreach {
      case Cmp(op@(EQ | NE), StrAt(s, i), Const(s1: String)) if s == baseStr && s1.length == 1 =>
        val c = s1.head
        for k <- decideIndex(i, baseStr, baseIndex, premises) do
          refiners += CharAt(k, op == EQ, c)
        val (lb, _) = RangeSolver.solve(SUB(i, baseIndex), premises)
        val (_, ub) = RangeSolver.solve(SUB(SUB(i, baseIndex), StrLen(baseStr)), premises)
        if lb == Fin(0) && ub == Fin(-1) then
          refiners += (if op == EQ then Contain(c) else NotContain(c))
      case Not(Cmp(op@(EQ | NE), StrAt(s, i), Const(s1: String))) if s == baseStr && s1.length == 1 =>
        val c = s1.head
        for k <- decideIndex(i, baseStr, baseIndex, premises) do
          refiners += CharAt(k, op == NE, c)
        val (lb, _) = RangeSolver.solve(SUB(i, baseIndex), premises)
        val (_, ub) = RangeSolver.solve(SUB(SUB(i, baseIndex), StrLen(baseStr)), premises)
        if lb == Fin(0) && ub == Fin(-1) then
          refiners += (if op == NE then Contain(c) else NotContain(c))
      case Cmp(op@(EQ | NE), s, Const(s1: String)) if s == str && s1.length == 1 =>
        refiners += CharAt(0, op == EQ, s1.head)
      case Not(Cmp(op@(EQ | NE), s, Const(s1: String))) if s == str && s1.length == 1 =>
        refiners += CharAt(0, op == NE, s1.head)
      case Not(Cmp(EQ, s, Const(s1: String))) if s == str && s1.length == 2 =>
        refiners += NotEqual(s1)
      case StrContains(s, Const(s1: String)) if s == str && s1.length == 1 =>
        refiners += Contain(s1.head)
      case Not(StrContains(s, Const(s1: String))) if s == str && s1.length == 1 =>
        refiners += NotContain(s1.head)
      case Cmp(EQ, StrFind(s, Const(s1: String)), Const(k: Int)) if s == str && s1.length == 1 && k >= 0 =>
        refiners += CharAt(k, true, s1.head)
      case _ =>
    }

    refiners.toList

  private def decideIndex(index: Expr, baseStr: Expr, baseIndex: Expr,
                          premises: List[Expr])(using types: Types): Option[Int] =
    val (lb, ub) = RangeSolver.solve(SUB(index, baseIndex), premises)
    if lb >= 0 && lb.isFin && lb == ub then
      return Some(lb.asInt)
    val (lb1, ub1) = RangeSolver.solve(SUB(SUB(index, StrLen(baseStr)), baseIndex), premises)
    if lb1 < 0 && lb1.isFin && lb1 == ub1 then
      return Some(lb1.asInt)
    None

trait ReLangRefine extends LazyLogging:
  def apply(base: ReLang): ReLang

final case class LenIn(interval: Interval) extends ReLangRefine:
  require(interval.lb >= 0, interval.toString)

  def apply(base: ReLang): ReLang =
    if (base.length & interval).isEmpty then
      return ReNone
    if interval.ub == Fin(0) then
      return ReEmpty
    base match
      case ReUnion(r1, r2) =>
        ReUnion(apply(r1), apply(r2)).normalize
      case ReConcat(r1, r2) if r1.length.isInt && !r2.length.isInt =>
        val r21 = LenIn(safeSub(interval, r1.length))(r2)
        ReConcat(r1, r21)
      case ReConcat(r1, r2) if r2.length.isInt && !r1.length.isInt =>
        val r11 = LenIn(safeSub(interval, r2.length))(r1)
        ReConcat(r11, r2)
      case ReStar(r) =>
        val i = r.length.ub match
          case Fin(k) => Math.ceilDiv(interval.lb.asInt, k)
          case PosInf => 1
          case _ => assert(false)
        val j = (interval.ub, r.length.lb.asInt) match
          case (PosInf, _) => PosInf
          case (Fin(m), 0) => PosInf // better: (r \ {ε}) ^ m
          case (Fin(m), k) => Fin(Math.floorDiv(m, k))
          case _ => assert(false)
        r.loop(i, j).normalize
      case _ => base

  private def safeSub(i: Interval, j: Interval): Interval =
    val k = i - j
    if k.lb < 0 then Interval(0, k.ub) else k

final case class Contain(char: Char) extends ReLangRefine:
  def apply(base: ReLang): ReLang =
    base match
      case ReNone | ReEmpty => ReNone
      case ReChars(cs) =>
        if cs.contains(char) then ReChars(CharSet.of(char)) else ReNone
      case ReConcat(r1, r2) =>
        val r11 = apply(r1)
        val r21 = apply(r2)
        if r11.isEmpty && !r21.isEmpty then ReConcat(r1, r21)
        else if r21.isEmpty && !r11.isEmpty then ReConcat(r11, r2)
        else base
      case ReUnion(r1, r2) =>
        ReUnion(apply(r1), apply(r2)).normalize
      case ReStar(r) =>
        val r1 = apply(r)
        if r1.isEmpty then ReNone else ReConcat(r, base)

final case class NotContain(chars: Set[Char]) extends ReLangRefine:
  def apply(base: ReLang): ReLang =
    base match
      case ReNone => ReNone
      case ReEmpty => ReEmpty
      case ReChars(cs) =>
        val cs1 = cs -- chars
        if cs1.isEmpty then ReNone else ReChars(cs1)
      case ReConcat(r1, r2) => ReConcat(apply(r1), apply(r2))
      case ReUnion(r1, r2) => ReUnion(apply(r1), apply(r2))
      case ReStar(r) => ReStar(apply(r))

object NotContain:
  def apply(char: Char): NotContain = NotContain(Set(char))

final case class CharAt(index: Int, isEQ: Boolean, char: Char) extends ReLangRefine:
  def apply(base: ReLang): ReLang =
    logger.debug(s"CharAt: $index ${if this.isEQ then "=" else "!="} $char for $base")
    if index >= 0 then
      val (ra, _) = go(base, index)
      ra.normalize
    else
      val (ra, _) = go(base.reverse, -index - 1)
      ra.normalize.reverse

  private def go(r: ReLang, k: Int): (ReLang, List[(ReLang, Int)]) =
    r match
      case ReNone | ReEmpty => (ReNone, Nil)
      case ReChars(cs) if k == 0 =>
        if (cs & CharSet(isEQ, Set(char))).nonEmpty then (r, Nil) else (ReNone, Nil)
      case ReChars(cs) =>
        (ReNone, List(r -> (k - 1)))
      case ReConcat(r1, r2) =>
        val accepted = ListBuffer.empty[ReLang]
        val todos = ListBuffer.empty[(ReLang, Int)]
        val (r1a, todo) = go(r1, k)
        accepted += ReConcat(r1a, r2)
        for (rt, kt) <- todo do
          val (at, todo1) = go(r2, kt)
          accepted += ReConcat(rt, at)
          for (rt1, kt1) <- todo1 do todos += (ReConcat(rt, rt1) -> kt1)
        if r1.nullable then
          val (r2a, todo2) = go(r2, k)
          accepted += r2a
          todo2.foreach(todos += _)
        (mkUnion(accepted.toSeq *), todos.toList)
      case ReUnion(r1, r2) =>
        val (r1a, todo1) = go(r1, k)
        val (r2a, todo2) = go(r2, k)
        (ReUnion(r1a, r2a), todo1 ++ todo2)
      case ReStar(r1) =>
        val accepted = ListBuffer.empty[ReLang]
        val todos = ListBuffer.empty[(ReLang, Int)]
        val (r1a, todo) = go(r1, k)
        accepted += ReConcat(r1a, r)
        for (rt, kt) <- todo do
          val (at, todo1) = go(r, kt)
          accepted += ReConcat(rt, at)
          for (rt1, kt1) <- todo1 do todos += (ReConcat(rt, rt1) -> kt1)
        (mkUnion(accepted.toSeq *), todos.toList)

final case class NotEqual(string: String) extends ReLangRefine:
  require(string.length == 2)

  def apply(base: ReLang): ReLang =
    if base.first.isSingleton && base.first.contains(string.head) then
      CharAt(1, false, string.charAt(1)).apply(base)
    else base
