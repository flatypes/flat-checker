package flat.checker.abs

import flat.checker.Bound
import flat.checker.Bound.PosInf

final class AList(private val elems: Seq[(AVal, Option[Bound])]):
  def isEmpty: Boolean = elems.isEmpty

  def length: Range =
    elems.map {
      case (_, Some(ub)) => Range(0, ub)
      case (_, None) => 1: Range
    }.reduce(_ + _)

  def ++(other: AList): AList = AList(elems ++ other.elems)

  def get(k: Int): Option[AVal] =
    if k >= 0 then
      if k < elems.length && elems.take(k + 1).forall(_._2.isEmpty) then Some(elems(k)._1) else None
    else // k < 0
      if -k <= elems.length && elems.takeRight(-k).forall(_._2.isEmpty)
      then Some(elems(elems.length + k)._1) else None

  def map(f: AVal => AVal): AList = AList(for (v, times) <- elems yield (f(v), times))

  private val values = elems.map(_._1)

  def forall(p: AVal => Boolean): Boolean = values.forall(p)

  def exists(p: AVal => Boolean): Boolean = values.exists(p)

object AList:
  def top(elemTop: AVal): AList = AList(Seq(elemTop -> Some(PosInf)))

given AbsDom[AList]:
  def top: AList = AList.top(ATop)

  def bot: AList = throw UnsupportedOperationException()

  def subElement(l1: AList, l2: AList): Boolean = ???

  def join(l1: AList, l2: AList): AList = ???

  def widen(l1: AList, l2: AList): AList = ???