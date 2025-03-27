package flat.checker.backend

import flat.checker.backend.core.{ArrayType, IntervalType, Type, strType}
import flat.checker.{Bound, Interval, ReLang}

trait Hint:
  def toType: Type

final case class Index(cnf: List[ReLang], pos: Int) extends Hint:
  def toType: IntervalType = IntervalType(ReLang.fromCNF(cnf.take(pos)).length)

  override def toString: String = s"index($pos)"

final class Split(private val elems: Seq[(ReLang, Option[Bound])]) extends Hint:
  def toType: ArrayType = ArrayType(strType)

  def isEmpty: Boolean = elems.isEmpty

  def length: Interval =
    elems.map {
      case (_, Some(ub)) => Interval(0, ub)
      case (_, None) => 1: Interval
    }.reduce(_ + _)

  def ++(other: Split): Split = Split(elems ++ other.elems)

  def get(k: Int): Option[ReLang] =
    if k >= 0 then
      if k < elems.length && elems.take(k + 1).forall(_._2.isEmpty) then Some(elems(k)._1) else None
    else // k < 0
      if -k <= elems.length && elems.takeRight(-k).forall(_._2.isEmpty)
      then Some(elems(elems.length + k)._1) else None

  def map(f: ReLang => ReLang): Split = Split(for (v, times) <- elems yield (f(v), times))

  private val values = elems.map(_._1)

  def forall(p: ReLang => Boolean): Boolean = values.forall(p)

  def exists(p: ReLang => Boolean): Boolean = values.exists(p)
