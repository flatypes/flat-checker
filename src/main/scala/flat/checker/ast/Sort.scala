package flat.checker.ast

/** Sort: a type that is not constrained. */
sealed trait Sort:
  def base: Sort = this

  def constraint: Option[Expr] = None

case object TopSort extends Sort

case object NoSort extends Sort

case object IntSort extends Sort

case object BoolSort extends Sort

case object CharSort extends Sort

case object StringSort extends Sort

case object UnitSort extends Sort

final case class TupleSort(elemSorts: List[Sort]) extends Sort:
  val arity: Int = elemSorts.length

final case class SeqSort(elemSort: Sort) extends Sort

final case class SetSort(elemSort: Sort) extends Sort

final case class MapSort(keySort: Sort, valueSort: Sort) extends Sort

final case class FunSort(argSorts: List[Sort], returnSort: Sort) extends Sort

final class SortingContext(val sorts: Map[String, Sort]):
  def apply(x: String): Sort = sorts(x.split(':').head)

trait Sorted:
  def sort(using ctx: SortingContext): Sort