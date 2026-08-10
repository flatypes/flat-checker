package flat.checker.flan

sealed trait Sort

case object NullSort extends Sort

case object BoolSort extends Sort

case object IntSort extends Sort

case object CharSort extends Sort

final case class SeqSort(elemSort: Sort) extends Sort

val stringSort = SeqSort(CharSort)

val strListSort = SeqSort(stringSort)

final case class SetSort(elemSort: Sort) extends Sort

final case class MapSort(keySort: Sort, valueSort: Sort) extends Sort

final case class TupleSort(elemSorts: List[Sort]) extends Sort

val unitSort = TupleSort(Nil)

def mkTupleSort(elemSorts: Sort*): TupleSort = TupleSort(elemSorts.toList)

final case class FunSort(argSorts: List[Sort], returnSort: Sort) extends Sort:
  def arity: Int = argSorts.length

final case class UnionSort(left: Sort, right: Sort) extends Sort

// Only for typing
case object NoSort extends Sort
