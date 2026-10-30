package flat.checker.domain

trait LitOps[T, S]:
  def empty: S

  def full: S

  def singleton(x: T): S

  def union(as: List[S]): S = as.foldLeft(empty)(_ | _)

  extension (a: S)
    def isEmpty: Boolean
    def nonEmpty: Boolean = !a.isEmpty
    def contains(x: T): Boolean
    def |(b: S): S
    def -(x: T): S
//def &(xs: List[T]): S = xs.filter(a.contains).foldLeft(empty) { (b, x) => b | singleton(x) }
//def --(xs: List[T]): S = xs.filter(a.contains).foldLeft(a) { (b, x) => b - x }
