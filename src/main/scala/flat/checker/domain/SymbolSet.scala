package flat.checker.domain

trait SymbolSet[A]:
  type Set = A

  def empty: A

  def full: A

  type Symbol
  type Word = List[Symbol]

  def singleton(x: Symbol): A

  extension (a: A)
    def isEmpty: Boolean
    def nonEmpty: Boolean = !a.isEmpty
    def contains(x: Symbol): Boolean
    def |(b: A): A
    def -(x: Symbol): A
    def &(xs: List[Symbol]): A = xs.filter(a.contains).foldLeft(empty) { (b, x) => b | singleton(x) }
    def --(xs: List[Symbol]): A = xs.filter(a.contains).foldLeft(a) { (b, x) => b - x }