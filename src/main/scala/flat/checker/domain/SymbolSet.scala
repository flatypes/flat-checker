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