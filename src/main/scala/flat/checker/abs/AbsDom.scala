package flat.checker.abs

trait AbsDom[T]:
  def top: T
  def bot: T

  def subElement(x: T, y: T): Boolean

  def join(x: T, y: T): T

  def widen(x: T, y: T): T

extension [T: AbsDom as dom](x: T)
  def :<:(y: T): Boolean = dom.subElement(x, y)
  def |(y: T): T = dom.join(x, y)
  def ∇(y: T): T = dom.widen(x, y)

def kleene[T: AbsDom as dom](f: (T => T) => T => T)(t: T): T =
  var acc: T = dom.bot

  def iter(t: T): T =
    if t :<: acc then acc
    else
      acc = dom.widen(acc, t)
      f(iter)(acc)

  iter(t)

//
//trait Lattice[T]:
//  /** Top element. */
//  def top: T
//
//  /** Bottom element. */
//  def bot: T
//
//  extension (x: T)
//    /** Partial order relation. */
//    @targetName("subElemOf")
//    def :<:(y: T): Boolean
//
//    infix def min(y: T): T = if x :<: y then x else y
//    infix def max(y: T): T = if y :<: x then x else y
//
//    @targetName("join")
//    def |(y: T): T
//
//    @targetName("meet")
//    def &(y: T): T
//
//trait AbsDom[T] extends Lattice[T]:
//  extension (x: T)
//    infix def widen(y: T): T
//
//given SeqAbsDom[A: AbsDom]: AbsDom[Seq[A]] with
//  def top: Seq[A] = throw UnsupportedOperationException("top")
//
//  def bot: Seq[A] = throw UnsupportedOperationException("bot")
//
//  extension (l1: Seq[A])
//    @targetName("subElemOf")
//    def :<:(l2: Seq[A]): Boolean =
//      require(l1.length == l2.length)
//      (l1 zip l2).forall(_ :<: _)
//
//    @targetName("meet")
//    def &(l2: Seq[A]): Seq[A] =
//      require(l1.length == l2.length)
//      for (x, y) <- (l1 zip l2) yield x & y
//
//    @targetName("join")
//    def |(l2: Seq[A]): Seq[A] =
//      require(l1.length == l2.length)
//      for (x, y) <- (l1 zip l2) yield x | y
//
//    infix def widen(l2: Seq[A]): Seq[A] =
//      require(l1.length == l2.length)
//      for (x, y) <- (l1 zip l2) yield x widen y
//
//given MapAbsDom[A: AbsDom]: AbsDom[Map[String, A]] with
//  def top: Map[String, A] = throw UnsupportedOperationException("top")
//
//  def bot: Map[String, A] = throw UnsupportedOperationException("bot")
//
//  extension (l1: Map[String, A])
//    @targetName("subElemOf")
//    def :<:(l2: Map[String, A]): Boolean = l1.keySet.forall(x => l1(x) :<: l2(x))
//
//    @targetName("meet")
//    def &(l2: Map[String, A]): Map[String, A] =
//      Map.from(for x <- l1.keySet yield x -> (l1(x) & l2(x)))
//
//    @targetName("join")
//    def |(l2: Map[String, A]): Map[String, A] =
//      Map.from(for x <- l1.keySet yield x -> (l1(x) | l2(x)))
//
//    infix def widen(l2: Map[String, A]): Map[String, A] =
//      Map.from(for x <- l1.keySet yield x -> (l1(x) widen l2(x)))
