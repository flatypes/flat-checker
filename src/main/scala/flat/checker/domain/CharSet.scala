package flat.checker.domain

import flat.checker.util.CharSetUtil.compress

/** Infinite set of characters. */
final case class CharSet(pos: Boolean, chars: Set[Char]):
  def isEmpty: Boolean = pos && chars.isEmpty

  def nonEmpty: Boolean = !pos || chars.nonEmpty

  def isFull: Boolean = !pos && chars.isEmpty

  def isSingleton: Boolean = pos && chars.size == 1

  def subsetOf(that: CharSet): Boolean = (pos, that.pos) match
    case (true, true) => chars.subsetOf(that.chars)
    case (false, false) => that.chars.subsetOf(chars)
    case (true, false) => (chars & that.chars).isEmpty
    case (false, true) => false // NOTE: left is infinite, right is finite

  def |(that: CharSet): CharSet = (pos, that.pos) match
    case (true, true) => new CharSet(true, chars | that.chars)
    case (false, false) => new CharSet(false, chars & that.chars)
    case (true, false) => new CharSet(false, that.chars -- chars)
    case (false, true) => new CharSet(false, chars -- that.chars)

  def &(that: CharSet): CharSet = (pos, that.pos) match
    case (true, true) => new CharSet(true, chars & that.chars)
    case (false, false) => new CharSet(false, chars | that.chars)
    case (true, false) => new CharSet(true, chars -- that.chars)
    case (false, true) => new CharSet(true, that.chars -- chars)

  def unary_~ : CharSet = new CharSet(!pos, chars)

  def contains(c: Char): Boolean =
    if pos then chars.contains(c) else !chars.contains(c)

  def -(c: Char): CharSet =
    if pos then new CharSet(true, chars - c) else new CharSet(false, chars + c)

  def representative: Char =
    if pos then chars.min else (chars.min - 1).toChar

  def toFinSet(alphabet: Set[Char]): Set[Char] =
    if pos then chars else alphabet -- chars

  def map(f: Char => Char): CharSet = CharSet(pos, chars.map(f))

  override def toString: String =
    val sign = if pos then "" else "^"
    val ranges = chars.compress.map:
      case c: Char => c.toString
      case (c1, c2) => s"$c1-$c2"
    val content = ranges.mkString
    s"[$sign$content]"

object CharSet:
  val empty: CharSet = new CharSet(true, Set.empty)

  val full: CharSet = new CharSet(false, Set.empty)

  def apply(chars: Char*): CharSet = new CharSet(true, Set(chars *))

  def from(it: Iterable[Char]): CharSet = new CharSet(true, Set.from(it))

given SymbolSet[CharSet] with
  def empty: CharSet = CharSet.empty

  type Symbol = Char

  def singleton(c: Char): CharSet = CharSet(c)

  extension (a: CharSet)
    def isEmpty: Boolean = a.isEmpty
    def contains(c: Char): Boolean = a.contains(c)
    def |(b: CharSet): CharSet = a | b
    def -(c: Char): CharSet = a - c
