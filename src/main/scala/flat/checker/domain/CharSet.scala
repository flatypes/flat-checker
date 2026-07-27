package flat.checker.domain

import flat.checker.util.CharSetUtil.compress

final case class CharSet(pos: Boolean, chars: Set[Char]):
  def isEmpty: Boolean = pos && chars.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def isFull: Boolean = !pos && chars.isEmpty

  def isSingleton: Boolean = pos && chars.size == 1

  def contains(c: Char): Boolean = if pos then chars.contains(c) else !chars.contains(c)

  def unary_! : CharSet = new CharSet(!pos, chars)

  def toSet: Set[Char] = if pos then chars else Set.from(0.toChar to 127.toChar) -- chars

  def subsetOf(that: CharSet): Boolean = (this.pos, that.pos) match
    case (true, true) => chars.subsetOf(that.chars)
    case (true, false) => (chars & that.chars).isEmpty
    case (false, true) => toSet.subsetOf(that.chars)
    case (false, false) => that.chars.subsetOf(chars)

  def equiv(that: CharSet): Boolean = this.subsetOf(that) && that.subsetOf(this)

  def |(that: CharSet): CharSet = (this.pos, that.pos) match
    case (true, true) => new CharSet(true, this.chars | that.chars)
    case (true, false) => new CharSet(false, that.chars -- this.chars)
    case (false, true) => new CharSet(false, this.chars -- that.chars)
    case (false, false) => new CharSet(false, this.chars & that.chars)

  def &(that: CharSet): CharSet = (this.pos, that.pos) match
    case (true, true) => new CharSet(true, this.chars & that.chars)
    case (true, false) => new CharSet(true, this.chars -- that.chars)
    case (false, true) => new CharSet(true, that.chars -- this.chars)
    case (false, false) => new CharSet(false, this.chars | that.chars)

  def -(c: Char): CharSet = if pos then new CharSet(true, chars - c) else new CharSet(false, chars + c)

  def head: Char = if pos then chars.head else throw new NoSuchElementException("head of complement char set")

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

given Domain[Char, CharSet] with
  def top: CharSet = CharSet.full

  def bot: CharSet = CharSet.empty

  def mkSingleton(c: Char): CharSet = new CharSet(true, Set(c))

  extension (a: CharSet)
    def isEmpty: Boolean = a.isEmpty
    def subsetOf(b: CharSet): Boolean = a.subsetOf(b)
    def |(b: CharSet): CharSet = a | b
    def &(b: CharSet): CharSet = a & b
    def unary_~ : CharSet = !a

    def contains(c: Char): Boolean = a.contains(c)
    def representative: Char =
      if a.pos then a.chars.min else (a.chars.min - 1).toChar
    def -(c: Char): CharSet = a - c
