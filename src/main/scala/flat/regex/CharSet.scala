package flat.regex

import org.apache.commons.text.StringEscapeUtils

/** Unicode character set. */
final case class CharSet private(polarity: Boolean, chars: Set[Char]):
  // If polarity is true, `chars` stores included characters. Otherwise, it stores excluded characters.
  require(chars.size < 1000)

  /** Tests if this set is empty. */
  def isEmpty: Boolean = polarity && chars.isEmpty

  /** Tests if this set is full. */
  def isFull: Boolean = !polarity && chars.isEmpty

  /** Tests if this set is a singleton. */
  def isSingleton: Boolean = polarity && chars.size == 1

  /** Tests if this set contains the given `ch`. */
  def contains(ch: Char): Boolean =
    if polarity then chars.contains(ch) else !chars.contains(ch)

  /** Tests if this set is a subset of `that`. */
  def subsetOf(that: CharSet): Boolean =
    if polarity && that.polarity then chars.subsetOf(that.chars)
    else if !polarity && !that.polarity then that.chars.subsetOf(chars)
    else if polarity then /* !other.polarity */ (chars & that.chars).isEmpty
    else /* !polarity && other.polarity */ false

  /** Set complement: full set - this set. */
  def unary_! : CharSet = CharSet(!polarity, chars)

  /** Set union. */
  def |(other: CharSet): CharSet =
    if polarity && other.polarity then CharSet(true, chars | other.chars)
    else if !polarity && !other.polarity then CharSet(false, chars & other.chars)
    else
      val include = if polarity then chars else other.chars
      val exclude = if polarity then other.chars else chars
      CharSet(false, exclude -- include)

  /** Set intersection. */
  def &(other: CharSet): CharSet =
    if polarity && other.polarity then CharSet(true, chars & other.chars)
    else if !polarity && !other.polarity then CharSet(false, chars | other.chars)
    else
      val include = if polarity then chars else other.chars
      val exclude = if polarity then other.chars else chars
      CharSet(true, include -- exclude)

  /** Tests if two sets are disjoint: their characters do not overlap. */
  def **(other: CharSet): Boolean = (this & other).isEmpty

  /** Removes the given `ch` from this set. */
  def -(ch: Char): CharSet =
    if polarity then CharSet(true, chars - ch)
    else CharSet(false, chars + ch)

  /** Returns a random character of this set (if nonempty) as a representative. */
  def representative: Char =
    if polarity then
      require(chars.nonEmpty, "Cannot get representative for empty char set")
      chars.head
    else if chars.isEmpty then 0.toChar
    else if chars.min.toInt > 0 then (chars.min.toInt - 1).toChar else (chars.max.toInt + 1).toChar

  override def equals(obj: Any): Boolean = obj match
    case other: CharSet => polarity == other.polarity && chars == other.chars
    case _ => false

  override def toString: String =
    (if polarity then "" else "^") + StringEscapeUtils.escapeJava(chars.mkString(""))

object CharSet:
  /** The empty set. */
  val empty: CharSet = CharSet(true, Set.empty)

  /** The full set that contains all Unicode characters. */
  val full: CharSet = CharSet(false, Set.empty)

  /** Creates a set with the given `ch`s. */
  def apply(ch: Char*): CharSet = CharSet(true, Set(ch *))

  /** Creates a set that contains all characters but ''not'' the given `ch`s. */
  def not(ch: Char*): CharSet = CharSet(false, Set(ch *))

  /** Creates a set with the given characters in the collection `it`. */
  def from(it: IterableOnce[Char]): CharSet = CharSet(true, Set.from(it))
