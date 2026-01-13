package flat.regex

import org.apache.commons.lang3.StringUtils

import scala.collection.Searching.*

/** A set of character ranges.
 *
 * @param starts the starting characters (inclusive) of the ranges
 * @param ends   the ending characters (inclusive) of the ranges
 * @note the ranges must be sorted and non-overlapping
 */
class CharRangeSet private(private val starts: List[Char], private val ends: List[Char]):
  def ranges: List[(Char, Char)] = starts.zip(ends)

  /** Tests if this set is empty. */
  def isEmpty: Boolean = starts.isEmpty

  /** Tests if this set is a singleton. */
  def isSingleton: Boolean = starts.length == 1 && starts.head == ends.head

  /** Performs a binary search of the given character `c`.
   *
   * @return `Found(i)` if `c` is within the range `i`, or
   *         `InsertionPoint(i)` if `c` is inbetween the ranges `i-1` and `i`
   */
  private def search(c: Char): SearchResult =
    starts.search(c) match
      case Found(i) => Found(i)
      case InsertionPoint(i) => if 0 <= i - 1 && c <= ends(i - 1) then Found(i - 1) else InsertionPoint(i)

  /** Tests if this set contains the given character `c`. */
  def contains(c: Char): Boolean =
    search(c) match
      case Found(_) => true
      case InsertionPoint(_) => false

  /** Tests if this set covers the given `range`, i.e., contains all its elements. */
  private def covers(range: (Char, Char)): Boolean =
    val (c1, c2) = range
    require(c1 <= c2)
    search(c1) match
      case Found(i) => c2 <= ends(i)
      case InsertionPoint(_) => false

  /** Tests if this set is a subset of `that` set. */
  def isSubsetOf(that: CharRangeSet): Boolean = ranges.forall(that.covers)

  /** Includes the given `range` in this set. */
  def incl(range: (Char, Char)): CharRangeSet =
    val (c1, c2) = range
    require(c1 <= c2)
    val (i, start) = search(c1) match
      case Found(i) => (i, starts(i))
      case InsertionPoint(i) => if 0 <= i - 1 && ends(i - 1) == c1 - 1 then (i - 1, starts(i - 1)) else (i, c1)
    val (j, end) = search(c2) match
      case Found(j) => (j + 1, ends(j))
      case InsertionPoint(j) => if j < starts.length && starts(j) == c2 + 1 then (j + 1, ends(j)) else (j, c2)
    CharRangeSet(starts.patch(i, List(start), j - i), ends.patch(i, List(end), j - i))

  /** Union with `that` set. */
  def |(that: CharRangeSet): CharRangeSet = that.ranges.foldLeft(this)(_.incl(_))

  /** Shrinks to the given `range`. */
  private def shrink(range: (Char, Char)): CharRangeSet =
    val (c1, c2) = range
    require(c1 <= c2)
    val (i, updateStart) = search(c1) match
      case Found(i) => (i, true)
      case InsertionPoint(i) => (i, false)
    val (j, updateEnd) = search(c2) match
      case Found(j) => (j + 1, true)
      case InsertionPoint(j) => (j, false)
    if i == j then CharRangeSet.empty
    else CharRangeSet(
      if updateStart then c1 :: starts.slice(i + 1, j) else starts.slice(i, j),
      if updateEnd then ends.slice(i, j - 1) :+ c2 else ends.slice(i, j))

  /** Intersection with `that` set. */
  def &(that: CharRangeSet): CharRangeSet =
    val ranges = that.ranges
    if ranges.isEmpty then CharRangeSet.empty else ranges.map(shrink).reduce(_ | _)

  /** Excludes the given `range` from this set. */
  def excl(range: (Char, Char)): CharRangeSet =
    val (c1, c2) = range
    require(c1 <= c2)
    val (i, startsL, endsL) = search(c1) match
      case Found(i) => if starts(i) == c1 then (i, Nil, Nil) else (i, List(starts(i)), List((c1 - 1).toChar))
      case InsertionPoint(i) => (i, Nil, Nil)
    val (j, startsR, endsR) = search(c2) match
      case Found(j) => if ends(j) == c2 then (j + 1, Nil, Nil) else (j + 1, List((c2 + 1).toChar), List(ends(j)))
      case InsertionPoint(j) => (j, Nil, Nil)
    CharRangeSet(starts.patch(i, startsL ++ startsR, j - i), ends.patch(i, endsL ++ endsR, j - i))

  /** Set minus with `that`. */
  def --(that: CharRangeSet): CharRangeSet = that.ranges.foldLeft(this)(_.excl(_))

  override def equals(obj: Any): Boolean = obj match
    case that: CharRangeSet => starts == that.starts && ends == that.ends
    case _ => false

  def toSet: Set[Char] = ranges.flatMap(_ to _).toSet

  def toJavaRegex: String =
    val sb = new StringBuilder
    for (c1, c2) <- ranges do
      if c1 == c2 then sb ++= formatChar(c1)
      else sb ++= formatChar(c1) + "-" + formatChar(c2)
    sb.toString

  private def formatChar(c: Char): String = c match
    case '[' | ']' | '-' | '\\' => "\\" + c.toString
    case _ if StringUtils.isAsciiPrintable(c.toString) => c.toString
    case _ => String.format("\\u%04X", c.toInt)

  override def toString: String = "[" + toJavaRegex + "]"

object CharRangeSet:
  val empty: CharRangeSet = CharRangeSet(Nil, Nil)

  /** Creates a `CharRangeSet` from the given elements. */
  def of(elems: (Char | (Char, Char))*): CharRangeSet =
    val ranges = elems.map:
      case c: Char => (c, c)
      case r: (Char, Char) => r
    ranges.foldLeft(empty)(_.incl(_))

final class NCharSet(val positive: Boolean, val set: CharRangeSet):
  /** Tests if this set is empty. */
  def isEmpty: Boolean = positive && set.isEmpty

  /** Tests if this set is full. */
  def isFull: Boolean = !positive && set.isEmpty

  /** Tests if this set is a singleton. */
  def isSingleton: Boolean = positive && set.isSingleton

  /** Tests if this set contains the given character `c`. */
  def contains(c: Char): Boolean = if positive then set.contains(c) else !set.contains(c)

  /** Tests if this set is a subset of `that` set. */
  def isSubsetOf(that: NCharSet): Boolean =
    if positive && that.positive then set.isSubsetOf(that.set)
    else if !positive && !that.positive then that.set.isSubsetOf(set)
    else if positive then (set & that.set).isEmpty // !that.positive
    else ???

  /** Complement set. */
  def unary_! : NCharSet = NCharSet(!positive, set)

  /** Union with `that` set. */
  def |(that: NCharSet): NCharSet =
    if positive && that.positive then NCharSet(true, set | that.set)
    else if !positive && !that.positive then NCharSet(false, set & that.set)
    else if positive then NCharSet(false, that.set -- set) // !that.positive
    else NCharSet(false, set -- that.set) // !positive && that.positive

  /** Intersection with `that` set. */
  def &(that: NCharSet): NCharSet =
    if positive && that.positive then NCharSet(true, set & that.set)
    else if !positive && !that.positive then NCharSet(false, set | that.set)
    else if positive then NCharSet(true, set -- that.set) // !that.positive
    else NCharSet(true, that.set -- set) // !positive && that.positive

  /** Excludes the given character `c`. */
  def -(c: Char): NCharSet =
    if positive then NCharSet(true, set.excl((c, c))) else NCharSet(false, set.incl((c, c)))

  /** Returns the minimal character. */
  def min: Char =
    if isEmpty then
      throw IllegalArgumentException("empty CharSet")
    val (c1, c2) = set.ranges.head
    if positive then c1
    else if c1 > Character.MIN_VALUE then Character.MIN_VALUE
    else (c2 + 1).toChar

  override def equals(obj: Any): Boolean = obj match
    case that: NCharSet => positive == that.positive && set == that.set
    case _ => false

  /** Converts to `Set[Char]`. */
  def toSet: Set[Char] =
    val chars = set.ranges.flatMap(_ to _).toSet
    if positive then chars else Set.from(Character.MIN_VALUE to Character.MAX_VALUE) -- chars

  /** Converts to a Java regex string. */
  def toJavaRegex: String =
    if isEmpty then throw IllegalArgumentException("empty CharSet")
    else if isFull then "."
    else (if positive then "[" else "[^") + set.toJavaRegex + "]"

  override def toString: String =
    if isEmpty then "[]"
    else if isFull then "[^]"
    else toJavaRegex

object NCharSet:
  val empty: NCharSet = NCharSet(true, CharRangeSet.empty)
  val full: NCharSet = NCharSet(false, CharRangeSet.empty)
  val asciiDigit: NCharSet = NCharSet(true, CharRangeSet.of('0' -> '9'))
  val asciiSpace: NCharSet = NCharSet(true, CharRangeSet.of(' ', '\t', '\n', '\r', '\f', '\u000B'))
  val asciiWord: NCharSet = NCharSet(true, CharRangeSet.of('A' -> 'Z', 'a' -> 'z', '0' -> '9', '_'))