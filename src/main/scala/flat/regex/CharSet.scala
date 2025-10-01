package flat.regex

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeJava

/** A finite Set of Unicode Characters (CS).
 *
 * @param set   the underlying set of characters
 * @param isInc if true, `set` stores the characters that should be included; otherwise excluded
 * */
final class CharSet private(private val set: Set[Char], private val isInc: Boolean):
  /** Tests if this CS is empty. */
  def isEmpty: Boolean = set.isEmpty && isInc

  /** Tests if this CS is full. */
  def isFull: Boolean = set.isEmpty && !isInc

  /** Tests if this CS is a singleton. */
  def isSingleton: Boolean = set.size == 1 && isInc

  /** Tests if this CS contains the given character `c`. */
  def contains(c: Char): Boolean = if isInc then set.contains(c) else !set.contains(c)

  /** Returns the smallest character in this CS if nonempty. */
  def head: Char = if isInc then set.head else CharSet.allChars.find(!set.contains(_)).get

  /** Returns the number of characters in this CS. Note: Full CS has 65536 characters. */
  def size: Int = if isInc then set.size else 65536 - set.size

  /** Tests if this CS is a subset of `that`. */
  def subsetOf(that: CharSet): Boolean =
    if isInc && that.isInc then set.subsetOf(that.set)
    else if !isInc && !that.isInc then that.set.subsetOf(set)
    else if isInc && !that.isInc then (set & that.set).isEmpty
    else LazyList.from(CharSet.allChars).filterNot(set.contains).forall(that.contains)

  /** Complement: full CS - this CS. */
  def unary_! : CharSet = new CharSet(set, !isInc)

  /** Union. */
  def |(that: CharSet): CharSet =
    if isInc && that.isInc then new CharSet(set | that.set, true)
    else if !isInc && !that.isInc then new CharSet(set & that.set, false)
    else
      val include = if isInc then set else that.set
      val exclude = if isInc then that.set else set
      new CharSet(exclude -- include, false)

  /** Intersection. */
  def &(that: CharSet): CharSet =
    if isInc && that.isInc then new CharSet(set & that.set, true)
    else if !isInc && !that.isInc then new CharSet(set | that.set, false)
    else
      val include = if isInc then set else that.set
      val exclude = if isInc then that.set else set
      new CharSet(include -- exclude, true)

  /** Disjointness: characters do not overlap. */
  def **(other: CharSet): Boolean = (this & other).isEmpty

  /** Excludes the given character `c`. */
  def -(c: Char): CharSet = new CharSet(if isInc then set - c else set + c, isInc)

  override def equals(obj: Any): Boolean = obj match
    case that: CharSet => isInc == that.isInc && set == that.set
    case _ => false

  /** Returns the usual `Set` encoding of this CS. */
  def toSet: Set[Char] = if isInc then set else CharSet.allChars.toSet -- set

  /** Returns the SMT-LIB encoding that consists of:
   *  - a sequence of characters or character ranges (both inclusive), and
   *  - a Boolean value indicates if the characters above are ''included'' (true) or ''excluded'' (false).
   */
  def toSMT: (Seq[Char | (Char, Char)], Boolean) =
    require(set.nonEmpty)
    val pts = set.toList.sorted.map(_.toInt)
    val is = 0 +: pts.indices.filter(i => i > 0 && pts(i) != pts(i - 1) + 1)
    val chars =
      for (i, j) <- is.zip(is.tail :+ pts.length)
        yield if i == j - 1 then pts(i).toChar else (pts(i).toChar, pts(j - 1).toChar)
    (chars, isInc)

  override def toString: String =
    val (chars, _) = toSMT
    val parts = chars.map:
      case c: Char => escapeJava(c.toString)
      case (c1, c2) => escapeJava(c1.toString) + "-" + escapeJava(c2.toString)
    "[" + (if isInc then "" else "^") + parts.mkString("") + "]"

object CharSet:
  /** The empty CS. */
  val empty: CharSet = new CharSet(Set.empty, true)

  /** The full CS. */
  val full: CharSet = new CharSet(Set.empty, false)

  /** The sequence of all Unicode characters. */
  val allChars: Seq[Char] = 0.toChar to 65535.toChar

  /** Creates a CS with the given `c`s. */
  def apply(c: Char*): CharSet = new CharSet(Set(c *), true)

  /** Creates a CS that contains all characters but ''not'' the given `c`s. */
  def not(c: Char*): CharSet = new CharSet(Set(c *), false)

  /** Creates a CS with the given characters in the collection `it`. */
  def from(it: IterableOnce[Char]): CharSet = new CharSet(Set.from(it), true)
