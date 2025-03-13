package flat.checker

import org.apache.commons.text.StringEscapeUtils

import scala.collection.mutable

/** A set of Unicode characters. */
final class CharSet(private val polarity: Boolean, private val chars: Set[Char]):
  // If polarity is true, `chars` stores included characters. Otherwise, it stores excluded characters.
  require(chars.size < 1000)

  def isEmpty: Boolean = polarity && chars.isEmpty

  def isFull: Boolean = !polarity && chars.isEmpty

  def isSingleton: Boolean = polarity && chars.size == 1

  def isSingletonOf(c: Char): Boolean = polarity && chars == Set(c)

  def asChar: Char =
    require(isSingleton)
    chars.head

  def contains(ch: Char): Boolean =
    if polarity then chars.contains(ch) else !chars.contains(ch)

  def unary_! : CharSet = CharSet(!polarity, chars)

  def |(other: CharSet): CharSet =
    if polarity && other.polarity then CharSet(true, chars | other.chars)
    else if !polarity && !other.polarity then CharSet(false, chars & other.chars)
    else
      val include = if polarity then chars else other.chars
      val exclude = if polarity then other.chars else chars
      CharSet(false, exclude -- include)

  def &(other: CharSet): CharSet =
    if polarity && other.polarity then CharSet(true, chars & other.chars)
    else if !polarity && !other.polarity then CharSet(false, chars | other.chars)
    else
      val include = if polarity then chars else other.chars
      val exclude = if polarity then other.chars else chars
      CharSet(true, include -- exclude)

  def **(other: CharSet): Boolean = (this & other).isEmpty

  def subsetOf(other: CharSet): Boolean =
    if polarity && other.polarity then chars.subsetOf(other.chars)
    else if !polarity && !other.polarity then other.chars.subsetOf(chars)
    else if polarity then /* !other.polarity */ (chars & other.chars).isEmpty
    else /* !polarity && other.polarity */ false

  override def equals(obj: Any): Boolean = obj match
    case other: CharSet => polarity == other.polarity && chars == other.chars
    case _ => false

  def getRepresentative: Char =
    require(polarity && chars.nonEmpty)
    chars.head

  def prettyString: String =
    val raw =
      if isSingleton then chars.head.toString
      else
        val part1 = (chars & CharSet.ALPHA_NUM.chars).toList.sorted match
          case Nil => ""
          case c :: Nil => c.toString
          case cs =>
            val sb = StringBuilder()
            var i = 0
            while i < cs.length do
              var k = 1
              while i + k < cs.length && cs(i + k) == cs(i) + k do k += 1
              if k == 1 then sb += cs(i) else sb ++= (cs(i).toString + "-" + cs(i + k - 1).toString)
              i += k
            sb.toString
        val part2 = (chars -- CharSet.ALPHA_NUM.chars).mkString("")
        "[" + (if polarity then "" else "^") + part1 + part2 + "]"
    StringEscapeUtils.escapeJava(raw)

  override def toString: String =
    (if polarity then "" else "^") + StringEscapeUtils.escapeJava(chars.mkString(""))

object CharSet:
  val empty: CharSet = CharSet(true, Set.empty)
  val full: CharSet = CharSet(false, Set.empty)

  val NUM: CharSet = from('0' to '9')
  val ALPHA_LOWER: CharSet = from('a' to 'z')
  val ALPHA_UPPER: CharSet = from('A' to 'Z')
  val ALPHA: CharSet = from('a' to 'z', 'A' to 'Z')
  val ALPHA_NUM: CharSet = from('a' to 'z', 'A' to 'Z', '0' to '9')

  def of(chars: Char*): CharSet = CharSet(true, chars.toSet)

  def from(sets: (Char | collection.IterableOnce[Char])*): CharSet =
    CharSet(true, sets.map {
      case c: Char => Set(c)
      case iter: collection.IterableOnce[Char] => Set.from(iter)
    }.reduce(_ | _))

  def complementOf(chars: Char*): CharSet = CharSet(false, chars.toSet)

  def complementFrom(sets: (Char | collection.IterableOnce[Char])*): CharSet =
    CharSet(false, sets.map {
      case c: Char => Set(c)
      case iter: collection.IterableOnce[Char] => Set.from(iter)
    }.reduce(_ | _))
