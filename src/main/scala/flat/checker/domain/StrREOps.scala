package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.REAPI.alphabet
import flat.checker.domain.RegExp.*

import scala.collection.mutable.ListBuffer

object StrREOps extends LazyLogging:
  extension (r: CharRE)
    def absReplace(s1: String, s2: String): CharRE = s1.length match
      case 0 => throw IllegalArgumentException("Replacement string cannot be empty")
      case 1 => r.absReplace(s1.head, s2)
      case _ => throw UnsupportedOperationException("Replacement string is not a single character")

    def absReplace(c: Char, s: String): CharRE = r match
      case Zero() | One() => r
      case Lit(a) => if a.contains(c) then fromString(s.toList) + fromCharSet(a - c) else r
      case Plus(r1, r2) => r1.absReplace(c, s) + r2.absReplace(c, s)
      case Comp(r1, r2) => r1.absReplace(c, s) * r2.absReplace(c, s)
      case Star(r1) => r1.absReplace(c, s).star

    def absTrimLeft(chars: String = " \n\r\t"): CharRE = r match
      case Zero() => Zero()
      case One() => One()
      case Lit(a) =>
        val b = a -- chars
        if b.isEmpty then One() else Lit(b)
      case Plus(r1, r2) => r1.absTrimLeft(chars) + r2.absTrimLeft(chars)
      case Comp(r1, r2) =>
        val trimmed1 = r1.absTrimLeft(chars)
        if trimmed1 == One() then r2.absTrimLeft(chars) else trimmed1 * r2
      case Star(r1) =>
        val trimmed = r1.absTrimLeft(chars)
        if trimmed == One() then One() else trimmed.star

    def absTrimRight(chars: String = " \n\r\t"): CharRE = r.reverse.absTrimLeft(chars).reverse

    def absTrim(chars: String = " \n\r\t"): CharRE = r.absTrimLeft(chars).absTrimRight(chars)

    def absIsAscii: BoolSet =
      if r.alphabet.subsetOf(CharSet.from(0.toChar to 0x7F.toChar)) then BoolSet.True
      else BoolSet.Full

    def absToInt(radix: Int = 10): NatRange =
      NatRange(Integer.parseInt(r.minInt, radix), r.maxInt.map(Integer.parseInt(_, radix)))

    private def minInt: String = r match
      case Zero() | One() | Star(_) => ""
      case Lit(CharSet(true, cs)) => cs.map(_.toUpper).min.toString
      case Lit(CharSet(false, cs)) => throw IllegalArgumentException("not valid digit character set")
      case Plus(r1, r2) => List(r1.minInt, r2.minInt).min(using IntStringOrdering)
      case Comp(r1, r2) => r1.minInt + r2.minInt

    private def maxInt: Option[String] = r match
      case Zero() | One() => Some("")
      case Lit(CharSet(true, cs)) => Some(cs.map(_.toUpper).max.toString)
      case Lit(CharSet(false, cs)) => throw IllegalArgumentException("not valid digit character set")
      case Plus(r1, r2) =>
        (r1.maxInt, r2.maxInt) match
          case (Some(s1), Some(s2)) => Some(List(s1, s2).max(using IntStringOrdering))
          case _ => None
      case Comp(r1, r2) =>
        (r1.maxInt, r2.maxInt) match
          case (Some(s1), Some(s2)) => Some(s1 + s2)
          case _ => None
      case Star(_) => None

    def isFiniteLang: Boolean = r match
      case Zero() | One() | Lit(CharSet(true, _)) => true
      case Plus(r1, r2) => r1.isFiniteLang && r2.isFiniteLang
      case Comp(r1, r2) => r1.isFiniteLang && r2.isFiniteLang
      case _ => false

    def getLang: Set[String] = r match
      case Zero() => Set.empty
      case One() => Set("")
      case Lit(CharSet(true, cs)) => cs.map(_.toString)
      case Plus(r1, r2) => r1.getLang ++ r2.getLang
      case Comp(r1, r2) => for s1 <- r1.getLang; s2 <- r2.getLang yield s1 + s2
      case _ => throw UnsupportedOperationException("Cannot convert infinite CharRE to finite set")

    def filterForallCharIn(a: CharSet): CharRE = r match
      case Zero() | One() => r
      case Lit(b) => fromCharSet(b & a)
      case Plus(r1, r2) => r1.filterForallCharIn(a) + r2.filterForallCharIn(a)
      case Comp(r1, r2) => r1.filterForallCharIn(a) * r2.filterForallCharIn(a)
      case Star(r1) => r1.filterForallCharIn(a).star

    def filterExistsCharIn(a: CharSet): CharRE = r match
      case Zero() | One() => Zero()
      case Lit(b) => if (b & a).isEmpty then Zero() else r
      case Plus(r1, r2) => r1.filterExistsCharIn(a) + r2.filterExistsCharIn(a)
      case Comp(r1, r2) => r1.filterExistsCharIn(a) * r2 + r1 * r2.filterExistsCharIn(a)
      case Star(r1) => r * r1.filterExistsCharIn(a) * r

  private object IntStringOrdering extends Ordering[String]:
    def compare(s1: String, s2: String): Int =
      if s1.length != s2.length then s1.length - s2.length
      else s1.compareTo(s2)

  final case class NumStrFormat(zeroPadded: Boolean = false,
                                width: Int = 0,
                                conv: Char = 'd'):
    val radix: Int = conv match
      case 'd' => 10
      case 'o' => 8
      case 'x' | 'X' => 16
      case _ => throw IllegalArgumentException(s"Invalid conversion: $conv")

    val maxDigit: Char = conv match
      case 'd' => '9'
      case 'o' => '7'
      case 'x' => 'f'
      case 'X' => 'F'
      case _ => throw IllegalArgumentException(s"Invalid conversion: $conv")

    val digits: String = conv match
      case 'd' => "0123456789"
      case 'o' => "01234567"
      case 'x' => "0123456789abcdef"
      case 'X' => "0123456789ABCDEF"
      case _ => throw IllegalArgumentException(s"Invalid conversion: $conv")

    def format(n: BigInt): String =
      var s = n.toString(radix)
      if conv == 'x' then
        s = s.toLowerCase
      else if conv == 'X' then
        s = s.toUpperCase
      if width <= s.length then s
      else (if zeroPadded then "0" else " ") * (width - s.length) + s

    override def toString: String =
      val pad = if zeroPadded then "0" else ""
      val w = if width > 0 then width.toString else ""
      s"%$pad$w$conv"

  def absFromInt(min: BigInt, max: BigInt, fmt: NumStrFormat): CharRE =
    NumConverter(fmt).convert(min, max)

  private class NumConverter(fmt: NumStrFormat):
    def convert(min: BigInt, max: BigInt): CharRE =
      val ranges = rangesBetween(min, max)
      sum(ranges.map(convert))

    def rangesBetween(min: BigInt, max: BigInt): List[Interval] =
      if min == max then
        return List(Interval(min, max))

      val leftRanges = ListBuffer.empty[Interval]
      var n = min
      while n < max do
        val range = rangeFrom(n)
        leftRanges += range
        n = range.max + 1

      val rightRanges = ListBuffer.empty[Interval]
      n = max
      while min < n do
        val range = rangeTo(n)
        rightRanges += range
        n = range.min - 1

      val merged = ListBuffer.from(leftRanges.init)
      if rightRanges.last.min <= leftRanges.last.min && rightRanges.last.max <= leftRanges.last.max &&
        leftRanges.last.min < rightRanges.last.max then
        merged += Interval(leftRanges.last.min, rightRanges.last.max)
      merged ++= rightRanges.init.reverse
      merged.toList

    private def rangeFrom(min: BigInt): Interval =
      val s = fmt.format(min)
      val i = s.reverse.indexWhere(_ != '0')
      val d = fmt.maxDigit.toString
      val s1 = if i == -1 then d * s.length else s.dropRight(i + 1) + d * (i + 1)
      Interval(min, BigInt(s1, fmt.radix))

    private def rangeTo(max: BigInt): Interval =
      val s = fmt.format(max)
      val i = s.reverse.indexWhere(_ != fmt.maxDigit)
      val s1 = if i == -1 then "0" * s.length else s.dropRight(i + 1) + "0" * (i + 1)
      Interval(BigInt(s1, fmt.radix), max)

    private def convert(r: Interval): CharRE =
      val s1 = fmt.format(r.min)
      val s2 = fmt.format(r.max)
      assert(s1.length == s2.length, s"Length mismatch: $s1 and $s2")
      val parts = for i <- s1.indices yield
        if s1(i) == s2(i) then fromChar(s1(i))
        else Lit(CharSet.from(fmt.digits.slice(fmt.digits.indexOf(s1(i)), fmt.digits.indexOf(s2(i)) + 1)))
      product(parts.toList)
