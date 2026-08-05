package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.RegEx.*

import scala.collection.mutable.ListBuffer

object StrREOps extends LazyLogging:
  extension (r: StrRE)
    def filterStartWith(s: String): StrRE = REOps.filterStartsWith(r)(s.toList)

    def absStartsWithStr(s: String): BoolSet = REOps.absStartsWith(r)(s.toList)

    def absEndsWithStr(s: String): BoolSet = REOps.absEndsWith(r)(s.toList)

    def filterContainsStr(s: String): StrRE = REOps.filterContains(r)(s.toList)

    def filterNotContainStr(s: String): StrRE = REOps.filterNotContain(r)(s.toList)

    def absContainsStr(s: String): BoolSet = REOps.absContains(r)(s.toList)

    def absIndexOfStr(s: String): IndexSet = REOps.absIndexOf(r)(s.toList)

    def absReplace(s1: String, s2: String): StrRE = s1.length match
      case 0 => throw IllegalArgumentException("Replacement string cannot be empty")
      case 1 => r.absReplace(s1.head, s2)
      case _ => throw UnsupportedOperationException("Replacement string is not a single character")

    def absReplace(c: Char, s: String): StrRE = r match
      case Zero() | One() => r
      case Lit(a) => if a.contains(c) then word(s.toList) + symbolSet(a - c) else r
      case Plus(r1, r2) => r1.absReplace(c, s) + r2.absReplace(c, s)
      case Comp(r1, r2) => r1.absReplace(c, s) * r2.absReplace(c, s)
      case Star(r1) => r1.absReplace(c, s).star

    def absCountStr(s: String): CountingRE = REOps.absCount(r)(s.toList)

    def absSplitStr(s: String): RegEx[StrRE] = REOps.absSplit(r)(s.toList)

    def absTrimLeft: StrRE = r match
      case Zero() => Zero()
      case One() => One()
      case Lit(a) =>
        val b = a - ' ' - '\n' - '\r' - '\t'
        if b.isEmpty then One() else Lit(b)
      case Plus(r1, r2) => r1.absTrimLeft + r2.absTrimLeft
      case Comp(r1, r2) =>
        val trimmed1 = r1.absTrimLeft
        if trimmed1 == One() then r2.absTrimLeft else trimmed1 * r2
      case Star(r1) =>
        val trimmed = r1.absTrimLeft
        if trimmed == One() then One() else trimmed.star

    def absTrimRight: StrRE = r.reverse.absTrimLeft.reverse

    def absTrim: StrRE = r.absTrimLeft.absTrimRight

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

    def format(n: Int): String =
      var s = Integer.toString(n, radix)
      if conv == 'x' then
        s = s.toLowerCase
      else if conv == 'X' then
        s = s.toUpperCase
      if width <= s.length then s
      else (if zeroPadded then "0" else " ") * (width - s.length) + s

  def absFromInt(min: Int, max: Int, fmt: NumStrFormat): StrRE =
    NumConverter(fmt).convert(min, max)

  private class NumConverter(fmt: NumStrFormat):
    def convert(min: Int, max: Int): StrRE =
      val ranges = rangesBetween(min, max)
      sum(ranges.map(convert))

    def rangesBetween(min: Int, max: Int): List[Range.Inclusive] =
      val leftRanges = ListBuffer.empty[Range.Inclusive]
      var n = min
      while n < max do
        val range = rangeFrom(n)
        leftRanges += range
        n = range.end + 1

      val rightRanges = ListBuffer.empty[Range.Inclusive]
      n = max
      while min < n do
        val range = rangeTo(n)
        rightRanges += range
        n = range.start - 1

      val merged = ListBuffer.from(leftRanges.init)
      if rightRanges.last.start <= leftRanges.last.start && rightRanges.last.end <= leftRanges.last.end &&
        leftRanges.last.start < rightRanges.last.end then
        merged += (leftRanges.last.start to rightRanges.last.end)
      merged ++= rightRanges.init.reverse
      merged.toList

    private def rangeFrom(min: Int): Range.Inclusive =
      val s = fmt.format(min)
      val i = s.reverse.indexWhere(_ != '0')
      val d = fmt.maxDigit.toString
      val s1 = if i == -1 then d * s.length else s.dropRight(i + 1) + d * (i + 1)
      min to Integer.parseInt(s1, fmt.radix)

    private def rangeTo(max: Int): Range.Inclusive =
      val s = fmt.format(max)
      val i = s.reverse.indexWhere(_ != fmt.maxDigit)
      val s1 = if i == -1 then "0" * s.length else s.dropRight(i + 1) + "0" * (i + 1)
      Integer.parseInt(s1, fmt.radix) to max

    private def convert(r: Range.Inclusive): StrRE =
      val s1 = fmt.format(r.start)
      val s2 = fmt.format(r.end)
      assert(s1.length == s2.length, s"Length mismatch: $s1 and $s2")
      val parts = for i <- s1.indices yield
        if s1(i) == s2(i) then symbol(s1(i))
        else Lit(CharSet.from(fmt.digits.slice(fmt.digits.indexOf(s1(i)), fmt.digits.indexOf(s2(i)) + 1)))
      RegEx.product(parts.toList)
