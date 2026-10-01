package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
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

    def forallIndexOfLt(c1: Char, c2: Char): Boolean =
      val r1 = REOps.takeIndexOf(r)(List(c1))
      logger.debug("forallIndexOfLt: r1 = {}", r1.pp)
      val r2 = REOps.filterContains(r1)(List(c2))
      logger.debug("forallIndexOfLt: r2 = {}", r2.pp)
      r2.isEmpty

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

    def absTrimLeft(chars: String = " \n\r\t"): StrRE = r match
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

    def absTrimRight(chars: String = " \n\r\t"): StrRE = r.reverse.absTrimLeft(chars).reverse

    def absTrim(chars: String = " \n\r\t"): StrRE = r.absTrimLeft(chars).absTrimRight(chars)

    def absIsAscii: BoolSet =
      if r.alphabet.subsetOf(CharSet.from(0.toChar to 0x7F.toChar)) then BoolSet.True
      else BoolSet.Full

    def absToInt(radix: Int = 10): NatRange =
      val r1 = r.absTrimLeft("0")
      val s1 = r1.shortestMin
      val n1 = if s1.isEmpty then 0 else Integer.parseInt(s1, radix)
      val s2 = r1.longestMax
      val n2 =
        if s2.contains("∞") then None
        else if s2.isEmpty then Some(0)
        else Some(Integer.parseInt(s2, radix))
      NatRange(n1, n2)

    private def shortestMin: String = r match
      case Zero() | One() | Star(_) => ""
      case Lit(CharSet(true, cs)) => cs.map(_.toUpper).min.toString
      case Lit(CharSet(false, cs)) => throw IllegalArgumentException("not valid digit character set")
      case Plus(r1, r2) =>
        val s1 = r1.shortestMin
        val s2 = r2.shortestMin
        if s1.length < s2.length then s1
        else if s2.length < s1.length then s2
        else List(s1, s2).min
      case Comp(r1, r2) => r1.shortestMin + r2.shortestMin

    private def longestMax: String = r match
      case Zero() | One() => ""
      case Lit(CharSet(true, cs)) => cs.map(_.toUpper).max.toString
      case Lit(CharSet(false, cs)) => throw IllegalArgumentException("not valid digit character set")
      case Plus(r1, r2) =>
        val s1 = r1.longestMax
        val s2 = r2.longestMax
        if s1.contains("∞") || s2.contains("∞") then "∞"
        else if s1.length > s2.length then s1
        else if s2.length > s1.length then s2
        else List(s1, s2).max
      case Comp(r1, r2) => r1.longestMax + r2.longestMax
      case Star(_) => "∞"

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
      case _ => throw UnsupportedOperationException("Cannot convert infinite StrRE to finite set")

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
