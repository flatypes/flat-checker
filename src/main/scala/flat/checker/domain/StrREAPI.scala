package flat.checker.domain

import flat.checker.domain.REOps.*

object StrREAPI:
  // Equality
  def filterEq(s: String): StrRE => StrRE = r => r.filterEq(s.toList)

  def filterNe(s: String): StrRE => StrRE = r => r.filterNe(s.toList)

  // Length
  def filterLengthEq(n: Int): StrRE => StrRE = r => r.filterLengthEq(n)

  def filterLengthNe(n: Int): StrRE => StrRE = r => r.filterLengthLe(n - 1) + r.filterLengthGe(n + 1)

  def filterLengthLe(n: Int): StrRE => StrRE = r => r.filterLengthLe(n)

  def filterLengthGe(n: Int): StrRE => StrRE = r => r.filterLengthGe(n)

  // Contains
  def filterContains(c: Char): StrRE => StrRE = r => r.filterContains(List(c))

  def filterContains(s: String): StrRE => StrRE = r => r.filterContains(s.toList)

  def filterNotContain(c: Char): StrRE => StrRE = r => r.filterNotContain(List(c))

  def filterNotContain(s: String): StrRE => StrRE = r => r.filterNotContain(s.toList)

  // Prefix & Suffix
  def filterStartsWith(s: String): StrRE => StrRE = r => r.filterStartsWith(s.toList)

  def filterNotStartWith(s: String): StrRE => StrRE = r => r.filterNotStartWith(s.toList)

  def filterEndsWith(s: String): StrRE => StrRE = r => r.filterEndsWith(s.toList)

  def filterNotEndWith(s: String): StrRE => StrRE = r => r.filterNotEndWith(s.toList)

  // IndexOf
  def filterIndexOfNeg(s: String, start: Int): StrRE => StrRE = r => r.filterIndexOfNeg(s.toList, start)

  def filterIndexOfEq(t: String, start: Int, i: Int): StrRE => StrRE = r => r.filterIndexOfEq(t.toList, start, i)

  def filterIndexOfLe(t: String, start: Int, i: Int): StrRE => StrRE = r => r.filterIndexOfLe(t.toList, start, i)

  def filterIndexOfGe(t: String, start: Int, i: Int): StrRE => StrRE = r => r.filterIndexOfGe(t.toList, start, i)

  // Higher-order filters
  def filterElemWith(i: Int, f: CharSet => CharSet): StrRE => StrRE = r => r.filterElemWith(i, f)

  def filterSliceWith(from: Int, f: StrRE => StrRE): StrRE => StrRE = r => r.filterSliceWith(from, f)

  def filterSliceWith(fromIndexOf: List[Char], start: Int, offset: Int, f: StrRE => StrRE): StrRE => StrRE =
    r => r.filterSliceWith(fromIndexOf, start, offset, f)

  def filterSliceWith(fromLeft: Int, until: Int, f: StrRE => StrRE): StrRE => StrRE =
    r => r.filterSliceWith(fromLeft, until, f)

  def filterSliceWith(fromLeft: Int, untilIndexOf: List[Char], start: Int, offset: Int,
                      f: StrRE => StrRE): StrRE => StrRE =
    r => r.filterSliceWith(fromLeft, untilIndexOf, start, offset, f)

  def filterSliceWith(fromIndexOf: List[Char], start: Int, offset: Int, untilLast: Int,
                      f: StrRE => StrRE): StrRE => StrRE =
    r => r.filterSliceWith(fromIndexOf, start, offset, untilLast, f)

  def filterForall(f: CharSet => CharSet): StrRE => StrRE = r => r.filterForall(f)

  def filterExists(f: CharSet => CharSet): StrRE => StrRE = r => r.filterExists(f)