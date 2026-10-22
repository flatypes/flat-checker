package flat.checker.domain

import flat.checker.domain.REOps.*

type ListRE = RegEx[StrRE]

object ListREAPI:
  private val symbolSet =
    given_SymbolSet_RegEx[CharSet].asInstanceOf[SymbolSet[RegEx[CharSet]] {type Symbol = List[Char]}]

  // Length
  def filterLengthEq(n: Int): ListRE => ListRE = r => r.filterLengthEq(n)

  def filterLengthLe(n: Int): ListRE => ListRE = r => r.filterLengthLe(n)

  def filterLengthGe(n: Int): ListRE => ListRE = r => r.filterLengthGe(n)

  // Contains
  def filterContains(s: String): ListRE => ListRE =
    r => REOps.filterContains(r)(using symbolSet)(List(s.toList))

  def filterNotContain(s: String): ListRE => ListRE =
    r => REOps.filterNotContain(r)(using symbolSet)(List(s.toList))

  // IndexOf
  def filterIndexOfEq(s: String, start: Int, i: Int): ListRE => ListRE =
    r => REOps.filterIndexOfEq(r)(using symbolSet)(List(s.toList), start, i)

  // Higher-order
  def filterElemWith(at: Int, f: StrRE => StrRE): ListRE => ListRE = r => r.filterElemWith(at, f)

  def filterSliceWith(from: Int, f: ListRE => ListRE): ListRE => ListRE = r => r.filterSliceWith(from, f)

  def filterSliceWith(fromIndexOf: List[String], start: Int, offset: Int, f: ListRE => ListRE): ListRE => ListRE =
    r => REOps.filterSliceWith(r)(using symbolSet)(fromIndexOf.map(_.toList), start, offset, f)

  def filterSliceWith(fromLeft: Int, until: Int, f: ListRE => ListRE): ListRE => ListRE =
    r => r.filterSliceWith(fromLeft, until, f)

  def filterSliceWith(fromLeft: Int, untilIndexOf: List[String], start: Int, offset: Int,
                      f: ListRE => ListRE): ListRE => ListRE =
    r => REOps.filterSliceWith(r)(using symbolSet)(fromLeft, untilIndexOf.map(_.toList), start, offset, f)

  def filterSliceWith(fromIndexOf: List[String], start: Int, offset: Int, untilLast: Int,
                      f: ListRE => ListRE): ListRE => ListRE =
    r => REOps.filterSliceWith(r)(using symbolSet)(fromIndexOf.map(_.toList), start, offset, untilLast, f)

  def filterExists(f: StrRE => StrRE): ListRE => ListRE = r => r.filterExists(f)