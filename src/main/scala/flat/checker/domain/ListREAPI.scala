package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.REOps.*

@deprecated
type ListRE = RegEx[StrRE]

@deprecated
object ListREAPI extends LazyLogging:
  private val symbolSet =
    given_SymbolSet_RegEx[CharSet].asInstanceOf[SymbolSet[RegEx[CharSet]] {type Symbol = List[Char]}]

  // Operations
  def elemAt(r: ListRE, at: AIndex | AIndexRange): StrRE = at match
    case AIndex.Const(i) => if i >= 0 then r.absAt(i) else r.absAtRight(-i)
    case AIndex.End => throw IllegalArgumentException("Cannot get element at end index")
    case AIndex.Of(s, _, _) => RegEx.word(s.toList)
    case AIndexRange(from, until) => slice(r, from, until).alphabet

  def slice(r: ListRE, from: AIndex, until: AIndex): ListRE =
    (from, until) match
      case (AIndex.Const(i), AIndex.End) => r.absSlice(i)
      case (AIndex.Of(s, i, k), AIndex.End) =>
        REOps.absSlice(r)(using symbolSet)(List(s.toList), i, k)
      case (AIndex.Const(i), AIndex.Const(j)) if i >= 0 => r.absSlice(i, j)
      case (AIndex.Const(i), AIndex.Of(s, j, k)) if i >= 0 =>
        REOps.absSlice(r)(using symbolSet)(i, List(s.toList), j, k)
      case (AIndex.Of(s, i, k), AIndex.Const(j)) if j < 0 =>
        REOps.absSlice(r)(using symbolSet)(List(s.toList), i, k, j)
      case _ => throw new IllegalArgumentException(s"slice $r from $from until $until")

  // Filters
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
  def filterForall(f: StrRE => StrRE): ListRE => ListRE = r => r.filterForall(f)

  def filterExists(f: StrRE => StrRE): ListRE => ListRE = r => r.filterExists(f)

  def filterElemWith(at: AIndex | AIndexRange, f: StrRE => StrRE): ListRE => ListRE = at match
    case AIndex.Const(i) => r => r.filterElemWith(i, f)
    case AIndex.End => throw IllegalArgumentException("AIndex.End out of bound")
    case _: AIndex.Of => throw new IllegalArgumentException("AIndex.Of does not make sense here")
    case AIndexRange(from, until) =>
      tryFilterSliceWith(from, until, filterExists(f)).getOrElse:
        logger.warn("fallback [{}, {}) to [0, -1)", from, until)
        filterExists(f)

  def filterSliceWith(from: AIndex, until: AIndex, f: ListRE => ListRE): ListRE => ListRE =
    tryFilterSliceWith(from, until, f).getOrElse:
      logger.warn("fallback [{}, {}) to [0, -1)", from, until)
      identity

  private def tryFilterSliceWith(from: AIndex, until: AIndex, f: ListRE => ListRE): Option[ListRE => ListRE] =
    (from, until) match
      case (AIndex.Const(i), AIndex.End) => Some(r => r.filterSliceWith(i, f))
      case (AIndex.Of(s, i, k), AIndex.End) =>
        Some(r => REOps.filterSliceWith(r)(using symbolSet)(List(s.toList), i, k, f))
      case (AIndex.Const(i), AIndex.Const(j)) if i >= 0 => Some(r => r.filterSliceWith(i, j, f))
      case (AIndex.Const(i), AIndex.Of(s, j, k)) if i >= 0 =>
        Some(r => REOps.filterSliceWith(r)(using symbolSet)(i, List(s.toList), j, k, f))
      case (AIndex.Of(s, i, k), AIndex.Const(j)) if j < 0 =>
        Some(r => REOps.filterSliceWith(r)(using symbolSet)(List(s.toList), i, k, j, f))
      case _ => None
