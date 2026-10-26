package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.REOps.*

enum AIndex:
  /** Constant index. If positive, left to right. Otherwise, right to left. */
  case Const(i: Int)
  /** Index of the first occurrence of the element/slice. */
  case Of(s: String, start: Int, offset: Int)
  case End

final case class AIndexRange(from: AIndex, until: AIndex)

object StrREAPI extends LazyLogging:
  // Operations
  def charAt(r: StrRE, at: AIndex | AIndexRange): CharSet = at match
    case AIndex.Const(i) => if i >= 0 then r.absAt(i) else r.absAtRight(-i)
    case AIndex.Of(s, _, _) => CharSet(s.head)
    case AIndex.End => throw IllegalArgumentException("Cannot get element at end index")
    case AIndexRange(from, until) => substr(r, from, until).alphabet

  def substr(r: StrRE, from: AIndex, until: AIndex): StrRE =
    (from, until) match
      case (AIndex.Const(i), AIndex.End) => r.absSlice(i)
      case (AIndex.Of(s, i, k), AIndex.End) => r.absSlice(s.toList, i, k)
      case (AIndex.Const(i), AIndex.Const(j)) if i >= 0 => r.absSlice(i, j)
      case (AIndex.Const(i), AIndex.Of(s, j, k)) if i >= 0 => r.absSlice(i, s.toList, j, k)
      case (AIndex.Of(s, i, k), AIndex.Const(j)) if j < 0 => r.absSlice(s.toList, i, k, j)
      case _ => throw new IllegalArgumentException(s"slice $r from $from until $until")

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

  def filterIndexOfNeLast(t: String, start: Int): StrRE => StrRE =
    r => r.filterIndexOfRightGe(t.toList, start, 2)

  // Higher-order filters
  def filterForall(f: CharSet => CharSet): StrRE => StrRE = r => r.filterForall(f)

  def filterExists(f: CharSet => CharSet): StrRE => StrRE = r => r.filterExists(f)

  def filterCharWith(at: AIndex | AIndexRange, f: CharSet => CharSet): StrRE => StrRE = at match
    case AIndex.Const(i) => r => r.filterElemWith(i, f)
    case AIndex.End => throw new IllegalArgumentException("AIndex.End out of bound")
    case AIndex.Of(s, i, k) => r => r.filterElemWith(s.toList, i, k, f)
    case AIndexRange(from, until) =>
      tryFilterSubstrWith(from, until, filterExists(f)).getOrElse:
        logger.warn("fallback [{}, {}) to [0, -1)", from, until)
        filterExists(f)

  def filterSubstrWith(from: AIndex, until: AIndex, f: StrRE => StrRE): StrRE => StrRE =
    tryFilterSubstrWith(from, until, f).getOrElse:
      logger.warn("give up filtering [{}, {})", from, until)
      identity

  private def tryFilterSubstrWith(from: AIndex, until: AIndex, f: StrRE => StrRE): Option[StrRE => StrRE] =
    (from, until) match
      case (AIndex.Const(i), AIndex.End) => Some(r => r.filterSliceWith(i, f))
      case (AIndex.Of(s, i, k), AIndex.End) => Some(r => r.filterSliceWith(s.toList, i, k, f))
      case (AIndex.Const(i), AIndex.Const(j)) if i >= 0 => Some(r => r.filterSliceWith(i, j, f))
      case (AIndex.Const(i), AIndex.Of(s, j, k)) if i >= 0 => Some(r => r.filterSliceWith(i, s.toList, j, k, f))
      case (AIndex.Of(s, i, k), AIndex.Const(j)) if j < 0 => Some(r => r.filterSliceWith(s.toList, i, k, j, f))
      case _ => None
