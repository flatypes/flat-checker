package flat.checker

import flat.checker
import flat.checker.Bound.PosInf
import flat.checker.ReLang.*
import flat.checker.backend.{Index, Split}

import scala.annotation.tailrec

object CNFOps:
  enum BiIndex:
    case FromLeft(k: Int)
    case FromRight(k: Int)

    def toCNFIndex(cnf: List[ReLang]): Option[Int] =
      this match
        case FromLeft(k) => convertToRelative(cnf, k).toOption
        case FromRight(k) =>
          for i <- convertToRelative(cnf.reverse, k).toOption yield cnf.length - i

  /** Try to convert an absolute position into a relative position in `cnf`. */
  def convertToRelative(cnf: List[ReLang], k: Int): Either[String, Int] =
    var i = 0
    var acc = checker.Interval.fromInt(0)
    while i < cnf.length && !acc.contains(k) do {
      acc += cnf(i).length
      i += 1
    }
    if i == cnf.length then Right(cnf.length) // end position
    else if acc.isInt then Right(i)
    else Left("index position is ambiguous")

  /** Try to shift a relative position with an absolute `offset`. */
  def shiftIndex(index: Index, offset: Int): Either[String, Index] =
    require(offset >= 0)
    for delta <- convertToRelative(index.cnf.drop(index.pos), offset)
      yield Index(index.cnf, index.pos + delta)

  def charAt(cnf: List[ReLang], pos: Int): CharSet = cnf(pos).first

  def substring(cnf: List[ReLang], fromPos: Int, untilPos: Int): ReLang =
    ReLang.fromCNF(cnf.slice(fromPos, untilPos))

  def substring(cnf: List[ReLang], fromPos: Int): ReLang =
    ReLang.fromCNF(cnf.drop(fromPos))

  @tailrec
  def indexOf(cnf: List[ReLang], target: Char, fromPos: Int = 0): Either[String, Int] =
    require(fromPos >= 0)
    if fromPos >= cnf.length then Left("out of bounds")
    else if cnf(fromPos).isChar && cnf(fromPos).asChar == target then Right(fromPos)
    else if cnf(fromPos).contains(target) == Ternary.False then indexOf(cnf, target, fromPos + 1)
    else Left("position is ambiguous")

  def split(cnf: List[ReLang], sep: Char): Either[String, Split] =
    def matchStarStartsWithSep(regex: ReLang): Boolean = regex match {
      case ReStar(r) =>
        val cnf = r.toCNF
        cnf.head.isChar && cnf.head.asChar == sep && cnf.tail.forall(_.neverContain(sep))
      case _ => false
    }

    def matchStarEndsWithSep(regex: ReLang): Boolean = regex match {
      case ReStar(r) =>
        val cnf = r.toCNF
        cnf.last.isChar && cnf.last.asChar == sep && cnf.dropRight(1).forall(_.neverContain(sep))
      case _ => false
    }

    def iter(rest: List[ReLang], cache: List[ReLang]): Either[String, Split] = rest match
      case Nil =>
        val sr = Split(Seq(ReLang.fromCNF(cache) -> None))
        Right(sr)
      case r :: rs =>
        if r.isChar && r.asChar == sep then
          val sr1 = Split(Seq(ReLang.fromCNF(cache) -> None))
          for sr <- iter(rs, Nil) yield sr1 ++ sr
        else if matchStarStartsWithSep(r) then
          val ReStar(r1) = r: @unchecked
          val sr1 = Split(Seq(ReLang.fromCNF(cache) -> None, ReLang.fromCNF(r1.toCNF.tail) -> Some(PosInf)))
          if rs.isEmpty then Right(sr1)
          else if rs.head.isChar && rs.head.asChar == sep then for sr <- iter(rs.tail, Nil) yield sr1 ++ sr
          else Left("expect a sep after the star")
        else if matchStarEndsWithSep(r) then
          val ReStar(r1) = r: @unchecked
          val sr1 = Split(Seq(ReLang.fromCNF(r1.toCNF.dropRight(1)) -> Some(PosInf)))
          if cache.isEmpty then for sr <- iter(rs, Nil) yield sr1 ++ sr
          else Left("expect a sep before the star")
        else if r.neverContain(sep) then
          iter(rs, cache :+ r)
        else Left("ambiguous position")

    iter(cnf, Nil)