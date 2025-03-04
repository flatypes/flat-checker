package flat.checker.abs

import flat.checker.Bound
import flat.checker.Bound.given

import scala.annotation.{tailrec, targetName}

object analysis {
  import ReLang.*

  def measureLength(regex: ReLang): Range = regex match {
    case ReEmpty => Range.fromInt(0)
    case ReChars(_) => Range.fromInt(1)
    case ReConcat(r1, r2) => measureLength(r1) + measureLength(r2)
    case ReUnion(r1, r2) => measureLength(r1) | measureLength(r2)
    case ReStar(_) => Range(0, Bound.PosInf)
  }

  def mustContain(regex: ReLang, c: Char): Boolean = regex match {
    case ReEmpty => false
    case ReChars(cs) => cs.isSingleton && cs.contains(c)
    case ReConcat(r1, r2) => mustContain(r1, c) || mustContain(r2, c)
    case ReUnion(r1, r2) => mustContain(r1, c) && mustContain(r2, c)
    case ReStar(r) => false
  }

  def mayContain(regex: ReLang, c: Char): Boolean = regex match {
    case ReEmpty => false
    case ReChars(cs) => cs.contains(c)
    case ReConcat(r1, r2) => mayContain(r1, c) || mayContain(r2, c)
    case ReUnion(r1, r2) => mayContain(r1, c) || mayContain(r2, c)
    case ReStar(r) => mayContain(r, c)
  }

  def mustStartWith(regex: ReLang, c: Char): Boolean =
    !regex.nullable && regex.first.isSingletonOf(c)

  def mayStartWith(regex: ReLang, c: Char): Boolean =
    regex.derivative(CharSet.of(c)).isDefined

  sealed trait Pos

  final case class AbsPos(k: Int) extends Pos

  final case class RelPos(cnf: List[ReLang], k: Int) extends Pos

  def evalPos(pos: Pos, cnf: List[ReLang]): Either[String, Int] = pos match {
    case RelPos(_, k) => Right(k)
    case AbsPos(k) =>
      var i = 0
      var acc = Range.fromInt(0)
      while i < cnf.length && !acc.contains(k) do {
        acc += measureLength(cnf(i))
        i += 1
      }

      if i == cnf.length then Right(cnf.length) // end position
      else if acc.isInt then Right(i)
      else Left("ambiguous position")
  }

  def rightShift(relPos: RelPos, k: Int): Either[String, RelPos] = {
    require(k >= 0)
    val RelPos(cnf, i) = relPos
    for j <- evalPos(AbsPos(k), cnf.drop(i)) yield RelPos(cnf, i + j)
  }

  private def equalChar(regex: ReLang, char: Char): Boolean = regex match {
    case ReChars(cs) if cs.isSingletonOf(char) => true
    case _ => false
  }

  @tailrec
  def find(cnf: List[ReLang], target: Char, i: Int): Either[String, RelPos] = {
    require(i >= 0)
    if i >= cnf.length then Right(RelPos(cnf, cnf.length)) // end position
    else if equalChar(cnf(i), target) then Right(RelPos(cnf, i))
    else if !mayContain(cnf(i), target) then find(cnf, target, i + 1)
    else Left("")
  }

  def find(cnf: List[ReLang], target: Char, from: Pos): Either[String, RelPos] = {
    for {
      i <- evalPos(from, cnf)
      pos <- find(cnf, target, i)
    } yield pos
  }

  def slice(cnf: List[ReLang], from: Pos, until: Pos): Either[String, ReLang] = {
    for {
      i <- evalPos(from, cnf)
      j <- evalPos(until, cnf)
    } yield ReLang.fromCNF(cnf.slice(i, j))
  }

  def slice(cnf: List[ReLang], from: Pos): Either[String, ReLang] = {
    for i <- evalPos(from, cnf) yield ReLang.fromCNF(cnf.drop(i))
  }

  def charAt(regex: ReLang, pos: Pos): Either[String, CharSet] = {
    pos match {
      case AbsPos(k) =>
        var i = 0
        var l: Option[ReLang] = Some(regex)
        while i <= k && l.isDefined do {
          l = l.get.derivative(CharSet.full)
          i += 1
        }
        l match {
          case Some(r) => Right(r.first)
          case None => Left("index out of bound")
        }
      case RelPos(cnf, i) =>
        assert(regex.toCNF == cnf)
        if i < cnf.length then Right(cnf(i).first)
        else Left("index out of bound")
    }
  }

  private def matchStarStartsWithSep(regex: ReLang, sep: Char): Boolean = regex match {
    case ReStar(r) =>
      val cnf = r.toCNF
      equalChar(cnf.head, sep) && cnf.tail.forall(!mayContain(_, sep))
    case _ => false
  }

  private def matchStarEndsWithSep(regex: ReLang, sep: Char): Boolean = regex match {
    case ReStar(r) =>
      val cnf = r.toCNF
      equalChar(cnf.last, sep) && cnf.dropRight(1).forall(!mayContain(_, sep))
    case _ => false
  }

  final class SplitResult(private val elems: List[(ReLang, 1 | Bound.PosInf.type)]) {
    require(elems.nonEmpty)

    @targetName("concat")
    def ++(other: SplitResult): SplitResult = SplitResult(elems ++ other.elems)

    def lengthRange: Range = {
      val lb = Bound.Fin(elems.count(_._2 == 1))
      val ub = if elems.exists(_._2 == Bound.PosInf) then Bound.PosInf else lb
      Range(lb, ub)
    }

    def get(k: Int): Option[ReLang] = {
      if k >= 0 then {
        if k < elems.length && elems.take(k + 1).forall(_._2 == 1) then Some(elems(k)._1) else None
      } else { // k < 0
        if -k <= elems.length && elems.takeRight(-k).forall(_._2 == 1) then Some(elems(elems.length + k)._1) else None
      }
    }

    def each: ReLang = {
      val r :: rs = elems.map(_._1): @unchecked
      if rs.forall(_ == r) then r else {
        val r1 = rs.find(_ != r).get
        throw UnsupportedOperationException(s"$r1 != $r")
      }
    }

    override def toString: String = elems.map {
      case (r, 1) => r.toString
      case (r, Bound.PosInf) => "rep(" + r.toString + ")"
    }.mkString(" ")
  }

  private def splitCNF(cnf: List[ReLang], sep: Char, cache: List[ReLang]): Either[String, SplitResult] = cnf match {
    case Nil =>
      val sr = SplitResult(List(ReLang.fromCNF(cache) -> 1))
      Right(sr)
    case r :: rs =>
      if equalChar(r, sep) then {
        val sr1 = SplitResult(List(ReLang.fromCNF(cache) -> 1))
        for sr <- splitCNF(rs, sep, Nil) yield sr1 ++ sr
      } else if matchStarStartsWithSep(r, sep) then {
        val ReStar(r1) = r: @unchecked
        val sr1 = SplitResult(List(ReLang.fromCNF(cache) -> 1, ReLang.fromCNF(r1.toCNF.tail) -> Bound.PosInf))
        if rs.isEmpty then Right(sr1)
        else if equalChar(rs.head, sep) then for sr <- splitCNF(rs.tail, sep, Nil) yield sr1 ++ sr
        else Left("expect a sep after the star")
      } else if matchStarEndsWithSep(r, sep) then {
        val ReStar(r1) = r: @unchecked
        val sr1 = SplitResult(List(ReLang.fromCNF(r1.toCNF.dropRight(1)) -> Bound.PosInf))
        if cache.isEmpty then for sr <- splitCNF(rs, sep, Nil) yield sr1 ++ sr
        else Left("expect a sep before the star")
      } else if !mayContain(r, sep) then {
        splitCNF(rs, sep, cache :+ r)
      } else Left("ambiguous position")
  }

  def split(regex: ReLang, sep: Char): Either[String, SplitResult] = splitCNF(regex.toCNF, sep, Nil)

  @tailrec
  def mustStartWith(regex: ReLang, prefix: String): Boolean = {
    if prefix.isEmpty then true
    else mustStartWith(regex, prefix.head) && mustStartWith(regex.derivative(CharSet.full).get, prefix.tail)
  }

  @tailrec
  def mayStartWith(regex: ReLang, prefix: String): Boolean = {
    if prefix.isEmpty then true
    else regex.derivative(CharSet.of(prefix.head)) match {
      case Some(r) => mayStartWith(r, prefix.tail)
      case None => false
    }
  }

  def onlyContain(regex: ReLang, allowed: CharSet): Boolean = regex match {
    case ReEmpty => true
    case ReChars(cs) => cs.subsetOf(allowed)
    case ReConcat(r1, r2) => onlyContain(r1, allowed) && onlyContain(r2, allowed)
    case ReUnion(r1, r2) => onlyContain(r1, allowed) && onlyContain(r2, allowed)
    case ReStar(r) => onlyContain(r, allowed)
  }

  def canCastToNat(regex: ReLang): Boolean = onlyContain(regex, CharSet.NUM) && !regex.nullable
}
