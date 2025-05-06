package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.util.{cartesianPower, cartesianProduct}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.pow

object REOps extends LazyLogging:

  import RegExpr.*

  def length(r: RegExpr): NatRange = r match
    case RENone => throw IllegalArgumentException("∅")
    case RENull => NatRange.at(0)
    case REChar(_) => NatRange.at(1)
    case REConcat(r1, r2) => length(r1) + length(r2)
    case REUnion(r1, r2) => length(r1) | length(r2)
    case RELoop(range, r) => length(r) * range

  def alphabet(re: RegExpr): CharSet = re match
    case RENone => throw IllegalArgumentException("∅")
    case RENull => CharSet.empty
    case REChar(cs) => cs
    case REConcat(r1, r2) => alphabet(r1) | alphabet(r2)
    case REUnion(r1, r2) => alphabet(r1) | alphabet(r2)
    case RELoop(_, r) => alphabet(r)

  def contains(re: RegExpr, c: Char): Option[Boolean] = re match
    case RENone | RENull => Some(false)
    case REChar(cs) =>
      if cs.isSingleton && cs.contains(c) then Some(true)
      else if !cs.contains(c) then Some(false)
      else None
    case REConcat(r1, r2) =>
      val b1 = contains(r1, c)
      val b2 = contains(r2, c)
      if b1.contains(true) || b2.contains(true) then Some(true)
      else if b1.contains(false) && b2.contains(false) then Some(false)
      else None
    case REUnion(r1, r2) =>
      val b1 = contains(r1, c)
      val b2 = contains(r2, c)
      if b1.contains(true) && b2.contains(true) then Some(true)
      else if b1.contains(false) && b2.contains(false) then Some(false)
      else None
    case RELoop(q, r) =>
      contains(r, c) match
        case Some(true) => if q.lower > 0 then Some(true) else None
        case other => other

  def merge(cases: List[RegExpr]): RegExpr =
    val rs = cases.filterNot(_ == RENone).distinct.map(_.toCNF)
    if rs.isEmpty then return RENone
    val buffers = rs.map(ListBuffer.from)
    val preBuffer = ListBuffer.empty[RegExpr]
    while buffers.forall(_.nonEmpty) && buffers.map(_.head).distinct.length == 1 do
      preBuffer += buffers.head.head
      buffers.foreach(_.dropInPlace(1))
    val sufBuffer = ListBuffer.empty[RegExpr]
    while buffers.forall(_.nonEmpty) && buffers.map(_.last).distinct.length == 1 do
      sufBuffer += buffers.head.last
      buffers.foreach(_.dropRightInPlace(1))
    val infix = mkUnion(buffers.map(buf => mkConcat(buf.toList)))
    mkConcat(mkConcat(preBuffer.toList), infix, mkConcat(sufBuffer.toList))

  def cutAt(re: RegExpr, k: Int): List[(RegExpr, CharSet, RegExpr)] =
    require(k >= 0)
    if k == 0 then
      for (cs, r) <- cutFirstChar(re) yield (RENull, cs, r)
    else
      for
        (r1, cs1, r) <- cutAt(re, k - 1)
        (cs2, r2) <- cutFirstChar(r)
      yield (mkConcat(r1, REChar(cs1)), cs2, r2)

  private def cutFirstChar(re: RegExpr): List[(CharSet, RegExpr)] = re match
    case RENone => throw IllegalArgumentException("∅")
    case RENull => Nil
    case REChar(cs) => List((cs, RENull))
    case REConcat(r1, r2) =>
      val cuts1 = for (cs, r11) <- cutFirstChar(r1) yield (cs, mkConcat(r11, r2))
      val cuts2 = if r1.nullable then cutFirstChar(r2) else Nil
      cuts1 ++ cuts2
    case REUnion(r1, r2) =>
      val cuts1 = cutFirstChar(r1)
      val cuts2 = cutFirstChar(r2)
      cuts1 ++ cuts2
    case RELoop(range, r1) =>
      val rLoop = mkLoop(range - 1, r1)
      for (cs, r11) <- cutFirstChar(r1) yield (cs, mkConcat(r11, rLoop))

  def firstSet(re: RegExpr): CharSet = cutFirstChar(re).map(_._1).reduce(_ | _)

  def charAt(re: RegExpr, k: Int): CharSet =
    val css = for (_, cs, _) <- cutAt(re, k) yield cs
    css.foldLeft(CharSet.empty)(_ | _)

  def splitAt(r: RegExpr, k: Int): (RegExpr, RegExpr) =
    require(k >= 0)
    if k == 0 then (RENull, r)
    else
      val cuts = for (r1, cs, r2) <- cutAt(r, k - 1) yield (mkConcat(r1, REChar(cs)), r2)
      val (r1s, r2s) = cuts.unzip
      (if r1s.isEmpty then r else merge(r1s), if r2s.isEmpty then RENull else merge(r2s))

  def take(re: RegExpr, k: Int): RegExpr = splitAt(re, k)._1

  def drop(re: RegExpr, k: Int): RegExpr = splitAt(re, k)._2

  def takeRight(re: RegExpr, k: Int): RegExpr = splitAt(re.reverse, k)._1.reverse

  def dropRight(re: RegExpr, k: Int): RegExpr = splitAt(re.reverse, k)._2.reverse

  def shift(rl: RegExpr, rr: RegExpr, offset: Int): (RegExpr, RegExpr) =
    if offset == 0 then (rl, rr)
    else if offset > 0 then
      val (rer1, rer2) = splitAt(rr, offset)
      (mkConcat(rl, rer1), rer2)
    else
      val (r1, r2) = splitAt(rl.reverse, -offset)
      val (rel1, rel2) = (r2.reverse, r1.reverse)
      (rel1, mkConcat(rel2, rr))

  def cutBy(r: RegExpr, c: Char): (List[(RegExpr, RegExpr)], RegExpr) = r match
    case RENone => throw IllegalArgumentException("∅")
    case RENull => (Nil, RENull)
    case REChar(cs) =>
      if cs.contains(c) then
        (List((RENull, RENull)), if cs.isSingleton then RENone else REChar(cs - c))
      else (Nil, r)
    case REConcat(r1, r2) =>
      val cuts = ListBuffer.empty[(RegExpr, RegExpr)]
      val (cuts1, r1Not) = cutBy(r1, c)
      for (r1l, r1r) <- cuts1 do
        cuts += ((r1l, mkConcat(r1r, r2)))
      if r1Not == RENone then
        (cuts.toList, RENone)
      else
        val (cuts2, r2Not) = cutBy(r2, c)
        for (r2l, r2r) <- cuts2 do
          cuts += ((mkConcat(r1Not, r2l), r2r))
        (cuts.toList, mkConcat(r1Not, r2Not))
    case REUnion(r1, r2) =>
      val (cuts1, r1Not) = cutBy(r1, c)
      val (cuts2, r2Not) = cutBy(r2, c)
      (cuts1 ++ cuts2, mkUnion(r1Not, r2Not))
    case RELoop(range, r1) =>
      val (cuts1, r1Not) = cutBy(r1, c)
      if cuts1.isEmpty then // not found
        (Nil, r)
      else if r1Not == RENone then // found at first iteration
        val rLoop = mkLoop(range - 1, r1)
        val cuts = for (r1l, r1r) <- cuts1 yield (r1l, mkConcat(r1r, rLoop))
        (cuts, if range.lower == 0 then RENull else RENone)
      else // may be found at any iteration
        val cuts = ListBuffer.empty[(RegExpr, RegExpr)]
        for
          k <- 0 until range.upper.getOrElse(range.lower)
          rLoop = mkLoop(range - (k + 1), r1)
          (r1l, r1r) <- cuts1
        do cuts += ((mkConcat(r1Not ^ k, r1l), mkConcat(r1r, rLoop)))
        if range.upper.isEmpty then
          for (r1l, r1r) <- cuts1 do
            cuts += ((mkConcat(RELoop(range, r1Not), r1l), mkConcat(r1r, r1.*)))
        val rNot = mkUnion(if range.lower == 0 then RENull else RENone, RELoop(range, r1Not))
        (cuts.toList, rNot)

  def splitAtIndexOf(r: RegExpr, c: Char): ((RegExpr, RegExpr), RegExpr) =
    val (cuts, rNot) = cutBy(r, c)
    val (r1s, r2s) = cuts.unzip
    ((merge(r1s), mkConcat(fromChar(c), merge(r2s))), rNot)

  def langSet(re: RegExpr): Set[String] = re match
    case RENone => Set.empty
    case RENull => Set("")
    case REChar(cs) if cs.polarity => cs.chars.map(_.toString)
    case REConcat(r1, r2) =>
      cartesianProduct(langSet(r1), langSet(r2), _ + _)
    case REUnion(r1, r2) =>
      langSet(r1) | langSet(r2)
    case RELoop(NatRange(m1, Some(m2)), r) =>
      Set.from(for
        k <- m1 to m2
        s <- if k == 0 then Set("") else cartesianPower(langSet(r), k, _ + _)
      yield s)
    case _ => throw IllegalArgumentException()

  private def size(re: RegExpr): Option[Int] = re match
    case RENone => Some(0)
    case RENull => Some(1)
    case REChar(cs) => Some(cs.size)
    case REConcat(r1, r2) =>
      for x1 <- size(r1); x2 <- size(r2) yield x1 * x2
    case REUnion(r1, r2) =>
      for x1 <- size(r1); x2 <- size(r2) yield x1 + x2
    case RELoop(NatRange(m1, m), r) =>
      for m2 <- m; x <- size(r) yield (for k <- m1 to m2 yield pow(x, k).toInt).sum

  def tryEnumerate(re: RegExpr): Option[Set[String]] =
    if size(re.normalize).exists(_ < 20) then Some(langSet(re)) else None

  def containsWord(re: RegExpr, word: String): Boolean = word.length match
    case 0 => re.nullable
    case 1 => length(re) == NatRange.at(1) && alphabet(re).contains(word.head)
    case _ =>
      tryEnumerate(re) match
        case Some(words) => words.contains(word)
        case None => throw UnsupportedOperationException(re.toString)

  def derivative(re: RegExpr, c: Char): RegExpr = re match
    case RENone | RENull => RENone
    case REChar(cs) => if cs.contains(c) then RENull else RENone
    case REConcat(r1, r2) =>
      val r = derivative(r1, c)
      if r1.nullable then mkUnion(r, derivative(r2, c)) else mkConcat(r, r2)
    case REUnion(r1, r2) => mkUnion(derivative(r1, c), derivative(r2, c))
    case RELoop(q, r1) => mkConcat(derivative(r1, c), mkLoop(q - 1, r1))

  @tailrec
  private def mayStartWith(re: RegExpr, prefix: String): Boolean =
    if prefix.isEmpty then true
    else
      val r = derivative(re, prefix.head)
      r != RENone && mayStartWith(r, prefix.tail)

  def startsWith(re: RegExpr, prefix: String): Option[Boolean] =
    if mayStartWith(re, prefix) then
      if length(re).lower >= prefix.length && take(re, prefix.length).tryAsString.isDefined then Some(true) else None
    else Some(false)
