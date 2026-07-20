package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.regex
import flat.regex.RegEx.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Abstract string operations, defined over the domain of `RegEx`es. */
object AOps extends LazyLogging:
  extension (re: RegEx)
    /** Abstract version of `s.take(1)`. */
    def take1: RegEx = re match
      case RENone => RENull
      case RENull => RENull
      case RELit(_) => re
      case REConcat(r1, r2) =>
        if r1.nullable then r1.take1.minusNull | r2.take1 // the character comes from either r1 or r2
        else r1.take1
      case REUnion(r1, r2) => r1.take1 | r2.take1
      case REStar(r) => r.take1 | RENull

    /** Tests if ''all'' members of this regex start with the prefix `t`. */
    @tailrec
    def forallPrefix(t: String): Boolean =
      if t.isEmpty then true
      else !take1.nullable && re.first.isSingleton && re.first.contains(t.head) &&
        re.deriv(t.head).forallPrefix(t.tail)

    def startsWith(t: String): Option[Boolean] =
      if t.isEmpty then Some(true)
      else if re.deriv(t).isEmpty then Some(false)
      else if forallPrefix(t) then Some(true)
      else None

    /** Splits this regex at ''any'' occurrence of the char `c`.
     * Returns a list of splits: each is prefix-suffix pair `(rl, rr)` where `rr` starts with `c`. */
    def split(c: Char): List[(RegEx, RegEx)] = re match
      case RENone => Nil
      case RENull => Nil
      case RELit(cs) => if cs.contains(c) then List((RENull, re)) else Nil
      case REConcat(r1, r2) =>
        (for (r1l, r1r) <- r1.split(c) yield (r1l, r1r ++ r2)) ++ (for (r2l, r2r) <- r2.split(c) yield (r1 ++ r2l, r2r))
      case REUnion(r1, r2) => r1.split(c) ++ r2.split(c)
      case REStar(r) => for (rl, rr) <- r.split(c) yield (re ++ rl, rr ++ re)

    def splitPrefix(c: Char): RegEx = union(split(c).map(_._1))
    def splitSuffix(c: Char): RegEx = union(split(c).map(_._2))

    def split(cs: CharSet): List[(RegEx, RegEx)] = re match
      case RENone => Nil
      case RENull => Nil
      case RELit(cs1) =>
        val cs2 = cs1 & cs
        if cs2.isEmpty then Nil else List((RENull, RELit(cs2)))
      case REConcat(r1, r2) =>
        (for (r1l, r1r) <- r1.split(cs) yield (r1l, r1r ++ r2)) ++
          (for (r2l, r2r) <- r2.split(cs) yield (r1 ++ r2l, r2r))
      case REUnion(r1, r2) => r1.split(cs) ++ r2.split(cs)
      case REStar(r) => for (rl, rr) <- r.split(cs) yield (re ++ rl, rr ++ re)

    def splitPrefix(cs: CharSet): RegEx = union(split(cs).map(_._1))
    def splitSuffix(cs: CharSet): RegEx = union(split(cs).map(_._2))

    /** Splits this regex at ''any'' occurrence of the nonempty string `t`.
     * Returns a list of splits: each is prefix-suffix pair `(rl, rr)` where `rr` starts with `c`. */
    def split(t: String): List[(RegEx, RegEx)] =
      require(t.nonEmpty)
      for
        (rl, rr) <- split(t.head)
        r2 = rr.deriv(t)
        if !r2.isEmpty
      yield (rl, fromString(t) ++ r2)

    def splitPrefix(t: String): RegEx = union(split(t).map(_._1))
    def splitSuffix(t: String): RegEx = union(split(t).map(_._2))

    /** Tests if ''all'' members of this regex contains the char `c`. */
    def forallContains(c: Char): Boolean = re match
      case RENone => false
      case RENull => false
      case RELit(cs) => cs.isSingleton && cs.contains(c)
      case REConcat(r1, r2) => r1.forallContains(c) || r2.forallContains(c)
      case REUnion(r1, r2) => r1.forallContains(c) && r2.forallContains(c)
      case REStar(r) => false

    /** Tests if ''all'' members of this regex include the infix `t`. */
    def forallInfix(t: String): Boolean = t.length match
      case 0 => true
      case 1 => forallContains(t.head)
      case _ => forallContains(t.head) && splitSuffix(t.head).forallPrefix(t)

    def containsInfix(t: String): Option[Boolean] = t.length match
      case 0 => Some(true)
      case 1 =>
        val c = t.head
        if forallContains(c) then Some(true)
        else if !re.alphabet.contains(c) then Some(false)
        else None
      case _ =>
        val r1 = re.splitSuffix(t.head)
        if r1.deriv(t).isEmpty then Some(false)
        else if re.forallContains(t.head) && r1.forallPrefix(t) then Some(true)
        else None

    /** Abstract version of `s.drop(1)`. */
    def drop1: RegEx = re match
      case RENone => RENull
      case RENull => RENull
      case RELit(_) => RENull
      case REConcat(r1, r2) => if r1.nullable then (r1.drop1 ++ r2) | r2.drop1 else r1.drop1 ++ r2
      case REUnion(r1, r2) => r1.drop1 | r2.drop1
      case REStar(r) => (r.drop1 ++ re) | RENull

    /** Abstract version of `s.drop(k)`. */
    @tailrec
    def drop(k: Int): RegEx =
      require(k >= 0)
      k match
        case 0 => re
        case 1 => drop1
        case _ => drop1.drop(k - 1)

    /** Abstract version of `s.take(k)`. */
    def takeOld(k: Int): RegEx =
      require(k >= 0)
      k match
        case 0 => RENull
        case 1 => take1
        case _ => take1 ++ drop1.takeOld(k - 1)

    def take(k: Int): RegEx =
      k match
        case 0 => RENull
        case _ =>
          val cs = re.first
          val r = if cs.isEmpty then RENone else fromCharSet(cs) ++ re.derivativeAny.take(k - 1)
          if re.nullable then r | RENull else r

    /** Returns a sub-language of this regex of which ''all'' members no longer contain `c`. */
    def exclude(c: Char): RegEx = re match
      case RENone => RENone
      case RENull => RENull
      case RELit(cs) => fromCharSet(cs - c)
      case REConcat(r1, r2) => r1.exclude(c) ++ r2.exclude(c)
      case REUnion(r1, r2) => r1.exclude(c) | r2.exclude(c)
      case REStar(r) => r.exclude(c).*

    /** Splits this regex at the ''first'' occurrence of the char `c`.
     * Returns a list of splits: each is prefix-suffix pair `(rl, rr)` where `rr` starts with `c`. */
    def find(c: Char): List[(RegEx, RegEx)] =
      for
        (rl, rr) <- split(c)
        r1 = rl.exclude(c)
        if !r1.isEmpty
      yield (r1, fromChar(c) ++ rr.drop1)

    def findPrefix(c: Char): RegEx = union(find(c).map(_._1))
    def findSuffix(c: Char): RegEx = union(find(c).map(_._2))

    /** Abstract version of `s.count`. */
    def count(c: Char): Interval = re match
      case RENone => Interval.at(0)
      case RENull => Interval.at(0)
      case RELit(cs) => if cs.contains(c) then Interval.at(1) else Interval.at(0)
      case REConcat(r1, r2) => r1.count(c) + r2.count(c)
      case REUnion(r1, r2) => r1.count(c) | r2.count(c)
      case REStar(r) => if r.count(c) == Interval(0, 0) then Interval.at(0) else Interval(lb = 0)

    def splitAtFirst(c: Char): (List[(RegEx, RegEx)], RegEx) = re match
      case RENone => (Nil, RENone)
      case RENull => (Nil, RENull)
      case RELit(cs) =>
        if !cs.contains(c) then (Nil, re)
        else (List(RENull -> fromChar(c)), fromCharSet(cs - c))
      case REConcat(r1, r2) =>
        val (splits1, r1NotFound) = r1.splitAtFirst(c)
        val splits = ListBuffer.empty[(RegEx, RegEx)]
        for (r1l, r1r) <- splits1 do
          splits += (r1l -> (r1r ++ r2))
        if r1NotFound.isEmpty then
          (splits.toList, RENone)
        else
          val (splits2, r2NotFound) = r2.splitAtFirst(c)
          for (r2l, r2r) <- splits2 do
            splits += (r1NotFound ++ r2l) -> r2r
          (splits.toList, r1NotFound ++ r2NotFound)
      case REUnion(r1, r2) =>
        val (splits1, r1NotFound) = r1.splitAtFirst(c)
        val (splits2, r2NotFound) = r2.splitAtFirst(c)
        (splits1 ++ splits2, r1NotFound | r2NotFound)
      case REStar(r) =>
        val (splits1, r1NotFound) = r.splitAtFirst(c)
        if r1NotFound.isEmpty then
          (for (r1l, r1r) <- splits1 yield r1l -> (r1r ++ re), RENull)
        else
          (for (r1l, r1r) <- splits1 yield (r1NotFound.* ++ r1l) -> (r1r ++ re), r1NotFound.*)

    def splitWith(c: Char): AList =
      require(count(c) != Inf, "finite split")
      val (splits, rNotFound) = splitAtFirst(c)
      AList((if rNotFound.isEmpty then Nil else List(List(rNotFound))) ++
        (for (rl, rr) <- splits; t <- rr.drop1.splitWith(c).traces yield rl :: t))

    /** Abstract version of `s.drop(k)` where `k = index.concretize(s)`. */
    def drop(index: BasicIndex): RegEx = index match
      case IndexL(k) => drop(k)
      case IndexR(k) => re.reverse.take(k).reverse
      case IndexAt(t) =>
        t.length match
          case 0 => re
          case 1 => findSuffix(t.head)
          case _ => splitSuffix(t)

    /** Abstract version of `s.take(k)` where `k = index.concretize(s)`. */
    def take(index: BasicIndex): RegEx = index match
      case IndexL(k) => take(k)
      case IndexR(k) => re.reverse.drop(k).reverse
      case IndexAt(t) =>
        t.length match
          case 0 => RENull
          case 1 => findPrefix(t.head)
          case _ => splitPrefix(t)

    /** Abstract version of `s.length`. */
    def length: Interval = re match
      case RENone => Interval.at(0)
      case RENull => Interval.at(0)
      case RELit(_) => Interval.at(1)
      case REConcat(r1, r2) => r1.length + r2.length
      case REUnion(r1, r2) => r1.length | r2.length
      case REStar(r) => Interval(lb = 0)

    /** Tests if this regular language is *small*: free of Kleene stars and big CS. */
    def isSmall: Boolean = re.size match
      case n: BigInt if n < 50 => true
      case _ => false

    def size: BigInt | Inf.type = re match
      case RENone => 0
      case RENull => 1
      case RELit(cs) => cs.size
      case REConcat(r1, r2) =>
        (r1.size, r2.size) match
          case (n1: BigInt, n2: BigInt) => n1 * n2
          case _ => Inf
      case REUnion(r1, r2) =>
        (r1.size, r2.size) match
          case (n1: BigInt, n2: BigInt) => n1 + n2
          case _ => Inf
      case REStar(_) => Inf

    def words: Set[String] = re match
      case RENone => Set.empty
      case RENull => Set("")
      case RELit(cs) => cs.toSet.map(_.toString)
      case REConcat(r1, r2) =>
        for
          w1 <- r1.words
          w2 <- r2.words
        yield w1 + w2
      case REUnion(r1, r2) => r1.words | r2.words
      case REStar(_) => Set.empty

    def toNumber(base: Int = 10): Interval =
      require(2 <= base && base <= 36)
      val lb = parseInt(re.minNumber(base), base)
      val ub = re.maxNumber(base) match
        case Some(s) => Integer.parseInt(s, base)
        case None => Inf
      Interval(lb, ub)

    private def minNumber(base: Int): String = re match
      case RENone => throw IllegalArgumentException()
      case RENull => ""
      case RELit(cs) => cs.toSet.map(_.toUpper).min.toString
      case REConcat(r1, r2) =>
        val s1 = r1.minNumber(base)
        val s2 = r2.minNumber(base)
        s1 + s2
      case REUnion(r1, r2) =>
        val s1 = r1.minNumber(base)
        val s2 = r2.minNumber(base)
        if parseInt(s1, base) <= parseInt(s2, base) then s1 else s2
      case REStar(_) => ""

    private def maxNumber(base: Int): Option[String] = re match
      case RENone => throw IllegalArgumentException()
      case RENull => Some("")
      case RELit(cs) => Some(cs.toSet.map(_.toUpper).max.toString)
      case REConcat(r1, r2) =>
        for s1 <- r1.maxNumber(base); s2 <- r2.maxNumber(base) yield s1 + s2
      case REUnion(r1, r2) =>
        for s1 <- r1.maxNumber(base); s2 <- r2.maxNumber(base) yield
          if parseInt(s1, base) >= parseInt(s2, base) then s1 else s2
      case REStar(_) => None

    def toLower: RegEx = re match
      case RENone => RENone
      case RENull => RENull
      case RELit(cs) => RELit(CharSetAbs.toLower(cs))
      case REConcat(r1, r2) => r1.toLower ++ r2.toLower
      case REUnion(r1, r2) => r1.toLower | r2.toLower
      case REStar(r) => r.toLower.*

    def toUpper: RegEx = re match
      case RENone => RENone
      case RENull => RENull
      case RELit(cs) => RELit(CharSetAbs.toUpper(cs))
      case REConcat(r1, r2) => r1.toUpper ++ r2.toUpper
      case REUnion(r1, r2) => r1.toUpper | r2.toUpper
      case REStar(r) => r.toUpper.*

  private inline def parseInt(s: String, base: Int): Int =
    if s.isEmpty then 0 else Integer.parseInt(s, base)

type Trace = List[RegEx]

extension (trace: Trace)
  def find(s: String, fromLeft: Int, untilRight: Int): Set[Int] =
    require(fromLeft >= 0 && untilRight >= 0)
    var continue = true
    var i = fromLeft
    val indices = mutable.Set.empty[Int]
    while continue && i < trace.length - untilRight do
      val r = trace(i)
      if r.isSingleton then
        if r.contains(s) then
          indices += i
          continue = false
        else
          i += 1
      else
        if r.contains(s) then
          indices += 1
        i += 1
    if continue then
      indices += -1
    indices.toSet

  def takeIndexOf(s: String, fromLeft: Int, untilRight: Int): List[Trace] =
    val indices = trace.find(s, fromLeft, untilRight)
    indices.excl(-1).toList.sorted.map(trace.take)

  def dropIndexOf(s: String, fromLeft: Int, untilRight: Int): List[Trace] =
    val indices = trace.find(s, fromLeft, untilRight)
    indices.excl(-1).toList.sorted.map(trace.drop)

  def count(s: String): Interval =
    var must = 0
    var may = 0
    for r <- trace do
      if r.isSingleton then
        if r.contains(s) then
          must += 1
      else if r.contains(s) then
        may += 1
    Interval(must, must + may)

  def narrow(i: Int, f: RegEx => RegEx): Option[Trace] =
    require(i >= 0)
    for r <- trace.lift(i); r1 = f(r); if !r1.isEmpty yield trace.updated(i, r1)

  def narrowRight(i: Int, f: RegEx => RegEx): Option[Trace] =
    require(i > 0)
    val k = trace.length - i
    for r <- trace.lift(k); r1 = f(r); if !r1.isEmpty yield trace.updated(k, r1)

  def narrowEach(fromLeft: Int, untilRight: Int, f: RegEx => RegEx): Option[Trace] =
    require(fromLeft >= 0 && untilRight >= 1)
    val mid = trace.slice(fromLeft, trace.length - untilRight).map(f)
    if mid.exists(_.isEmpty) then None
    else Some(trace.take(fromLeft) ++ mid ++ trace.takeRight(untilRight))

  def narrowSomeFromIndexOfUntil(indexOf: (String, Int, Int), shift: Int, untilRight: Int,
                                 f: RegEx => RegEx): List[Trace] =
    val indices = trace.find(indexOf._1, indexOf._2, indexOf._3).excl(-1)
    if indices.isEmpty then Nil
    else
      List.from:
        for
          i <- indices.min + shift until trace.length - untilRight
          r = f(trace(i))
          if !r.isEmpty
        yield trace.updated(i, r)

final class AList(val traces: List[List[RegEx]]) extends Domain:
  def isEmpty: Boolean = traces.isEmpty

  def length: Interval =
    val lengths = traces.map(_.length)
    Interval(lengths.min, lengths.max)

  def get(i: Int): RegEx =
    require(i >= 0)
    union(traces.flatMap(_.lift(i)))

  def getRight(i: Int): RegEx =
    require(i > 0)
    union(traces.flatMap(t => t.lift(t.length - i)))

  def getEach(fromLeft: Int, untilRight: Int): RegEx =
    require(fromLeft >= 0 && untilRight >= 0)
    val rs = traces.map(t => t.slice(fromLeft, t.length - untilRight)).filter(_.nonEmpty).flatten
    union(rs.toSet.toList)

  def getAny: RegEx = getEach(0, 0)

  def drop(n: Int): AList =
    require(n >= 0)
    AList(traces.map(_.drop(n)).filter(_.nonEmpty))

  def dropRight(n: Int): AList =
    require(n >= 1)
    AList(traces.map(_.dropRight(n)).filter(_.nonEmpty))

  def takeIndexOf(s: String, fromLeft: Int, untilRight: Int): AList =
    AList(traces.flatMap(_.takeIndexOf(s, fromLeft, untilRight)))

  def dropIndexOf(s: String, fromLeft: Int, untilRight: Int): AList =
    AList(traces.flatMap(_.dropIndexOf(s, fromLeft, untilRight)))

  def ++(that: AList): AList = AList(for t1 <- traces; t2 <- that.traces yield t1 ++ t2)

  def append(r: RegEx): AList = AList(traces.map(_ :+ r))

  def indexOf(s: String, fromLeft: Int = 0, untilRight: Int = 0): Set[Int] =
    require(fromLeft >= 0 && untilRight >= 0)
    traces.map(_.find(s, fromLeft, untilRight)).reduce(_ | _)

  def rightIndexOf(s: String, fromLeft: Int = 0, untilRight: Int = 0): Set[Int] =
    require(fromLeft >= 0 && untilRight >= 0)
    traces.map(t => t.find(s, fromLeft, untilRight).map { case -1 => -1; case i => t.length - i }).reduce(_ | _)

  def count(s: String): Interval = traces.map(_.count(s)).reduce(_ | _)

  def narrow(i: Int, f: RegEx => RegEx): AList =
    require(i >= 0)
    AList(traces.flatMap(_.narrow(i, f)))

  def narrowRight(i: Int, f: RegEx => RegEx): AList =
    require(i > 0)
    AList(traces.flatMap(_.narrowRight(i, f)))

  def narrowEach(fromLeft: Int, untilRight: Int, f: RegEx => RegEx): AList =
    require(fromLeft >= 0 && untilRight >= 1)
    AList(traces.flatMap(_.narrowEach(fromLeft, untilRight, f)))

  def narrowSomeFromIndexOfUntil(indexOf: (String, Int, Int), shift: Int, untilRight: Int, f: RegEx => RegEx): AList =
    AList(traces.flatMap(_.narrowSomeFromIndexOfUntil(indexOf, shift, untilRight, f)))

  def unsplit(c: Char): RegEx =
    union(traces.map(trace => trace.reduce(_ ++ fromChar(c) ++ _)).distinct)

  override def toString: String =
    traces.map(_.mkString(", ")).mkString("\n")