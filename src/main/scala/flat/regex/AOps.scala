package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.regex.RegEx.*

import scala.annotation.tailrec

/** Abstract string operations, defined over the domain of `RegEx`es. */
object AOps extends LazyLogging:
  extension (re: RegEx)
    /** Abstract version of `s.take(1)`. */
    def take1: RegEx = re match
      case RENone => RENull
      case RENull => RENull
      case RELit(_) => re
      case REConcat(r1, r2) =>
        if r1.nullable then r1.minusNull.take1 | r2.take1 // the character comes from either r1 or r2
        else r1.take1
      case REUnion(r1, r2) => r1.take1 | r2.take1
      case REStar(r) => r.take1 | RENull

    /** Tests if ''all'' members of this regex start with the prefix `t`. */
    @tailrec
    def forallPrefix(t: String): Boolean =
      if t.isEmpty then true
      else !take1.nullable && re.first.isSingleton && re.first.contains(t.head) &&
        re.derivative(t.head).forallPrefix(t.tail)

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

    /** Splits this regex at ''any'' occurrence of the nonempty string `t`.
     * Returns a list of splits: each is prefix-suffix pair `(rl, rr)` where `rr` starts with `c`. */
    def split(t: String): List[(RegEx, RegEx)] =
      require(t.nonEmpty)
      for
        (rl, rr) <- split(t.head)
        r2 = rr.derivative(t)
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
    def take(k: Int): RegEx =
      require(k >= 0)
      k match
        case 0 => RENull
        case 1 => take1
        case _ => take1 ++ drop1.take(k - 1)

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
      case REStar(r) => Interval()
