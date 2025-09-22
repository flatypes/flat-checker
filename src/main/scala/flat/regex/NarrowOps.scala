package flat.regex

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp
import flat.Ops.CmpOp.*
import flat.regex.AOps.*
import flat.regex.RegEx.*
import flat.regex.simpl.extractCommonFactor

object NarrowOps extends LazyLogging:
  extension (re: RegEx)
    /** Narrows by the constraint that `|s|` is in `interval`. */
    def narrowByLength(interval: Interval): RegEx =
      val len = interval & re.length
      if len.isEmpty then
        return RENone

      re match
        case REConcat(r1, r2) =>
          val len1 = r1.length
          val len2 = r2.length
          if len1.isSingleton && !len2.isSingleton then r1 ++ r2.narrowByLength(len - len1.lb)
          else if len2.isSingleton && !len1.isSingleton then r1.narrowByLength(len - len2.lb) ++ r2
          else re
        case REUnion(r1, r2) => r1.narrowByLength(len) | r2.narrowByLength(len)
        case REStar(r1) =>
          val len1 = r1.length
          if len1.isSingleton && len1.lb > 0 then
            val lower = Math.ceilDiv(len.lb, len1.lb)
            val upper = len.ub match
              case m: Int => Math.floorDiv(m, len1.lb)
              case _ => Inf
            val loopRange = Interval(lower, upper)
            if loopRange.isEmpty then RENone else r1.loop(loopRange)
          else if !interval.contains(0) then r1.+
          else re
        case _ => re

    /** Narrows by the constraint that `s.find(c)` is in `interval`. */
    def narrowByFirstIndexOf(c: Char, interval: Interval): RegEx =
      val cases = for
        (rl, rr) <- re.find(c)
        r1 = rl.narrowByLength(interval)
        if !r1.isEmpty
      yield r1 ++ rr
      union(cases).extractCommonFactor

    /** Narrows by the constraint that `s[i] = c` for some `i`. Or, `s` contains `c`. */
    def narrowBySomeCharEq(c: Char): RegEx =
      union(re.find(c).map(_ ++ _)).extractCommonFactor

    /** Narrows by the constraint that `s[i] != c` for some `i`. */
    def narrowBySomeCharNotEq(c: Char): RegEx = re match
      case REUnion(r1, r2) => r1.narrowBySomeCharNotEq(c) | r2.narrowBySomeCharNotEq(c)
      case r => if r.alphabet.isSingletonOf(c) then RENone else r
    // union(List(r1, r2).filter(r => (r.alphabet & CharSet(false, Set(c))).nonEmpty))

    /** Narrows by the constraint that `s[i]` does not contain `c`. */
    def narrowByNotFound(c: Char): RegEx = re match
      case RELit(cs) => fromCharSet(cs - c)
      case REConcat(r1, r2) => r1.narrowByNotFound(c) ++ r2.narrowByNotFound(c)
      case REUnion(r1, r2) => r1.narrowByNotFound(c) | r2.narrowByNotFound(c)
      case _ => re

    /** Narrows by the constraint that `s[k]` is in `charSet`. */
    def narrowByChatAt(k: Int, charSet: CharSet): RegEx =
      val cases = for
        (r1, cs, r2) <- re.splitAt(k)
        cs1 = cs & charSet
        if !cs1.isEmpty
      yield r1 ++ RELit(cs1) ++ r2
      union(cases).extractCommonFactor

    /** Narrows by the constraint that `s = t`. */
    def narrowByEq(t: String): RegEx =
      if re.contains(t) then fromString(t) else RENone

    /** Narrows by the constraint that `s != t`. */
    def narrowByNotEq(t: String): RegEx = t.length match
      case 0 => narrowByLength(Interval(lb = 1))
      case n =>
        val r1 = narrowByLength(Interval(0, n - 1)) | narrowByLength(Interval(lb = n + 1))
        val r2 = narrowByLength(Interval.at(n)).exclude(t)
        r1 | r2

    private def exclude(t: String): RegEx =
      union(for k <- t.indices.toList yield re.narrowByChatAt(k, CharSet(false, Set(t.charAt(k)))))

  extension (re: RegEx)
    private def splitAt0: List[(CharSet, RegEx)] = re match
      case RENone => Nil
      case RENull => Nil
      case RELit(cs) => List((cs, RENull))
      case REConcat(r1, r2) =>
        (for (cs, r) <- r1.splitAt0 yield (cs, r ++ r2)) ++ (if r1.nullable then r2.splitAt0 else Nil)
      case REUnion(r1, r2) => r1.splitAt0 ++ r2.splitAt0
      case REStar(r) => for (cs, r1) <- r.splitAt0 yield (cs, r1 ++ re)

    private def splitAt(k: Int): List[(RegEx, CharSet, RegEx)] =
      require(k >= 0)
      if k == 0 then for (cs, r) <- re.splitAt0 yield (RENull, cs, r)
      else
        for
          (cs1, r) <- re.splitAt0
          (r1, cs, r2) <- r.splitAt(k - 1)
        yield (RELit(cs1) ++ r1, cs, r2)
