package flat.regex

import flat.Ops.CmpOp
import flat.regex.AOps.*
import flat.regex.simpl.extractCommonFactor

object RERefiner:

  import CmpOp.*
  import RegEx.*

  def refineByLen(r: RegEx, in: Interval): RegEx =
    val len = in & r.length
    if len.isEmpty then
      return RENone

    r match
      case REConcat(r1, r2) =>
        val len1 = r1.length
        val len2 = r2.length
        if len1.isSingleton && !len2.isSingleton then concat(r1, refineByLen(r2, len - len1.lb))
        else if len2.isSingleton && !len1.isSingleton then concat(refineByLen(r1, len - len2.lb), r2)
        else r
      case REUnion(r1, r2) =>
        union(refineByLen(r1, len), refineByLen(r2, len))
      case REStar(r1) =>
        val len1 = r1.length
        if len1.isSingleton && len1.lb > 0 then
          val lower = Math.ceilDiv(len.lb, len1.lb)
          val upper = len.ub match
            case m: Int => Math.floorDiv(m, len1.lb)
            case _ => Inf
          val loopRange = Interval(lower, upper)
          if loopRange.isEmpty then RENone else r1.loop(loopRange)
        else if !in.contains(0) then r1.+
        else r
      //      case RELoop(range1, r1) =>
      //        val len1 = r1.length
      //        if len1.isPoint && len1.lower > 0 then
      //          val lower = Math.ceilDiv(len.lower, len1.lower)
      //          val upper = len.upper.map(m => Math.floorDiv(m, len1.lower))
      //          val loopRange = Interval(lower, upper) & range1
      //          if loopRange.isEmpty then RENone else r1.loop(loopRange)
      //        else r
      case _ => r

  private def merge(cases: List[RegEx]): RegEx = union(cases).extractCommonFactor

  def refineByLen(r: RegEx, constraint: (CmpOp, Int)): RegEx =
    val ranges = constraint match
      case (EQ, n) => List(Interval.at(n))
      case (NE, n) => (if n > 0 then List(Interval(0, n - 1)) else Nil) :+ Interval(lb = n + 1)
      case (LE, n) => List(Interval(0, n))
      case (LT, n) => if n > 0 then List(Interval(0, n - 1)) else Nil
      case (GE, n) => List(Interval(lb = n))
      case (GT, n) => List(Interval(lb = n + 1))
    merge(ranges.map(refineByLen(r, _)))

  def refineByIndexOf(r: RegEx, c: Char, constraint: (CmpOp, Int)): RegEx =
    merge:
      for
        (rl, rr) <- r.find(c)
        r1 = refineByLen(rl, constraint)
        if !r1.isEmpty
      yield r1 ++ rr

  extension (re: RegEx)
    private def splitAt0: List[(CharSet, RegEx)] = re match
      case RENone => Nil
      case RENull => Nil
      case RELit(cs) => List((cs, RENull))
      case REConcat(r1, r2) =>
        (for (cs, r) <- r1.splitAt0 yield (cs, r ++ r2)) ++ (if r1.nullable then r2.splitAt0 else Nil)
      case REUnion(r1, r2) => r1.splitAt0 ++ r2.splitAt0
      case REStar(r) => for (cs, r1) <- r.splitAt0 yield (cs, r1 ++ re)

    def splitAt(k: Int): List[(RegEx, CharSet, RegEx)] =
      require(k >= 0)
      if k == 0 then for (cs, r) <- re.splitAt0 yield (RENull, cs, r)
      else
        for
          (cs1, r) <- re.splitAt0
          (r1, cs, r2) <- r.splitAt(k - 1)
        yield (RELit(cs1) ++ r1, cs, r2)

  def refineByCharAt(r: RegEx, k: Int, in: CharSet): RegEx =
    merge:
      for
        (r1, cs, r2) <- r.splitAt(k)
        cs1 = cs & in
        if !cs1.isEmpty
      yield r1 ++ RELit(cs1) ++ r2

  def refineByCharAt(r: RegEx, k: Int, constraint: (CmpOp, Char)): RegEx =
    val (op, c) = constraint
    val in = op match
      case EQ => CharSet(true, Set(c))
      case NE => CharSet(false, Set(c))
      case _ => throw IllegalArgumentException(op.toString)
    refineByCharAt(r, k, in)