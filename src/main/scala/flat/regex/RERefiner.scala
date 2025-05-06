package flat.regex

import flat.Ops.CmpOp

object RERefiner:

  import CmpOp.*
  import REOps.merge
  import RegExpr.*

  def refineByLen(r: RegExpr, in: NatRange): RegExpr =
    val len = in & REOps.length(r)
    if len.isEmpty then
      return RENone

    r match
      case REConcat(r1, r2) =>
        val len1 = REOps.length(r1)
        val len2 = REOps.length(r2)
        if len1.isPoint && !len2.isPoint then mkConcat(r1, refineByLen(r2, len - len1.lower))
        else if len2.isPoint && !len1.isPoint then mkConcat(refineByLen(r1, len - len2.lower), r2)
        else r
      case REUnion(r1, r2) =>
        mkUnion(refineByLen(r1, len), refineByLen(r2, len))
      case RELoop(range1, r1) =>
        val len1 = REOps.length(r1)
        if len1.isPoint && len1.lower > 0 then
          val lower = Math.ceilDiv(len.lower, len1.lower)
          val upper = len.upper.map(m => Math.floorDiv(m, len1.lower))
          val loopRange = NatRange(lower, upper) & range1
          if loopRange.isEmpty then RENone else mkLoop(loopRange, r1)
        else r
      case _ => r

  def refineByLen(r: RegExpr, constraint: (CmpOp, Int)): RegExpr =
    val ranges = constraint match
      case (EQ, n) => List(NatRange.at(n))
      case (NE, n) => (if n > 0 then List(NatRange(0, n - 1)) else Nil) :+ NatRange(n + 1)
      case (LE, n) => List(NatRange(0, n))
      case (LT, n) => if n > 0 then List(NatRange(0, n - 1)) else Nil
      case (GE, n) => List(NatRange(n))
      case (GT, n) => List(NatRange(n + 1))
    merge(ranges.map(refineByLen(r, _)))

  def refineByCharAt(r: RegExpr, k: Int, p: CharSet => Boolean): RegExpr =
    merge:
      for
        (r1, cs, r2) <- REOps.cutAt(r, k)
        if p(cs)
      yield mkConcat(r1, REChar(cs), r2)

  def refineByCharAt(r: RegExpr, k: Int, constraint: (CmpOp, Char)): RegExpr =
    val (op, c) = constraint
    val p: CharSet => Boolean = op match
      case EQ => cs => cs.isSingleton && cs.contains(c)
      case NE => !_.contains(c)
      case _ => throw IllegalArgumentException(op.toString)
    refineByCharAt(r, k, p)

  def refineByIndexOf(r: RegExpr, c: Char, constraint: (CmpOp, Int)): RegExpr =
    val (cuts, _) = REOps.cutBy(r, c)
    val rc = fromChar(c)
    merge:
      for
        (rl, rr) <- cuts
        r = refineByLen(rl, constraint)
        if constraint != RENone
      yield mkConcat(r, rc, rr)

  def refineByNotFound(r: RegExpr, c: Char): RegExpr =
    val (_, rNot) = REOps.cutBy(r, c)
    rNot