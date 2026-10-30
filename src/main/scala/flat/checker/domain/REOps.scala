package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.domain.CmpOp.*
import flat.checker.domain.RegEx.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Index[+T]

sealed trait PosIndex[T] extends Index[T]

/** Index from left: `n`. */
final case class ILeft(n: Int) extends PosIndex[Nothing]

/** Index of the first occurrence of a given word, possibly with shift: `find(s, start) + offset`. */
final case class IFirst[T](s: List[T], start: Index[T], offset: Int) extends PosIndex[T]

sealed trait NegIndex[T] extends Index[T]

/** Index from right: `len - n`. */
final case class IRight(n: Int) extends NegIndex[Nothing]

// final case class ILast[T](s: List[T], start: NegIndex[T], offset: Int) extends NegIndex[T]

enum CmpOp:
  case EQ, NE, LE, LT, GE, GT

  def flip: CmpOp = this match
    case LE => GE
    case LT => GT
    case GE => LE
    case GT => LT
    case _ => this

object REOps extends LazyLogging:
  // ElemAt, Slice
  extension [T, S](r: RegEx[T, S])(using litOps: LitOps[T, S])
    def deriv(x: T): RegEx[T, S] = r match
      case Zero() | One() => Zero()
      case Lit(a) => if a.contains(x) then One() else Zero()
      case Plus(r1, r2) => r1.deriv(x) + r2.deriv(x)
      case Comp(r1, r2) => r1.deriv(x) * r2 + (if r1.nullable then r2.deriv(x) else Zero())
      case Star(r1) => r1.deriv(x) * r

    @tailrec
    def deriv(s: List[T]): RegEx[T, S] = s match
      case Nil => r
      case x :: xs =>
        val r1 = r.deriv(x)
        if r1.isEmpty then Zero() else r1.deriv(xs)

    private def absDrop1: RegEx[T, S] = r match
      case Zero() | One() | Lit(_) => One()
      case Plus(r1, r2) => r1.absDrop1 + r2.absDrop1
      case Comp(r1, r2) => r1.absDrop1 * r2 + (if r1.nullable then r2.absDrop1 else Zero())
      case Star(r1) => r1.absDrop1 * r

    @tailrec
    def absDrop(n: Int): RegEx[T, S] =
      require(n >= 0, s"$n is negative")
      if n == 0 then r
      else r.absDrop1.absDrop(n - 1)

    def absElemAt(n: Int): S =
      require(n >= 0, s"$n is negative")
      r.absDrop(n).first

    def absHeadTail: List[(S, RegEx[T, S])] = r match
      case Zero() | One() => Nil
      case Lit(a) => if a.isEmpty then Nil else List((a, One()))
      case Plus(r1, r2) => r1.absHeadTail ++ r2.absHeadTail
      case Comp(r1, r2) =>
        val splits = for (a, tail) <- r1.absHeadTail yield (a, tail * r2)
        if r1.nullable then splits ++ r2.absHeadTail else splits
      case Star(r1) => for (a, tail) <- r1.absHeadTail yield (a, tail * r)

    def absSplitAt(n: Int): List[(RegEx[T, S], RegEx[T, S])] =
      require(n >= 0)
      if n == 0 then List((One(), r))
      else
        val splits = for
          (head, tail) <- r.absHeadTail
          (mid, right) <- tail.absSplitAt(n - 1)
        yield (Lit(head) * mid, right)
        if r.nullable then (One(), One()) :: splits else splits

    def absTake(n: Int): RegEx[T, S] =
      require(n >= 0)
      if n == 0 then One()
      else sum(r.absSplitAt(n).map(_._1))

    /** Search for any occurrence of the given symbol `x`, and return the possibilities as a list of pairs
     * (before `x`, after `x`). */
    def search(x: T): List[(RegEx[T, S], RegEx[T, S])] = r match
      case Zero() | One() => Nil
      case Lit(a) => if a.contains(x) then List((One(), One())) else Nil
      case Plus(r1, r2) => r1.search(x) ++ r2.search(x)
      case Comp(r1, r2) =>
        (for (before, after) <- r1.search(x) yield (before, after * r2))
          ++ (for (before, after) <- r2.search(x) yield (r1 * before, after))
      case Star(r1) =>
        for (before, after) <- r1.search(x) yield (r * before, after * r)

    def alphabet: S = r match
      case Zero() | One() => litOps.empty
      case Lit(a) => a
      case Plus(r1, r2) => r1.alphabet | r2.alphabet
      case Comp(r1, r2) => r1.alphabet | r2.alphabet
      case Star(r1) => r1.alphabet

    private inline def mayContain(x: T): Boolean = r.alphabet.contains(x)

    def filterNotContainElem(x: T): RegEx[T, S] =
      if !r.mayContain(x) then r
      else r match
        case Zero() | One() => r
        case Lit(a) => fromCharSet(a - x)
        case Plus(r1, r2) => r1.filterNotContainElem(x) + r2.filterNotContainElem(x)
        case Comp(r1, r2) => r1.filterNotContainElem(x) * r2.filterNotContainElem(x)
        case Star(r1) => r1.filterNotContainElem(x).star

    def absSplitAtFirst(x: T): List[(RegEx[T, S], RegEx[T, S])] =
      for
        (before, after) <- r.search(x)
        before1 = before.filterNotContainElem(x)
        if before1.nonEmpty
      yield (before1, fromChar(x) * after)

    def filterStartsWith(s: List[T]): RegEx[T, S] =
      val r1 = r.deriv(s)
      if r1.isEmpty then Zero() else fromString(s) * r1

    private def filterNotContainDiverge(x: T, xs: List[T]): RegEx[T, S] =
      (if r.nullable then One() else Zero()) +
        (if r.first.contains(x) then fromChar(x) * r.deriv(x).filterNotStartWith(xs).filterNotContainDiverge(x, xs)
        else Zero()) +
        sum(for (a, tail) <- r.absHeadTail if (a - x).nonEmpty
          yield fromCharSet(a - x) * tail.filterNotContainDiverge(x, xs))

    private def filterNotContainMulti(s: List[T]): RegEx[T, S] =
      if !r.mayContain(s) then r
      else r match
        case Plus(r1, r2) => r1.filterNotContainMulti(s) + r2.filterNotContainMulti(s)
        case _ => ArdenSolver.solve(r, notContainSpec(s.head, s.tail))

    def filterNotContain(s: List[T]): RegEx[T, S] = s match
      case Nil => Zero()
      case List(x) => r.filterNotContainElem(x)
      case _ => r.filterNotContainMulti(s)

    def filterNotStartWithElem(x: T): RegEx[T, S] =
      (if r.nullable then One() else Zero()) + sum:
        for
          (a, tail) <- r.absHeadTail
          a1 = a - x
          if a1.nonEmpty
        yield fromCharSet(a1) * tail

    def filterNotStartWith(s: List[T]): RegEx[T, S] = s match
      case Nil => Zero()
      case x :: xs =>
        val tail = r.deriv(x)
        if tail.isEmpty then r // r not start with x, thus the constraint is already met
        else
          (fromChar(x) * tail.filterNotStartWith(xs)) // r starts with x: tail cannot start with xs
            + r.filterNotStartWithElem(x) // r not start with x

    def filterNotEndWith(s: List[T]): RegEx[T, S] = r.reverse.filterNotStartWith(s.reverse).reverse

    @tailrec
    private def filterNotEndWithMany(ss: List[List[T]]): RegEx[T, S] = ss match
      case Nil => r
      case s :: ss =>
        val r1 = r.filterNotEndWith(s)
        if r1.isEmpty then Zero() else r1.filterNotEndWithMany(ss)

    def absSplitAtFirst(s: List[T]): List[(RegEx[T, S], RegEx[T, S])] = s match
      case Nil => List((One(), r))
      case List(x) => r.absSplitAtFirst(x)
      case x :: xs =>
        for
          (before, after) <- r.search(x)
          after1 = after.filterStartsWith(xs)
          if after1.nonEmpty
          before1 = before.filterNotContainMulti(s)
          if before1.nonEmpty
          forbidden = for n <- 1.until(s.length).toList; if s.endsWith(s.take(n)) yield s.dropRight(n)
          before2 = before1.filterNotEndWithMany(forbidden)
          if before2.nonEmpty
        yield (before2, fromChar(x) * after1)

    def absSplitAt(index: Index[T]): List[(RegEx[T, S], RegEx[T, S])] = index match
      case ILeft(n) => r.absSplitAt(n)
      case IRight(n) =>
        if n == 0 then List((r, One()))
        else for (rightRev, leftRev) <- r.reverse.absSplitAt(n) yield (leftRev.reverse, rightRev.reverse)
      case IFirst(s, start, k) =>
        for
          (left, right) <- r.absSplitAt(start)
          (mid, right) <- right.absSplitAtFirst(s)
          split <- shift((left * mid, right), k)
        yield split

    def absElemAt(index: Index[T]): S = index match
      case ILeft(n) => r.absDrop(n).first
      case IRight(n) =>
        require(n > 0, "index out of bounds")
        r.reverse.absDrop(n - 1).first
      case IFirst(s, _, k) =>
        if 0 <= k && k < s.length then litOps.singleton(s(k))
        else litOps.union(r.absSplitAt(index).map(_._2.first))

    def absSliceFromLeft(from: Int, until: Index[T] = IRight(0)): RegEx[T, S] =
      require(from >= 0, s"negative left index: $from")
      sum(for (left, _) <- r.absSplitAt(until) yield left.absDrop(from))

    def absSliceUntilRight(from: Index[T], until: Int = 0): RegEx[T, S] =
      require(until >= 0, s"negative right index: $until")
      sum(for (_, right) <- r.absSplitAt(from)
        yield if until == 0 then right else right.reverse.absDrop(until).reverse)

    // If Last is supported: from First until Last

    def absSliceFromFirst(from: IFirst[T], until: PosIndex[T]): RegEx[T, S] =
      // if from < until, then 1 can be removed
      val r1 = sum:
        for
          (left, _) <- r.absSplitAt(until)
          (mid, _) <- left.absSplitAt(from)
        yield mid
      r1.opt

    // Filters
    def filterElemWith(index: Index[T], f: S => S): RegEx[T, S] =
      sum:
        for
          (left, right) <- r.absSplitAt(index)
          (a, right) <- right.absHeadTail
          a1 = f(a)
          if a1.nonEmpty
        yield left * fromCharSet(a1) * right

    def filterSliceFromLeft(from: Int, until: Index[T], f: RegEx[T, S] => RegEx[T, S]): RegEx[T, S] =
      sum:
        for
          (left, right) <- r.absSplitAt(until)
          (left, mid) <- left.absSplitAt(from)
          mid1 = f(mid)
          if mid1.nonEmpty
        yield left * mid1 * right

    def filterSliceUntilRight(from: Index[T], until: Int, f: RegEx[T, S] => RegEx[T, S]): RegEx[T, S] =
      sum:
        for
          (left, right) <- r.absSplitAt(from)
          (mid, right) <- right.absSplitAt(IRight(until))
          mid1 = f(mid)
          if mid1.nonEmpty
        yield left * mid1 * right

  end extension

  private def notContainSpec[T, S](x: T, xs: List[T])(using LitOps[T, S]): ArdenSolver.Spec[T, S, T, S] = r =>
    (if r.nullable then One() else Zero(),
      (if r.first.contains(x) then List((fromChar(x), r.deriv(x).filterNotStartWith(xs))) else Nil) ++
        (for (a, tail) <- r.absHeadTail; if (a - x).nonEmpty yield (fromCharSet(a - x), tail)))

  private def shift[T, S](split: (RegEx[T, S], RegEx[T, S]), offset: Int)
                         (using LitOps[T, S]): List[(RegEx[T, S], RegEx[T, S])] =
    val (left, right) = split
    if offset == 0 then List(split)
    else if offset > 0 then
      for (mid, right) <- right.absSplitAt(offset) yield (left * mid, right)
    else // offset < 0
      for (left, mid) <- left.absSplitAt(IRight(-offset)) yield (left, mid * right)

  // StartsWith, EndsWith, Contains
  extension [T, S](r: RegEx[T, S])(using litOps: LitOps[T, S])
    private def mayStartWith(s: List[T]): Boolean = r.deriv(s).nonEmpty

    @tailrec
    private def mustStartWith(s: List[T]): Boolean = s match
      case Nil => true
      case x :: xs => (r.first - x).isEmpty && r.deriv(x).mustStartWith(xs)

    def absStartsWith(s: List[T]): BoolSet =
      if r.mayStartWith(s) then
        if r.mustStartWith(s) then BoolSet.True else BoolSet.Full
      else
        BoolSet.False

    def absEndsWith(s: List[T]): BoolSet = r.reverse.absStartsWith(s.reverse)

    private def mustContain(x: T): Boolean = r.filterNotContainElem(x).isEmpty

    def absContainsElem(x: T): BoolSet =
      if r.mayContain(x) then
        if r.mustContain(x) then BoolSet.True else BoolSet.Full
      else
        BoolSet.False

    private def mayContain(s: List[T]): Boolean = s match
      case Nil => true
      case List(x) => r.mayContain(x)
      case x :: xs =>
        val a = r.alphabet
        s.forall(a.contains) && r.search(x).exists(_._2.mayStartWith(xs))

    private def mustContain(s: List[T]): Boolean = s match
      case Nil => true
      case List(x) => r.mustContain(x)
      case _ => r.filterNotContainMulti(s).isEmpty

    def absContains(s: List[T]): BoolSet =
      if r.mayContain(s) then
        if r.mustContain(s) then BoolSet.True else BoolSet.Full
      else
        BoolSet.False

    // More filters
    def filterEndsWith(s: List[T]): RegEx[T, S] = r.reverse.filterStartsWith(s.reverse).reverse

    // NOTE: instead of just finding any occurrence of s, here we find the first occurrence.
    // This is because: (a) the first occurrence must exist when s is contained;
    // (b) the resulting regex is usually simpler, say a* -[contains a]-> a+, rather than a*aa*.
    def filterContains(s: List[T]): RegEx[T, S] = sum(r.absSplitAtFirst(s).map(_ * _))

    private inline def filterIsEmpty: RegEx[T, S] = if r.nullable then One() else Zero()

    private def filterNonEmpty: RegEx[T, S] = r match
      case Zero() | One() => Zero()
      case Lit(a) => if a.isEmpty then Zero() else r
      case Plus(r1, r2) => r1.filterNonEmpty + r2.filterNonEmpty
      case Comp(r1, r2) => r1.filterNonEmpty * r2 + (if r1.nullable then r2.filterNonEmpty else Zero())
      case Star(r1) => r1.filterNonEmpty * r

    def filterEq(s: List[T]): RegEx[T, S] =
      if s.isEmpty then r.filterIsEmpty
      else if r.contains(s) then fromString(s)
      else Zero()

    def filterNe(s: List[T]): RegEx[T, S] =
      val r1 = r.deriv(s)
      if r1.isEmpty then r
      else fromString(s) * r1.filterNonEmpty + r.filterNotStartWith(s)

  end extension

  // Length, Find, Count
  extension [T, S](r: RegEx[T, S])(using LitOps[T, S])
    def absLength: NatSet = r match
      case Zero() | One() => NatSet(0)
      case Lit(a) => if a.isEmpty then NatSet(0) else NatSet(1)
      case Plus(r1, r2) => r1.absLength | r2.absLength
      case Comp(r1, r2) => r1.absLength + r2.absLength
      case Star(_) => NatSet(Set(0), true)

    def absFind(s: List[T], start: Index[T] = ILeft(0)): IndexSet =
      val splits = r.absSplitAt(start)
      val negative = !splits.forall(_._2.mustContain(s))
      val splits1 =
        for
          (left, right) <- splits
          (mid, right) <- right.absSplitAtFirst(s)
        yield (left * mid, right)
      val leftIndices = NatSet.union(splits1.map(_._1.absLength))
      val rightIndices = NatSet.union(splits1.map(_._2.absLength))
      IndexSet(negative, leftIndices, rightIndices)

    private def absCountDiverge(s: List[T]): NatSet =
      (if r.mustContain(s) then NatSet.empty else NatSet(0)) | NatSet.union:
        for (_, right) <- r.absSplitAtFirst(s)
          yield NatSet(1) + right.absDrop(s.length).absCountDiverge(s)

    def absCount(s: List[T]): NatSet = s match
      case Nil => r.absLength + NatSet(1)
      case _ =>
        if !r.mayContain(s) then NatSet(0)
        else ArdenSolver.solve(r, countSpec(s)).toNatSet

    // Filters
    private def constLength: Option[Int] =
      val set = r.absLength
      if !set.infinite && set.nats.size == 1 then Some(set.nats.head) else None

    private def groupByLength(bound: Int): Map[Int, RegEx[T, S]] =
      r.constLength match
        case Some(k) => Map(k -> r)
        case None => r match
          case Zero() => Map.empty
          case One() => Map(0 -> r)
          case Lit(d) => if d.isEmpty then Map.empty else Map(1 -> r)
          case Plus(r1, r2) =>
            val m = mutable.Map.from(r1.groupByLength(bound))
            for (k, rk) <- r2.groupByLength(bound) do
              m(k) = m.getOrElse(k, Zero()) + rk
            m.toMap
          case Comp(r1, r2) =>
            val m1 = r1.groupByLength(bound)
            val m2 = r2.groupByLength(bound)
            val m = mutable.Map.empty[Int, RegEx[T, S]]
            // first part within bound
            for (k1, r1) <- m1; if k1 <= bound do
              for (k2, r2) <- m2 do
                val k = (k1 + k2) min (bound + 1)
                m(k) = m.getOrElse(k, Zero()) + (r1 * r2)
            // first part out of bound
            for r1 <- m1.get(bound + 1) do
              m(bound + 1) = m.getOrElse(bound + 1, Zero()) + (r1 * r2)
            m.toMap
          case Star(r1) =>
            val m1 = r1.groupByLength(bound)
            val ms = ListBuffer.empty[mutable.Map[Int, RegEx[T, S]]]
            for i <- 0 to bound do
              val m = mutable.Map.empty[Int, RegEx[T, S]]
              m(0) = One()
              // first part within bound
              for (k1, r1) <- m1; if k1 <= i do
                for (k2, r2) <- ms(i - k1) do
                  val k = (k1 + k2) min (i + 1)
                  m(k) = m.getOrElse(k, Zero()) + (r1 * r2)
              // first part out of bound
              val rs = (for (k1, r1) <- m1; if k1 > i yield r1).toList
              if rs.nonEmpty then
                m(i + 1) = m.getOrElse(i + 1, Zero()) + (sum(rs) * r)
              ms += m
            ms(bound).toMap

    def filterLengthCmp(op: CmpOp, value: Int): RegEx[T, S] = op match
      case EQ =>
        if value == 0 then r.filterIsEmpty
        else if value > 0 then r.groupByLength(value).getOrElse(value, Zero())
        else Zero()
      case NE =>
        if value == 0 then r.filterNonEmpty
        else if value > 0 then sum(r.groupByLength(value + 1).filter(_._1 != value).values.toList)
        else r
      case LE =>
        if value == 0 then r.filterIsEmpty
        else if value > 0 then sum(r.groupByLength(value).filter(_._1 <= value).values.toList)
        else Zero()
      case LT =>
        if value == 1 then if r.nullable then One() else Zero()
        else if value > 1 then sum(r.groupByLength(value - 1).filter(_._1 < value).values.toList)
        else Zero()
      case GE =>
        if value == 1 then r.filterNonEmpty
        else if value > 1 then sum(r.groupByLength(value).filter(_._1 >= value).values.toList)
        else r
      case GT =>
        if value == 0 then r.filterNonEmpty
        else if value > 0 then sum(r.groupByLength(value + 1).filter(_._1 > value).values.toList)
        else r

    // IndexOf
    private def filterFound(s: List[T], start: Index[T]): RegEx[T, S] =
      sum:
        for
          (left, right) <- r.absSplitAt(start)
          right1 = right.filterContains(s)
        yield left * right1

    private def filterNotFound(s: List[T], start: Index[T]): RegEx[T, S] =
      sum:
        for
          (left, right) <- r.absSplitAt(start)
          right1 = right.filterNotContain(s)
          if right1.nonEmpty
        yield left * right1

    def filterFindCmp(s: List[T], start: Index[T], op: CmpOp, value: Int): RegEx[T, S] =
      if value < -1 then
        op match
          case LE | LT | EQ => Zero()
          case _ => r
      else if value == -1 then
        op match
          case GT | NE => r.filterFound(s, start) // > -1
          case LE | EQ => r.filterNotFound(s, start) // <= -1
          case LT => Zero() // < -1
          case GE => r // >= -1
      else if value == 0 && op == GE then // >= 0
        r.filterFound(s, start)
      else if value == 0 && op == LT then // < 0
        r.filterNotFound(s, start)
      else // general case
        (if op == LE | op == LT | op == NE then r.filterNotFound(s, start) else Zero()) + sum:
          for
            (left, right) <- r.absSplitAt(IFirst(s, start, 0))
            left1 = left.filterLengthCmp(op, value)
            if left1.nonEmpty
          yield left1 * right

    def filterFindCmpRight(s: List[T], start: Index[T], op: CmpOp, value: Int): RegEx[T, S] =
      if value <= 0 then
        op match
          case GE | GE | EQ => Zero() // >= |w|
          case _ => r // < |w|
      else if value == 1 && op == GT then // > |w|
        Zero()
      else if value == 1 && op == LE then // <= |w|
        r
      else // general case
        (if op == LE | op == LT | op == NE then r.filterNotFound(s, start) else Zero()) + sum:
          for
            (left, right) <- r.absSplitAt(IFirst(s, start, 0))
            right1 = right.filterLengthCmp(op.flip, value)
            if right1.nonEmpty
          yield left * right1

    @deprecated("use filterFindLtFind instead")
    def filterIndexOfLt(x1: T, x2: T, start: Int): RegEx[T, S] =
      // Assume both character exist
      val index = IFirst(List(x2), ILeft(start), 0)
      sum:
        for
          (left, right) <- r.absSplitAt(start)
          (mid, right) <- right.absSplitAtFirst(List(x2))
          mid1 = mid.filterContains(List(x1))
        yield left * mid1 * right

    def filterFindLtFind(s: List[T], start: Int, index: IFirst[T]): RegEx[T, S] =
      // Assume both indices exist
      sum:
        for
          (left, right) <- r.absSplitAt(index)
          (left, mid) <- left.absSplitAt(start)
          mid1 = mid.filterContains(s)
        yield left * mid1 * right

  end extension

  private type CountingRE = RegEx[Int, Set[Int]]

  extension (r: CountingRE)
    private def toNatSet: NatSet = r match
      case Zero() | One() => NatSet(0)
      case Lit(_) => NatSet(1)
      case Plus(r1, r2) => r1.toNatSet | r2.toNatSet
      case Comp(r1, r2) => r1.toNatSet + r2.toNatSet
      case Star(r1) => if r1.toNatSet == NatSet(0) then NatSet(0) else NatSet(Set(0), true)

  private def countSpec[T, S](s: List[T])(using LitOps[T, S]): ArdenSolver.Spec[T, S, Int, Set[Int]] = r =>
    // In RegEx[Int, Set[Int]]:
    // r.absCount(s) = (if r.mustContain(s) then Zero else One)
    //               + Σ_{(_, right) <- r.absSplitAtFirst(s)} (⌜{1}⌝ * right.absDrop(|s|).absCount(s))
    (if r.mustContain(s) then Zero() else One(),
      for (_, right) <- r.absSplitAtFirst(s) yield (Lit(Set(1)), right.absDrop(s.length)))

  // Split
  extension [T, S](r: RegEx[T, S])(using LitOps[T, S])
    private def absSplitDiverge(s: List[T]): RegEx[List[T], RegEx[T, S]] =
      val r1 = r.filterNotContain(s)
      (if r1.isEmpty then Zero() else Lit(r1)) + sum:
        for (left, right) <- r.absSplitAtFirst(s)
          yield Lit(left) * right.absDrop(s.length).absSplitDiverge(s)

    def absSplit(s: List[T]): RegEx[List[T], RegEx[T, S]] = s match
      case Nil =>
        throw new IllegalArgumentException("empty seperator")
      case _ =>
        ArdenSolver.solve(r, splitSpec(s))

  end extension

  private def splitSpec[T, S](s: List[T])(using LitOps[T, S]): ArdenSolver.Spec[T, S, List[T], RegEx[T, S]] = r =>
    val r1 = r.filterNotContain(s)
    (if r1.isEmpty then Zero() else Lit(r1),
      for (left, right) <- r.absSplitAtFirst(s) yield (Lit(left), right.absDrop(s.length)))

  extension [T, S](r: RegEx[T, S])(using litOps: LitOps[T, S])
    def absForall(f: S => Boolean): Boolean = r match
      case Zero() | One() => true
      case Lit(a) => f(a)
      case Plus(r1, r2) => r1.absForall(f) && r2.absForall(f)
      case Comp(r1, r2) => r1.absForall(f) && r2.absForall(f)
      case Star(r1) => r1.absForall(f)

    def absExists(f: S => Boolean): Boolean = r match
      case Zero() | One() => false
      case Lit(a) => f(a)
      case Plus(r1, r2) => r1.absExists(f) || r2.absExists(f)
      case Comp(r1, r2) => r1.absExists(f) || r2.absExists(f)
      case Star(r1) => r1.absExists(f)

    def absMap(f: S => S): RegEx[T, S] = r match
      case Zero() | One() => r
      case Lit(a) => fromCharSet((f(a)))
      case Plus(r1, r2) => r1.absMap(f) + r2.absMap(f)
      case Comp(r1, r2) => r1.absMap(f) * r2.absMap(f)
      case Star(r1) => r1.absMap(f).star

    def filterForall(f: S => S): RegEx[T, S] = r match
      case Zero() | One() => r
      case Lit(d) => fromCharSet(f(d))
      case Plus(r1, r2) => r1.filterForall(f) + r2.filterForall(f)
      case Comp(r1, r2) => r1.filterForall(f) * r2.filterForall(f)
      case Star(r1) => r1.filterForall(f).star

    def filterExists(f: S => S): RegEx[T, S] = r match
      case Zero() | One() => Zero()
      case Lit(d) => fromCharSet(f(d))
      case Plus(r1, r2) => r1.filterExists(f) + r2.filterExists(f)
      case Comp(r1, r2) => r1.filterExists(f) * r2 + r1 * r2.filterExists(f)
      case Star(r1) => r * r1.filterExists(f) * r
