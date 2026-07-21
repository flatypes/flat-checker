package flat.regex

import flat.regex.REOps.filterNotContain
import flat.regex.RegEx.*

import scala.annotation.tailrec

enum NatSet extends Domain:
  case Concrete(elems: Set[Int])
  case From(start: Int)

  def |(that: NatSet): NatSet = (this, that) match
    case (Concrete(s1), Concrete(s2)) => Concrete(s1 | s2)
    case (From(n1), From(n2)) => From(n1 min n2)
    case (Concrete(s), From(n)) => if s.isEmpty then From(n) else From(s.min min n)
    case (From(n), Concrete(s)) => if s.isEmpty then From(n) else From(s.min min n)

  def +(that: NatSet): NatSet = (this, that) match
    case (Concrete(s1), Concrete(s2)) => Concrete(for n1 <- s1; n2 <- s2 yield n1 + n2)
    case (From(n1), From(n2)) => From(n1 + n2)
    case (Concrete(s), From(n)) => if s.isEmpty then From(n) else From(s.min + n)
    case (From(n), Concrete(s)) => if s.isEmpty then From(n) else From(s.min + n)

object NatSet:
  def union(sets: List[NatSet]): NatSet =
    if sets.isEmpty then Set.empty else sets.reduce(_ | _)

case class IndexSet(negative: Boolean, nonneg: NatSet) extends Domain

given Conversion[Set[Int], NatSet] = NatSet.Concrete(_)

enum BoolSet extends Domain:
  case Top, True, False, Bot

object REAbsOps:
  extension (regEx: RegEx)
    def absLength: NatSet = regEx match
      case RENone => Set.empty
      case RENull => Set(0)
      case RELit(_) => Set(1)
      case REConcat(r1, r2) => r1.absLength + r2.absLength
      case REUnion(r1, r2) => r1.absLength | r2.absLength
      case REStar(_) => NatSet.From(0)

    // slice
    def absDrop1: RegEx = regEx match
      case RENone | RENull | RELit(_) => RENull
      case REConcat(r1, r2) => if r1.nullable then (r1.absDrop1 ++ r2) | r2.absDrop1 else r1.absDrop1 ++ r2
      case REUnion(r1, r2) => r1.absDrop1 | r2.absDrop1
      case REStar(r) => RENull | (r.absDrop1 ++ regEx)

    @tailrec
    def absDrop(n: Int): RegEx =
      require(n >= 0)
      if n == 0 then regEx else regEx.absDrop1.absDrop(n - 1)

    def absDropRight(n: Int): RegEx = regEx.reverse.absDrop(n).reverse

    def absCharAt(i: Int): CharSet =
      require(i >= 0)
      absDrop(i).first

    def absSplitAt0: List[(CharSet, RegEx)] = regEx match
      case RENone | RENull => Nil
      case RELit(cs) => List((cs, RENull))
      case REConcat(r1, r2) =>
        (for (cs, r) <- r1.absSplitAt0 yield (cs, r ++ r2)) ++
          (if r1.nullable then r2.absSplitAt0 else Nil)
      case REUnion(r1, r2) => r1.absSplitAt0 ++ r2.absSplitAt0
      case REStar(r) => for (cs, r1) <- r.absSplitAt0 yield (cs, r1 ++ regEx)

    def absTake(n: Int): RegEx =
      require(n >= 0)
      if n == 0 then RENull
      else
        val rs = for (cs, r) <- regEx.absSplitAt0 yield RELit(cs) ++ r.absTake(n - 1)
        (if regEx.nullable then RENull else RENone) | RegEx.union(rs)

    def absTakeRight(n: Int): RegEx = regEx.reverse.absTake(n).reverse

    // find
    def forallContain(c: Char): Boolean = regEx match
      case RELit(s) => s.isSingleton && s.contains(c)
      case REConcat(r1, r2) => r1.forallContain(c) || r2.forallContain(c)
      case REUnion(r1, r2) => r1.forallContain(c) && r2.forallContain(c)
      case _ => false

    def existsContain(c: Char): Boolean = regEx match
      case RELit(s) => s.contains(c)
      case REConcat(r1, r2) => r1.existsContain(c) || r2.existsContain(c)
      case REUnion(r1, r2) => r1.existsContain(c) || r2.existsContain(c)
      case REStar(r) => r.existsContain(c)
      case _ => false

    def absContains(c: Char): BoolSet =
      if forallContain(c) then BoolSet.True
      else if !existsContain(c) then BoolSet.False
      else BoolSet.Top

    def splitBy(c: Char): List[(RegEx, RegEx)] = regEx match
      case RENone => Nil
      case RENull => Nil
      case RELit(cs) => if cs.contains(c) then List((RENull, RENull)) else Nil
      case REConcat(r1, r2) =>
        (for (r1l, r1r) <- r1.splitBy(c) yield (r1l, r1r ++ r2)) ++
          (for (r2l, r2r) <- r2.splitBy(c) yield (r1 ++ r2l, r2r))
      case REUnion(r1, r2) => r1.splitBy(c) ++ r2.splitBy(c)
      case REStar(r) => for (rl, rr) <- r.splitBy(c) yield (regEx ++ rl, rr ++ regEx)

    def absSplitAtIndexOf(c: Char): List[(RegEx, RegEx)] =
      for
        (r1, r2) <- regEx.splitBy(c)
        r = r1.filterNotContain(c)
        if !r.isEmpty
      yield (r, r2)

    def absIndexOf(c: Char): IndexSet =
      val sets = for (r1, _) <- regEx.absSplitAtIndexOf(c) yield r1.absLength
      IndexSet(!forallContain(c), NatSet.union(sets))

    // prefix
    def absStartsWith(t: String): BoolSet =
      if regEx.absTake(t.length) equiv RegEx.fromString(t) then BoolSet.True
      else if regEx.deriv(t).isEmpty then BoolSet.False
      else BoolSet.Top

    // others
    def absCount(c: Char): NatSet = regEx match
      case RENone => Set.empty
      case RENull => Set(0)
      case RELit(s) =>
        if s.isSingleton && s.contains(c) then Set(1)
        else if !s.contains(c) then Set(0)
        else Set(0, 1)
      case REConcat(r1, r2) => r1.absCount(c) + r2.absCount(c)
      case REUnion(r1, r2) => r1.absCount(c) | r2.absCount(c)
      case REStar(r) => if r.existsContain(c) then NatSet.From(0) else Set(0)

    def absTrim: RegEx = ???

    def absToLower: RegEx = regEx match
      case RENone => RENone
      case RENull => RENull
      case RELit(s) => RELit(CharSetAbs.toLower(s))
      case REConcat(r1, r2) => r1.absToLower ++ r2.absToLower
      case REUnion(r1, r2) => r1.absToLower | r2.absToLower
      case REStar(r) => r.absToLower.*

    def absToUpper: RegEx = regEx match
      case RENone => RENone
      case RENull => RENull
      case RELit(s) => RELit(CharSetAbs.toUpper(s))
      case REConcat(r1, r2) => r1.absToUpper ++ r2.absToUpper
      case REUnion(r1, r2) => r1.absToUpper | r2.absToUpper
      case REStar(r) => r.absToUpper.*

    def absToInt: Interval = ???
