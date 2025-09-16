package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.RegEx.*

import scala.collection.mutable.ListBuffer

class HintSynth(using ctx: PrfCtx, config: Config) extends LazyLogging:
  private val hints = ListBuffer.empty[Expr]

  private val types = ctx.types

  private val inferer = new Inferer

  private val smtSolver = new SMTSolver

  def collectHints(seed: Expr): List[Expr] =
    val temps = seed.collect {
      case e@Length(_) => HLength(e)
      case e@CharAt(_, _) => HChars(e)
      case e@Find(_, _) => HFind(e)
      case e@InfixOf(_, _) => HTest(e)
      case Cmp(EQ | NE, e, Const(s: String)) => HEq(e, s)
      case e@Var(_) if ctx.exists {
        case Cmp(_, CharAt(_, ei), _) => ei == e
        case Cmp(_, ei, Find(_, _)) => ei == e
      } => HIndex(e)
    }.distinct
    hints.clear()
    for temp <- temps do temp()
    hints.toList.distinct

  sealed trait HintTemp extends LazyLogging:
    def apply(): Unit

  final case class HLength(expr: Length) extends HintTemp:
    def apply(): Unit =
      // Optional attempt
      for
        (eb, r) <- ctx.lookupSuffixLang(expr.str)
        if smtSolver.canProve(mkAnd(GE(eb, 0), LT(eb, expr)))
      do
        val r1 = RERefiner.refineByLen(r, (GT, 0))
        exprIn(SUB(expr, eb), r1.length)
      // Ordinary attempt
      val r = inferer.inferLang(expr.str)
      exprIn(expr, r.length)

  private def exprIn(expr: Expr, interval: Interval): Unit = interval match
    case Interval(n1, n2: Int) if n1 == n2 => hints += EQ(expr, n1)
    case Interval(n1, n2: Int) => hints += mkAnd(GE(expr, n1), LE(expr, n2))
    case Interval(n1, _) => hints += GE(expr, n1)

  final case class HIndex(expr: Var) extends HintTemp:
    def apply(): Unit =
      ctx.foreach {
        case Cmp(EQ, CharAt(es, ei@Var(_)), Const(s: String)) if s.length == 1 =>
          val r = inferer.inferLang(es)
          val c = s.head
          val r1 = r.findPrefix(c)
          if r1 != RENone then
            val k1 = r1.length.lb
            hints += GE(ei, k1)
            val r2 = r.reverse.findSuffix(c)
            val k2 = r2.length.lb
            hints += LT(ei, SUB(Length(es), k2))
        case Cmp(NE, CharAt(es, ei@Var(_)), Const(s: String)) if s.length == 1 =>
          val r = inferer.inferLang(es)
          val c = s.head
          r match
            case REConcat(REStar(RELit(cs)), r1) if cs.isSingletonOf(c) =>
              r1.length.ub match
                case k1: Int => hints += GE(ei, SUB(Length(es), k1))
                case _ =>
            case _ =>
        case Cmp(_, e1, Find(es, Const(s: String))) if e1 == expr && s.length == 1 && ctx.exists(_ == InfixOf(s, es)) =>
          val c = s.head
          ctx.foreach:
            case InfixOf(Const(s1: String), e) if e == es && s1.length == 1 && s1.head != c =>
              val c1 = s1.head
              val r = inferer.inferLang(es)
              if firstOccurBefore(r, c, c1) then
                hints += LT(Find(es, c.toString), Find(es, c1.toString))
              else if firstOccurBefore(r, c1, c) then
                hints += LT(Find(es, c1.toString), Find(es, c.toString))
            case _ =>
        case _ =>
      }

  private def firstOccurBefore(r: RegEx, c1: Char, c2: Char): Boolean = !r.findPrefix(c1).alphabet.contains(c2)

  final case class HChars(expr: CharAt) extends HintTemp:
    def apply(): Unit =
      inferer.inferLang(expr) match
        case RELit(cs) if cs.polarity =>
          val cases = for c <- cs.chars yield EQ(expr, c.toString)
          hints += mkOr(cases.toList)
        case _ => throw InternalError()

  extension (index: Index)
    private def concretize(str: Expr): Expr = index match
      case IndexL(k) => k
      case IndexR(k) => SUB(Length(str), k)
      case IndexAt(t) => Find(str, t)
      case IndexShifted(b, k) => ADD(b.concretize(str), k)
      case IndexInterval(lb, ub) => throw IllegalArgumentException(s"cannot concretize $index")

    /** Generates a Boolean expression that encodes the concrete `idx` (of `str`) is in this abstract `index`. */
    def constraint(idx: Expr, str: Expr): Expr = index match
      case _: BasicIndex | IndexShifted => EQ(idx, concretize(str))
      case IndexInterval(lb, ub) => mkAnd(GE(idx, lb.concretize(str)), LE(idx, ub.concretize(str)))

  final case class HFind(expr: Find) extends HintTemp:
    def apply(): Unit =
      val (found, results) = inferer.inferFind(expr)
      found match
        case BoolSet.True => for index <- results do hints += index.constraint(expr, expr.str)
        case BoolSet.False => hints += EQ(expr, -1)
        case BoolSet.All => for index <- results do hints += mkOr(index.constraint(expr, expr.str), EQ(expr, -1))

  final case class HEq(str: Expr, target: String) extends HintTemp:
    def apply(): Unit =
      val r = inferer.inferLang(str)
      for ss <- AOps.tryEnumerate(r) do
        val cases = for s <- ss yield EQ(str, s)
        hints += mkOr(cases.toList)
      r match
        case RELit(cs) if !cs.polarity =>
          for c <- cs.chars do
            hints += NE(str, c.toString)
        case _ =>

  final case class HTest(expr: InfixOf) extends HintTemp:
    def apply(): Unit =
      inferer.inferTest(expr) match
        case BoolSet.True => hints += expr
        case BoolSet.False => hints += Not(expr)
        case _ =>

/*  def inferLang(str: Expr): RegEx = str match
    case Const(s: String) => RegEx.fromString(s)
    case Var(_) => ctx.getLang(str)
    case StrConcat(es1, es2) => inferLang(es1) ++ inferLang(es2)
    case StrRev(es) => inferLang(es).reverse
    case StrAt(es, ei) =>
      val r = inferLang(es)
      val i = indexInferer.infer(ei, es)
      substr(r, i, AIndexR(0)).take1
//      val attempt = for
//        (from, r) <- extractSuffixLang(es)
//        k <- Rewriter.tryGetConstDiff(ei, from)
//        if k >= 0
//      // r = langRefiner.refineSuffix(es, from, r0)
//      yield slice(r, IndexAt(null, L, k)).first
//      // normal attempt
//      val r = inferLang(es)
//      var cs = indexSolver.solve(ei, es) match
//        case IndexAt(_, L, k) => AOps.charAt(r, k)
//        case IndexAt(_, R, k) => AOps.charAt(r.reverse, k - 1)
//        case i: Index => slice(r, i).first
//        case IndexRange(i1: Index, i2: Index) =>
//          logger.debug(s"slice $r from $i1 to $i2")
//          slice(r, i1, i2 + 1).alphabet
//      if cs.isEmpty then cs = r.alphabet
//      ctx.foreach {
//        case Cmp(NE, StrAt(e1, e2), Const(s: String)) if e1 == es && e2 == ei && s.length == 1 =>
//          // TODO: is it possible to direct report inconsistency at this phase?
//          if !cs.isSingleton then cs = cs - s.head
//        case _ =>
//      }
//      // pick the more precise one
//      attempt match
//        case Some(cs1) if cs1.subsetOf(cs) =>
//          logger.debug(s"  infer $str ∈ $cs1 by suffix")
//          RELit(cs1)
//        case _ => RELit(cs)
    case StrSlice(es, ei, ej) =>
      logger.debug(s"infer $es[$ei:$ej]")
      tryAll(
        () =>
          if smtSolver.canProve(GE(ei, StrLen(es))) then Some(RENull)
          else None,
        () => for
          (from, r) <- extractSuffixLang(es)
          k <- Rewriter.tryGetConstDiff(ei, from)
          if k >= 0 && ej == StrLen(es)
        // r = langRefiner.refineSuffix(es, from, r0)
        yield
          val r1 = slice(r, IndexAt(null, L, k))
          r1,
        () => for
          (from, r) <- extractSuffixLang(es)
          ki <- Rewriter.tryGetConstDiff(ei, from)
          if ki >= 0
          kj <- Rewriter.tryGetConstDiff(ej, from)
          if kj >= 0
        yield
          val r1 = slice(r, IndexAt(null, L, ki), IndexAt(null, L, kj))
          logger.debug(s"$es[$ei:$ej] : $r1")
          r1,
      ).getOrElse {
        val r = inferLang(es)
        ej match
          case Arith(ADD, StrFind(StrSlice(e1, e2, e3), Const(s: String)), e4)
            if e1 == es && e2 == ei && e3 == StrLen(es) && s.length == 1 && e4 == ei =>
            indexSolver.solve(ei, es) match
              case i1: Index =>
                val r1 = slice(r, i1)
                val ((r2, _), _) = AOps.splitAtIndexOf(r1, s.head)
                return r2
              case _ =>
          case _ =>

        (indexSolver.solve(ei, es), indexSolver.solve(ej, es)) match
          case (i1: Index, i2: Index) =>
            val r1 = slice(r, i1, i2)
            logger.debug(s"$es[$ei:$ej] : $r1")
            r1
          case (IndexRange(i1, i2), IndexAt(_, R, 0)) =>
            val cs = slice(r, i1, i2 + 1).alphabet
            val r2 = slice(r, i2 + 1, IndexAt(es, R, 0))
            val r1 = i1 match
              case IndexAt(_, L, _) => RELit(cs).+
              case _ => throw UnsupportedOperationException(i1.toString)
            logger.debug(s"$es[$ei:$ej] : $r1$r2")
            concat(r1, r2)
          case (i1, i2) => throw UnsupportedOperationException(s"slice $i1 until $i2")
      }
    case _ => throw IllegalArgumentException(s"not a str-sorted expression: $str")

  private def extractSuffixLang(str: Expr): Option[(Expr, RegEx)] =
    ctx.collectFirst {
      case TypeTest(suffix@StrSlice(e1, ei, StrLen(e2)), LangType(_)) if e1 == str && e2 == str =>
        (ei, ctx.getLang(suffix))
    }

  private def slice(re: RegEx, fromIndex: Index): RegEx = slice(re, fromIndex, IndexAt(null, R, 0))

  private def slice(re: RegEx, fromIndex: Index, untilIndex: Index): RegEx =
    // TODO: check empty slice first?
    fromIndex match
      case IndexAt(_, d1, i1) =>
        val ra = d1 match
          case L => re.drop(i1)
          case R => AOps.takeRight(re, i1)
        untilIndex match
          case IndexAt(_, L, i2) =>
            require(d1 == L)
            ra.take((i2 - i1) max 0)
          case IndexAt(_, R, i2) =>
            AOps.dropRight(ra, i2)
          case IndexOf(e, c, k) =>
            val r0 = inferLang(e)
            require(r0 == re || r0 == ra)
            val ((ral, rar), raNot) = AOps.splitAtIndexOf(ra, c)
            // assert(raNot == ReNone)
            val (r, _) = AOps.shift(ral, rar, k)
            val dropped =
              if r0 == re then d1 match
                case L => re.take(i1).alphabet
                case R => AOps.dropRight(re, i1).alphabet
              else CharSet.empty
            if dropped.contains(c) then r.unionNull else r
      case IndexOf(es1, c1, k1) =>
        val r1 = inferLang(es1)
        require(r1 == re)
        untilIndex match
          case IndexAt(_, d2, i2) =>
            val ra = d2 match
              case L => re.take(i2)
              case R => AOps.dropRight(re, i2)
            val ((ral, rar), raNot) = AOps.splitAtIndexOf(ra, c1)
            // assert(raNot == ReNone, raNot.toString)
            val (_, r) = AOps.shift(ral, rar, k1)
            val dropped = d2 match
              case L => AOps.dropRight(re, i2).alphabet
              case R => re.take(i2).alphabet
            if dropped.contains(c1) then r.unionNull else r
          case IndexOf(es2, c2, k2) =>
            val r2 = inferLang(es2)
            val ((rel, rer), reNot) = AOps.splitAtIndexOf(re, c1)
            // assert(reNot == RENone, reNot.toString)
            val (rDrop, ra) = AOps.shift(rel, rer, k1)
            require(r2 == ra || r2 == re)
            val ((ral, rar), raNot) = AOps.splitAtIndexOf(ra, c2)
            // assert(raNot == RENone, raNot.toString)
            val (r, _) = AOps.shift(ral, rar, k2)
            val dropped = if r2 == re then rDrop.alphabet else CharSet.empty
            if dropped.contains(c2) then r.unionNull else r

  def inferIndexOf(expr: StrFind): Option[Index | (Index, Index)] = expr match
    case StrFind(es, Const(s: String)) if s.length == 1 =>
      val r = inferLang(es)
      val ((r1, r2), rNot) = AOps.splitAtIndexOf(r, s.head)
      if rNot != RENone && r1 == RENone && r2 == RENone then
        return Some(IndexAt(expr.str, R, 0))
      val k1 = r1.length.lower
      if r1.length.upper.contains(k1) && rNot == RENone then
        return Some(IndexAt(expr.str, L, k1))
      val k2 = r2.length.lower
      if r2.length.upper.contains(k2) && rNot == RENone then
        return Some(IndexAt(expr.str, R, k2))
      r1.length.upper match
        case Some(k3) => Some((IndexAt(expr.str, L, k1), IndexAt(expr.str, L, k3)))
        case None => Some((IndexAt(expr.str, L, k1), IndexAt(expr.str, R, 1)))
    case _ => None
*/