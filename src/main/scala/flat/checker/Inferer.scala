package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.Printer.ppExpr
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Boolean Set. */
enum BoolSet:
  /** The singleton set {true}. */
  case True
  /** The singleton set {false}. */
  case False
  /** The full set {true, false}. */
  case All

/** Type Inference. */
class Inferer(using config: Config, ctx: PrfCtx) extends LazyLogging:
  private val indexInferer = new IndexInferer

  /** Infer the regular language of a given `str`. */
  def inferLang(str: Expr): RegEx =
    str match
      case Const(s: String) => RegEx.fromString(s)
      case Var(x) => ctx.getLang(str)
      case Concat(es1, es2) => inferLang(es1) ++ inferLang(es2)
      case Reverse(es) => inferLang(es).reverse
      case CharAt(es, ei) =>
        // es[ei] = es[ei:].take1
        val (rOpt, rOrd) = inferSubstr(es, ei, Length(es))(using ctx + LT(ei, Length(es)))
        val cs = rOpt match
          case Some(r) => // select the more premise one
            val cs1 = r.first
            val cs2 = rOrd.first
            if cs2.subsetOf(cs1) then cs2 else cs1
          case None => rOrd.first
        if cs.isEmpty then RegEx.RENull else RegEx.fromCharSet(cs)
      case Substr(es, ei, Arith(ADD, Find(Substr(e1, e2, Length(e3)), Const(t: String)), e4))
        if e1 == es && e2 == ei && e3 == es && e4 == ei =>
        // Special case: es[ei : (es[ei:].find(t) + ei)] = es[ei : es.find(t, ei)]
        val r = inferLang(es)
        val i = indexInferer.infer(ei, es)
        val r1 = substr(r, i, IndexR(0), startIdx = Some(ei))(using es)
        r1.take(IndexAt(t))
      case Substr(es, ei, ej) =>
        inferSubstr(es, ei, ej) match
          case (Some(r1), r2) => // select the more precise one; if neither is a subset of another, prefer r1
            if r2.subsetOf(r1) then r2 else r1
          case (None, r) => r
      case _ => throw IllegalArgumentException(str.toString)

  /** Infer the type of `Substr(str, start, end)`.
   * Return two solutions: one using the suffix lang (if specified), and the other using the ordinary method. */
  private def inferSubstr(str: Expr, start: Expr, end: Expr)(using ctx: PrfCtx): (Option[RegEx], RegEx) =
    // Optional attempt: Given that the language of `str[base:]` (for some `base` index) is `r`,
    // and that `start` equals to `base + ki` for some constant ki >= 0.
    // - If `end` is the end index, then it suffices to infer `r[ki:]`;
    // - If `end` equals to `base + kj` for some constant kj >= 0, then it suffices to infer `r[ki:kj]`.
    val rOpt = for
      (base, r) <- ctx.lookupSuffixLang(str)
      ki <- start.diffNonneg(base)
      j <- if end == Length(str) then Some(IndexR(0)) else end.diffNonneg(base).map(IndexL(_))
      r1 = substr(r, IndexL(ki), j)(using str = Substr(str, base, Length(str)))
      _ = logger.debug("infer {}[{}:{}] as {}[{}:][{}:{}]: {}",
        ppExpr(str), ppExpr(start), ppExpr(end), ppExpr(str), ppExpr(base), ki, j, r1)
    yield r1

    // Ordinary attempt in a compositional manner.
    val r = inferLang(str)
    val i = indexInferer.infer(start, str)
    val j = indexInferer.infer(end, str)
    val rOrd = substr(r, i, j, startIdx = Some(start), endIdx = Some(end))(using str = str)

    // Return both results.
    (rOpt, rOrd)

  private def substr(r: RegEx, startIndex: Index, endIndex: Index,
                     startIdx: Option[Expr] = None, endIdx: Option[Expr] = None)(using str: Expr, ctx: PrfCtx): RegEx =
    (startIndex, endIndex) match
      case (IndexL(0), IndexR(0)) => r
      case (i: BasicIndex, IndexR(0)) => r.drop(i)
      case (IndexL(0), j: BasicIndex) => r.take(j)
      case (IndexL(i), IndexL(j)) => r.take(j).drop(i)
      case (IndexR(_), IndexL(_)) => throw UnsupportedOperationException(s"substr from $startIndex until $endIndex")
      case (IndexAt(t), IndexL(j)) =>
        // Require: startIndex + |t| < endIndex
        assert(isValid(LT(ADD(startIndex.concretize(str), t.length), endIndex.concretize(str))))
        r.take(j).drop(IndexAt(t))
      case (i: BasicIndex, j: BasicIndex) =>
        // Require: startIndex < endIndex
        assert(isValid(LT(startIndex.concretize(str), endIndex.concretize(str))))
        r.drop(i).take(j)
      case (IndexShifted(i, k), j) if k < 0 =>
        // Require: -k ≤ i ∧ i ≤ j ≤ length s
        assert(isValid(mkAnd(
          LE(-k, i.concretize(str)),
          LE(i.concretize(str), j.concretize(str)),
          LE(j.concretize(str), Length(str)))))
        // s[i + (-k) : j] = (reverse (reverse s[:i])[:(-k)]) ++ s[i:j]
        r.take(i).reverse.take(-k).reverse ++ substr(r, i, j, endIdx = endIdx)
      case (IndexShifted(i, k), j) if k > 0 =>
        // s[i + k : j] = s[i:j][k:]
        substr(r, i, j, endIdx = endIdx).drop(k)
      case (i, IndexShifted(j, k)) if k < 0 =>
        // Require: i ≤ j - (-k) ∧ j ≤ length s
        assert(isValid(And(
          LE(i.concretize(str), SUB(j.concretize(str), -k)),
          LE(j.concretize(str), Length(str)))))
        // s[i : j - k] = reverse (reverse s[i:j])[k:]
        substr(r, i, j, startIdx = startIdx).reverse.drop(-k).reverse
      case (i, IndexShifted(j, k)) if k > 0 =>
        // Require: i ≤ j
        assert(isValid(LE(i.concretize(str), j.concretize(str))))
        // s[i : j + k] = s[i:j] ++ s[j:][:k]
        substr(r, i, j, startIdx = startIdx) ++ r.drop(j).take(k)
      case (IndexInterval(i1, i2), j: BasicIndex) =>
        val rc = RegEx.fromCharSet(substr(r, i1, i2.shift(1)).alphabet)
        val r1 = if isValid(LT(startIdx.get, Length(str))) then rc.+ else rc.*
        val r2 = substr(r, i2.shift(1), j)
        logger.debug(s"infer substr of $r from $startIndex until $endIndex: $r1 ++ $r2")
        r1 ++ r2
      case (i: BasicIndex, IndexInterval(j1, j2)) =>
        val r1 = substr(r, i, j1)
        val rc = RegEx.fromCharSet(substr(r, j1, j2.shift(1)).alphabet)
        val r2 = if isValid(LT(endIdx.get, Length(str))) then rc.+ else rc.*
        logger.debug(s"infer substr of $r from $startIndex until $endIndex: $r1 ++ $r2")
        r1 ++ r2
      case (IndexInterval(i, _), IndexInterval(_, j)) =>
        val rc = RegEx.fromCharSet(substr(r, i, j).alphabet)
        val r1 = if isValid(LT(startIdx.get, endIdx.get)) then rc.+ else rc.*
        logger.debug(s"infer substr of $r from $startIndex until $endIndex: $r1")
        r1

  /** Infer the result of a given string `test`. */
  def inferTest(test: StrTest): BoolSet = test match
    case PrefixOf(Const(t: String), es) => prefixOf(t, es)
    case PrefixOf(_, _) => BoolSet.All
    case SuffixOf(Const(t: String), es) => prefixOf(t.reverse, Reverse(es))
    case SuffixOf(_, _) => BoolSet.All
    case InfixOf(Const(t: String), es) => infixOf(t, es)
    case InfixOf(_, _) => BoolSet.All

  private def prefixOf(t: String, str: Expr): BoolSet =
    if t.isEmpty then
      return BoolSet.True
    val r = inferLang(str)
    if r.derivative(t).isEmpty then BoolSet.False
    else if r.forallPrefix(t) then BoolSet.True
    else BoolSet.All

  private def infixOf(t: String, str: Expr): BoolSet = t.length match
    case 0 => BoolSet.True
    case 1 =>
      val c = t.head
      val r = inferLang(str)
      if r.forallContains(c) then BoolSet.True
      else if !r.alphabet.contains(c) then BoolSet.False
      else BoolSet.All
    case _ =>
      // TODO: why not just forallInfix?
      val r = inferLang(str)
      val r1 = r.splitSuffix(t.head)
      if r1.derivative(t).isEmpty then BoolSet.False
      else if r.forallContains(t.head) && r1.forallPrefix(t) then BoolSet.True
      else BoolSet.All

  /** Infer the length interval of a given `str`. */
  def inferLength(str: Expr): Interval =
    val r = inferLang(str)
    r.length

  /** Infer the result of `Find(str, pat)`.
   * Returns a pair of:
   *  - a Boolean set that indicates if the finding succeeds; and
   *  - an semantically equivalent set of indices of the first occurrence. */
  def inferFind(str: Expr, pat: String): (BoolSet, List[Index]) =
    infixOf(pat, str) match
      case BoolSet.False => (BoolSet.False, Nil)
      case bs =>
        val r = inferLang(str)
        val results = ListBuffer.empty[Index]
        r.take(IndexAt(pat)).length match
          case Interval(n1: Int, n2: Int) =>
            results += (if n1 == n2 then IndexL(n1) else IndexInterval(IndexL(n1), IndexL(n2)))
          case Interval(n1: Int, Inf) =>
            results += IndexInterval(IndexL(n1), IndexR(1))
          case _ => assert(false)
        r.drop(IndexAt(pat)).length match
          case Interval(n1: Int, n2: Int) =>
            results += (if n1 == n2 then IndexR(n1) else IndexInterval(IndexR(n2), IndexR(n1)))
          case Interval(n1: Int, Inf) =>
            results += IndexInterval(IndexL(0), IndexR(n1))
          case _ => assert(false)
        (bs, results.distinct.toList)

  private inline def isValid(cond: Expr)(using ctx: PrfCtx): Boolean = ctx.isValid(cond)
