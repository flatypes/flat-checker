package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.core.*
import flat.checker.core.ArithOp.ADD
import flat.checker.summands
import flat.regex.*
import flat.regex.AOps.*

import scala.collection.mutable.ListBuffer

enum BoolSet:
  case True
  case False
  case All

class Inferer(using ctx: PrfCtx, config: Config) extends LazyLogging:
  private val indexInferer = new IndexInferer

  def inferLang(str: Expr): RegEx = str match
    case Const(s: String) => RegEx.fromString(s)
    case Var(x) => ctx.getLang(str)
    case Concat(es1, es2) => inferLang(es1) ++ inferLang(es2)
    case Reverse(es) => inferLang(es).reverse
    case CharAt(es, ei) =>
      // es[ei] = es[ei:].first
      val (rOpt, rOrd) = inferSubstr(es, ei, Length(es))
      val cs = rOpt match
        case Some(r) => // select the more premise one
          val cs1 = r.first
          val cs2 = rOrd.first
          if cs1.subsetOf(cs2) then
            logger.debug(s"infer $es[$ei]: prefer $cs1 than $cs2")
            cs1
          else
            logger.debug(s"infer $es[$ei]: prefer $cs2 than $cs1")
            cs2
        case None => rOrd.first
      RegEx.fromCharSet(cs)
    case Substr(es, ei, Arith(ADD, Find(Substr(e1, e2, Length(e3)), Const(t: String)), e4))
      if e1 == es && e2 == ei && e3 == es && e4 == ei =>
      // Special case: es[ei : (es[ei:].find(t) + ei)] = es[ei : es.find(t, ei)]
      val r = inferLang(es)
      val i = indexInferer.infer(ei, es)
      val r1 = substr(r, i, IndexR(0))
      r1.take(IndexAt(t))
    case Substr(es, ei, ej) =>
      val (rOpt, rOrd) = inferSubstr(es, ei, ej)
      rOpt.getOrElse(rOrd)
    case _ => throw IllegalArgumentException()

  private def inferSubstr(str: Expr, start: Expr, end: Expr): (Option[RegEx], RegEx) =
    // Optional attempt: Given that the language of `str[base:]` (for some `base` index) is `r`,
    // and that `start` equals to `base + ki` for some constant ki >= 0.
    // - If `end` is the end index, then it suffices to infer `r[ki:]`;
    // - If `end` equals to `base + kj` for some constant kj >= 0, then it suffices to infer `r[ki:kj]`.
    val rOpt = for
      (base, r) <- ctx.lookupSuffixLang(str)
      ki <- computeConstOffset(start, base)
      j <- if end == Length(str) then Some(IndexR(0)) else computeConstOffset(end, base).map(IndexR(_))
    yield substr(r, IndexL(ki), j)

    // Ordinary attempt in a compositional manner.
    val r = inferLang(str)
    val i = indexInferer.infer(start, str)
    val j = indexInferer.infer(end, str)
    val rOrd = substr(r, i, j)

    // Return both results.
    (rOpt, rOrd)

  private def computeConstOffset(expr: Expr, base: Expr): Option[Int] =
    var unexpected = false
    var baseCount = 0
    var sum = 0
    expr.summands.foreach:
      case Const(n: Int) => sum += n
      case e => if e == base then baseCount += 1 else unexpected = true
    if !unexpected && baseCount == 1 && sum >= 0 then Some(sum)
    else None

  private def substr(r: RegEx, startIndex: Index, endIndex: Index): RegEx =
    (startIndex, endIndex) match
      case (IndexL(0), IndexR(0)) => r
      case (i: BasicIndex, IndexR(0)) => r.drop(i)
      case (IndexL(0), j: BasicIndex) => r.take(j)
      case (IndexL(i), IndexL(j)) => r.take(j).drop(i)
      case (IndexR(_), IndexL(_)) => throw UnsupportedOperationException()
      case (IndexAt(t), IndexL(j)) =>
        // TODO: prove that i + |t| < j
        r.take(j).drop(IndexAt(t))
      case (i: BasicIndex, j: BasicIndex) => r.drop(i).take(j)
      case (IndexShifted(i, k), j) if k < 0 =>
        // -k ≤ i ∧ i ≤ j ≤ length s
        r.take(i).reverse.take(-k).reverse ++ substr(r, i, j)
      case (IndexShifted(i, k), j) if k > 0 => substr(r, i, j).drop(k)
      case (i, IndexShifted(j, k)) if k < 0 =>
        // i ≤ j - -k ∧ j ≤ length s
        substr(r, i, j).reverse.drop(-k).reverse
      case (i, IndexShifted(j, k)) if k > 0 =>
        // i ≤ j
        substr(r, i, j) ++ r.drop(j).take(k)
      case (IndexInterval(i1, i2), IndexR(0)) =>
        val fragment = substr(r, i1, i2.shift(1))
        val r1 = RegEx.fromCharSet(fragment.alphabet).+
        val r2 = substr(r, i2.shift(1), IndexR(0))
        r1 ++ r2
      case _ =>
        val lb = startIndex match
          case i: (BasicIndex | IndexShifted) => i
          case IndexInterval(i, _) => i
        val ub = endIndex match
          case j: (BasicIndex | IndexShifted) => j
          case IndexInterval(_, j) => j
        logger.debug(s"substr($startIndex, $endIndex) => [$lb, $ub)")
        RegEx.RELit(substr(r, lb, ub).alphabet).*

  def inferTest(test: Expr): BoolSet = test match
    case PrefixOf(Const(t: String), es) => prefixOf(t, es)
    case PrefixOf(_, _) => BoolSet.All
    case SuffixOf(Const(t: String), es) => prefixOf(t.reverse, Reverse(es))
    case SuffixOf(_, _) => BoolSet.All
    case InfixOf(Const(t: String), es) => infixOf(t, es)
    case InfixOf(_, _) => BoolSet.All
    case _ => throw IllegalArgumentException()

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

  def inferLength(len: Length): Interval =
    val r = inferLang(len.str)
    r.length

  def inferFind(find: Find): (BoolSet, List[Index]) = find.pat match
    case Const(t: String) => infixOf(t, find.str) match
      case BoolSet.False => (BoolSet.False, Nil)
      case bs =>
        val r = inferLang(find.str)
        logger.debug(s"${find.str} : $r")
        val results = ListBuffer.empty[Index]
        r.take(IndexAt(t)).length match
          case Interval(n1, n2: Int) if n1 == n2 => results += IndexL(n1)
          case Interval(n1, n2: Int) => results += IndexInterval(IndexL(n1), IndexL(n2))
          case Interval(n1, _) => results += IndexInterval(IndexL(n1), IndexR(1))
        r.drop(IndexAt(t)).length match
          case Interval(n1, n2: Int) if n1 == n2 => results += IndexR(n1)
          case Interval(n1, n2: Int) => results += IndexInterval(IndexR(n2), IndexR(n1))
          case Interval(n1, _) => results += IndexInterval(IndexL(0), IndexR(n1))
        (bs, results.toList)
    case _ => (BoolSet.All, List(IndexInterval(IndexL(0), IndexR(1))))
