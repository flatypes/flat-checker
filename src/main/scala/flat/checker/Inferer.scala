package flat.checker

import flat.Config
import flat.checker.core.*
import flat.regex.*
import flat.regex.AOps.*

enum BoolSet:
  case True
  case False
  case All

class Inferer(using ctx: PrfCtx, config: Config):
  private val indexInferer = new IndexInferer

  def inferLang(str: Expr): RegEx = str match
    case Const(s: String) => RegEx.fromString(s)
    case Var(x) => ctx.getLang(str)
    case Concat(es1, es2) => inferLang(es1) ++ inferLang(es2)
    case Reverse(es) => inferLang(es).reverse
    case CharAt(es, ei) =>
      val r = inferLang(es)
      val i = indexInferer.infer(ei, es)
      substr(r, i, AIndexR(0)).take1
    case Substr(es, ei, ej) =>
      // TODO: ei, ej may not be index of es
      val r = inferLang(es)
      val i = indexInferer.infer(ei, es)
      val j = indexInferer.infer(ej, es)
      substr(r, i, j)
    case _ => throw IllegalArgumentException()

  private def substr(r: RegEx, startIndex: Index, endIndex: Index): RegEx =
    (startIndex, endIndex) match
      case (i: AIndex, AIndexR(0)) => r.drop(i)
      case (AIndexL(0), j: AIndex) => r.take(j)
      case (AIndexL(i), AIndexL(j)) => r.take(j).drop(i)
      case (AIndexR(_), AIndexL(_)) => throw UnsupportedOperationException()
      case (AIndexAt(t), AIndexL(j)) =>
        // TODO: prove that i + |t| < j
        r.take(j).drop(AIndexAt(t))
      case (i: AIndex, j: AIndex) => r.drop(i).take(j)
      case (IndexShifted(i, k), j) if k < 0 =>
        // -k ≤ i ∧ i ≤ j ≤ length s
        r.take(i).reverse.take(-k).reverse ++ substr(r, i, j)
      case (IndexShifted(i, k), j) if k > 0 => substr(r, i, j).drop(-k)
      case (i, IndexShifted(j, k)) if k < 0 =>
        // i ≤ j - -k ∧ j ≤ length s
        substr(r, i, j).reverse.drop(-k).reverse
      case (i, IndexShifted(j, k)) if k > 0 =>
        // i ≤ j
        substr(r, i, j) ++ r.drop(j).take(k)
      case _ =>
        val lb = startIndex match
          case i: (AIndex | IndexShifted) => i
          case IndexInterval(i, _) => i
        val ub = startIndex match
          case j: (AIndex | IndexShifted) => j
          case IndexInterval(_, j) => j
        RegEx.RELit(substr(r, lb, ub).alphabet).*

  def inferTest(test: Expr): BoolSet = test match
    case PrefixOf(Const(t: String), es) => prefixOf(t, es)
    case PrefixOf(_, _) => BoolSet.All
    case SuffixOf(Const(t: String), es) => prefixOf(t.reverse, Reverse(es))
    case SuffixOf(_, _) => BoolSet.All
    case InfixOf(Const(t: String), es) =>
      if t.isEmpty then
        return BoolSet.True
      val r = inferLang(es)
      val r1 = r.splitSuffix(t.head)
      if r1.derivative(t).isEmpty then BoolSet.False
      else if r.forallContains(t.head) && r1.forallPrefix(t) then BoolSet.True
      else BoolSet.All
    case InfixOf(_, _) => BoolSet.All
    case _ => throw IllegalArgumentException()

  private def prefixOf(t: String, str: Expr): BoolSet =
    if t.isEmpty then
      return BoolSet.True
    val r = inferLang(str)
    if r.derivative(t).isEmpty then BoolSet.False
    else if r.forallPrefix(t) then BoolSet.True
    else BoolSet.All

  def inferLength(len: Length): Interval =
    val r = inferLang(len.str)
    r.length

  def inferFind(find: Find): (Interval, Boolean) = find.pat match
    case Const("") => (Interval.at(0), true)
    case Const(t: String) =>
      val r = inferLang(find.str)
      (r.take(AIndexAt(t)).length, r.forallInfix(t))
    case _ => (Interval(), true)