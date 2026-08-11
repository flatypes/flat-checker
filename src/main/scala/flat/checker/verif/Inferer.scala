package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.CharSetOps.*
import flat.checker.domain.Index.*
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.*
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

enum Type:
  case TBool(dom: BoolSet)
  case TNat(dom: CountingRE)
  case TIndex(dom: IndexSet)
  case TNum(dom: NatRange)
  case TChar(dom: CharSet)
  case TStr(dom: StrRE)
  case TStrSeq(dom: RegEx[StrRE])

import flat.checker.verif.Type.*

class Inferer(goal: Goal) extends LazyLogging:
  def infer(expr: Expr): Type = expr match
    case Const(s: String) => TStr(RegEx.word(s.toList))
    case Var(x) =>
      goal.premises.reverseIterator
        .collectFirst:
          case CharIn(`expr`, a) => TChar(narrow(expr, a))
          case StringInLang(`expr`, r) => TStr(narrow(expr, r))
        .getOrElse:
          goal.sorts(x) match
            case CharSort => TChar(narrow(expr, CharSet.full))
            case SeqSort(CharSort) => TStr(narrow(expr, RegEx.full))
            case SeqSort(SeqSort(CharSort)) => TStrSeq(RegEx.Lit(RegEx.full).star)
            case _ => throw new Exception(s"Cannot infer type for ${expr.show}")

    // Char Operations
    case CharIn(e, b) =>
      infer(e) match
        case TChar(a) => TBool(a.absIn(b))
        case _ => throw new Exception("Expected a char type for CharIn")
    case CharToString(e) =>
      infer(e) match
        case TChar(a) => TStr(RegEx.symbolSet(a))
        case _ => throw new Exception("Expected a char type for CharToString")

    // Seq Operations
    case SeqConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStr(r1), TStr(r2)) => TStr(narrow(expr, r1 * r2))
        case (TStrSeq(r1), TStrSeq(r2)) => TStrSeq(r1 * r2)
        case _ => throw new Exception(s"Sort Error: ${expr.show}")
    case SeqLength(e) =>
      infer(e) match
        case TStr(r) => TNat(r.absLength)
        case TStrSeq(r) => TNat(r.absLength)
        case _ => throw new Exception("Expected a sequence type for SeqLength")
    case SeqSelect(e, ei) =>
      (infer(e), inferIndex(ei, e)) match
        case (TStr(r), index) => TChar(narrow(expr, r.absAt(index)))
        case (TStrSeq(r), index) => TStr(narrow(expr, r.absAt(index)))
        case _ => throw new Exception("Expected a sequence type for SeqAt")
    case SeqSlice(e, ei, ej) =>
      (infer(e), inferIndex(ei, e), inferIndex(ej, e)) match
        case (TStr(r), i, j) => TStr(narrow(expr, r.absSlice(i, j)))
        case _ => throw new Exception(s"Cannot infer ${expr.show}")
    case SeqStartsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absStartsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqStartsWith")
    case SeqEndsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absEndsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqEndsWith")
    case SeqContains(Const(s: String), e) =>
      infer(e) match
        case TStr(r) if r.isFiniteLang =>
          val results = r.getLang.map(s.contains)
          if results.forall(_ == true) then TBool(BoolSet.True)
          else if results.forall(_ == false) then TBool(BoolSet.False)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case SeqContains(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absContains(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case SeqIndexOf(e, et, Const(0)) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TIndex(r.absIndexOfStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqIndexOf")
    case StringSplit(e, et, lim) =>
      (infer(e), et, lim) match
        case (TStr(r), Const(s: String), Some(Const(m: Int))) => TStrSeq(r.absSplit(s.toList, m))
        case (TStr(r), Const(s: String), None) => TStrSeq(r.absSplitStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqSplit")
    case SeqCount(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TNat(r.absCountStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqCount")
    case SeqReverse(e) =>
      infer(e) match
        case TStrSeq(r) => TStrSeq(r.reverse)
        case _ => throw new Exception("Expected a sequence type for SeqReverse")

    // String-specific
    case StrReplace(e, Const(s1: String), Const(s2: String)) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absReplace(s1, s2)))
        case _ => throw new Exception("Expected a string type for StrReplace")
    case StringToLower(e) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absMap(_.map(_.toLower))))
        case _ => throw new Exception("Expected a string type for StringToLower")
    case StringToUpper(e) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absMap(_.map(_.toUpper))))
        case _ => throw new Exception("Expected a string type for StringToUpper")
    case StringTrim(e) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absTrim()))
        case _ => throw new Exception("Expected a string type for StringTrim")
    case StrFromInt(e, fmt) =>
      val solver = LPSolver(goal.premises)
      solver.solve(e) match
        case (Some(min), Some(max)) if 0 <= min =>
          TStr(narrow(expr, absFromInt(min, max, fmt)))
        case _ =>
          val r1 = RegEx.Lit(CharSet.from(fmt.digits))
          TStr(narrow(expr, r1.plus))
    case StringToInt(e) =>
      infer(e) match
        case TStr(r) => TNum(r.absToInt())
        case _ => throw new Exception("Expected a string type for StringToInt")

    case StrIsAscii(e) =>
      infer(e) match
        case TStr(r) => TBool(r.absIsAscii)
        case _ => throw new Exception("Expected a string type for StrIsAscii")

    // String eq
    case Eq(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) =>
          if r.filterEq(s.toList).isEmpty then TBool(BoolSet.False)
          else if r.filterNe(s.toList).isEmpty then TBool(BoolSet.True)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a string type for Eq")
    case Ne(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) =>
          if r.filterNe(s.toList).isEmpty then TBool(BoolSet.False)
          else if r.filterEq(s.toList).isEmpty then TBool(BoolSet.True)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a string type for Ne")

    case Ite(_, e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStr(r1), TStr(r2)) => TStr(narrow(expr, r1 | r2))
        case _ => throw new Exception(s"Type inference not implemented for Ite with types: ${infer(e1)} and ${infer(e2)}")

    case other =>
      throw new Exception(s"Type inference not implemented for expression: $other")

  private def inferIndex(idx: Expr, seq: Expr): Index = idx match
    case Const(i: Int) if 0 <= i => Left(i)
    case SeqLength(`seq`) => Right(0)
    case Add(SeqLength(`seq`), Const(i: Int)) if 0 <= i => Right(i)
    case Sub(SeqLength(`seq`), Const(i: Int)) if 0 <= i => Right(i)
    case Add(Const(i: Int), Sub(SeqLength(`seq`), Const(j: Int))) if 0 <= i - j => Right(i - j)
    case SeqIndexOf(`seq`, Const(s: String), Const(0)) => First(s.toList)
    case Add(SeqIndexOf(`seq`, Const(s: String), Const(0)), Const(i: Int)) => First(s.toList, i)
    case Sub(Add(SeqIndexOf(`seq`, Const(s: String), Const(0)), Const(i: Int)), Const(j: Int)) if 0 <= j =>
      First(s.toList, i - j)
    case _ =>
      logger.warn(s"Cannot infer index ${idx.show} for ${seq.show}")
      UnknownIndex

  private def narrow(e: Expr, r: StrRE): StrRE =
    var r1 = r
    for
      premise <- goal.premises
      r2 <- narrowBy(e, r1, premise)
    do
      logger.debug("Narrow {}: from {} to {} by {}", e.show, r1.pp, r2.pp, premise.show)
      r1 = r2
    r1

  private def narrowBy(e: Expr, r: StrRE, premise: Expr): Option[StrRE] = premise match
    // prefix, suffix
    case SeqStartsWith(`e`, Const(t: String)) => Some(r.filterStartsWith(t.toList))
    case Not(SeqStartsWith(`e`, Const(t: String))) => Some(r.filterNotStartWith(t.toList))
    case SeqEndsWith(`e`, Const(t: String)) => Some(r.filterEndsWith(t.toList))
    case Not(SeqEndsWith(`e`, Const(t: String))) => Some(r.filterNotEndWith(t.toList))
    // equality
    case Eq(`e`, Const(t: String)) => Some(r.filterEq(t.toList))
    case Ne(`e`, Const(t: String)) => Some(r.filterNe(t.toList))
    // contains
    case SeqContains(`e`, Const(t: String)) => Some(r.filterContains(t.toList))
    case Ne(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterContains(t.toList))
    case Le(Const(0), SeqIndexOf(`e`, Const(t: String), Const(0))) => Some(r.filterContains(t.toList))
    case Lt(Const(0), Add(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(1))) => Some(r.filterContains(t.toList))
    case Ne(Add(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(1)), Const(0)) => Some(r.filterContains(t.toList))
    // not contain
    case Not(SeqContains(`e`, Const(t: String))) => Some(r.filterNotContain(t.toList))
    case Eq(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(-1)) => Some(r.filterNotContain(t.toList))
    case Lt(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(0)) => Some(r.filterNotContain(t.toList))
    case Eq(Add(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(1)), Const(0)) => Some(r.filterNotContain(t.toList))
    case Le(Add(SeqIndexOf(`e`, Const(t: String), Const(0)), Const(1)), Const(0)) => Some(r.filterNotContain(t.toList))
    // indexOf order
    case Lt(SeqIndexOf(`e`, Const(s1: String), Const(0)), SeqIndexOf(`e`, Const(s2: String), Const(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    case Le(SeqIndexOf(`e`, Const(s1: String), Const(0)), SeqIndexOf(`e`, Const(s2: String), Const(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    // emptiness
    case Lt(Const(0), SeqLength(`e`)) => Some(r.filterNonEmpty)
    case Ne(SeqLength(`e`), Const(0)) => Some(r.filterNonEmpty)
    // charAt
    case Eq(SeqSelect(`e`, Const(i: Int)), Const(c: Char)) => Some(r.filterElemAtEq(i, c))
    case Eq(SeqSlice(`e`, Const(i: Int), Const(j: Int)), Const(s: String)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtEq(i, s.head))
    case Ne(SeqSelect(`e`, Const(i: Int)), Const(c: Char)) => Some(r.filterElemAtNe(i, c))
    case Ne(SeqSlice(`e`, Const(i: Int), Const(j: Int)), Const(s: String)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtNe(i, s.head))
    case SeqContains(Const(s: String), CharToString(SeqSelect(`e`, Const(i: Int)))) =>
      Some(r.filterElemAtIn(i, s.toList))
    case Not(SeqContains(Const(s: String), CharToString(SeqSelect(`e`, Const(i: Int))))) =>
      Some(r.filterElemAtNotIn(i, s.toList))
    // Length
    case Le(SeqLength(`e`), Const(n: Int)) if 0 <= n => Some(r.filterLengthLe(n))
    case Lt(SeqLength(`e`), Const(n: Int)) if 1 <= n => Some(r.filterLengthLe(n - 1))
    case Le(Const(n: Int), SeqLength(`e`)) if 0 <= n => Some(r.filterLengthGe(n))
    case Lt(Const(n: Int), SeqLength(`e`)) if -1 <= n => Some(r.filterLengthGe(n + 1))
    case Eq(SeqLength(`e`), Const(n: Int)) if 0 <= n => Some(r.filterLengthEq(n))
    case Ne(SeqSlice(`e`, Const(i: Int), Const(j: Int)), Const("")) if 0 <= i && i + 1 == j => Some(r.filterLengthGe(j))
    case _ => None

  private def narrow(e: Expr, a: CharSet): CharSet =
    var a1 = a
    for
      premise <- goal.premises
      a2 <- narrowBy(e, a1, premise)
    do
      logger.debug("Narrow {}: from {} to {} by {}", e.show, a1.toString, a2.toString, premise.show)
      a1 = a2
    a1

  private def narrowBy(e: Expr, a: CharSet, premise: Expr): Option[CharSet] = premise match
    // equality
    case Eq(`e`, Const(c: Char)) => Some(if a.contains(c) then CharSet(c) else CharSet.empty)
    case Ne(`e`, Const(c: Char)) => Some(a - c)
    // membership
    case SeqContains(es, CharToString(`e`)) =>
      infer(es) match
        case TStr(r) => Some(a & r.alphabet)
        case _ => None
    case _ => None