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
    case StrLit(s) => TStr(RegEx.word(s.toList))
    case Var(x) =>
      goal.premises.reverseIterator
        .collectFirst:
          case CharIn(`expr`, a) => TChar(narrow(expr, a))
          case StringInLang(`expr`, r) => TStr(narrow(expr, r))
        .getOrElse:
          goal.sorts(x) match
            case CharType => TChar(narrow(expr, CharSet.full))
            case `strType` => TStr(narrow(expr, RegEx.full))
            case ListType(`strType`) => TStrSeq(RegEx.Lit(RegEx.full).star)
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
    case ListAt(e, ei) =>
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
        case (TStr(r), StrLit(s)) => TBool(r.absStartsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqStartsWith")
    case SeqEndsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrLit(s)) => TBool(r.absEndsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqEndsWith")
    case ListContainsSlice(StrLit(s), e) =>
      infer(e) match
        case TStr(r) if r.isFiniteLang =>
          val results = r.getLang.map(s.contains)
          if results.forall(_ == true) then TBool(BoolSet.True)
          else if results.forall(_ == false) then TBool(BoolSet.False)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case ListContainsSlice(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrLit(s)) => TBool(r.absContains(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case SeqIndexOf(e, et, IntLit(0)) =>
      (infer(e), et) match
        case (TStr(r), StrLit(s)) => TIndex(r.absIndexOfStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqIndexOf")
    case StrSplit(e, et, lim) =>
      (infer(e), et, lim) match
        case (TStr(r), StrLit(s), Some(IntLit(m))) => TStrSeq(r.absSplit(s.toList, m.intValue))
        case (TStr(r), StrLit(s), None) => TStrSeq(r.absSplitStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqSplit")
    case SeqCount(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrLit(s)) => TNat(r.absCountStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqCount")
    case SeqReverse(e) =>
      infer(e) match
        case TStrSeq(r) => TStrSeq(r.reverse)
        case _ => throw new Exception("Expected a sequence type for SeqReverse")

    // String-specific
    case StrReplace(e, StrLit(s1), StrLit(s2)) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absReplace(s1, s2)))
        case _ => throw new Exception("Expected a string type for StrReplace")
    case StrToLower(e) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absMap(_.map(_.toLower))))
        case _ => throw new Exception("Expected a string type for StringToLower")
    case StrToUpper(e) =>
      infer(e) match
        case TStr(r) => TStr(narrow(expr, r.absMap(_.map(_.toUpper))))
        case _ => throw new Exception("Expected a string type for StringToUpper")
    case StrTrim(e) =>
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
    case StrToInt(e) =>
      infer(e) match
        case TStr(r) => TNum(r.absToInt())
        case _ => throw new Exception("Expected a string type for StringToInt")

    case StrIsAscii(e) =>
      infer(e) match
        case TStr(r) => TBool(r.absIsAscii)
        case _ => throw new Exception("Expected a string type for StrIsAscii")

    // String eq
    case Eq(e, StrLit(s)) =>
      infer(e) match
        case TStr(r) =>
          if r.filterEq(s.toList).isEmpty then TBool(BoolSet.False)
          else if r.filterNe(s.toList).isEmpty then TBool(BoolSet.True)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a string type for Eq")
    case Ne(e, StrLit(s)) =>
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
    case IntLit(i) if 0 <= i => Left(i.intValue)
    case SeqLength(`seq`) => Right(0)
    case Add(SeqLength(`seq`), IntLit(i)) if 0 <= i => Right(i.intValue)
    case Sub(SeqLength(`seq`), IntLit(i)) if 0 <= i => Right(i.intValue)
    case Add(IntLit(i), Sub(SeqLength(`seq`), IntLit(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case SeqIndexOf(`seq`, StrLit(s), IntLit(0)) => First(s.toList)
    case Add(SeqIndexOf(`seq`, StrLit(s), IntLit(0)), IntLit(i)) => First(s.toList, i.intValue)
    case Sub(Add(SeqIndexOf(`seq`, StrLit(s), IntLit(0)), IntLit(i)), IntLit(j)) if 0 <= j =>
      First(s.toList, i.intValue - j.intValue)
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
    case SeqStartsWith(`e`, StrLit(t)) => Some(r.filterStartsWith(t.toList))
    case Not(SeqStartsWith(`e`, StrLit(t))) => Some(r.filterNotStartWith(t.toList))
    case SeqEndsWith(`e`, StrLit(t)) => Some(r.filterEndsWith(t.toList))
    case Not(SeqEndsWith(`e`, StrLit(t))) => Some(r.filterNotEndWith(t.toList))
    // equality
    case Eq(`e`, StrLit(t)) => Some(r.filterEq(t.toList))
    case Ne(`e`, StrLit(t)) => Some(r.filterNe(t.toList))
    // contains
    case ListContainsSlice(`e`, StrLit(t)) => Some(r.filterContains(t.toList))
    case Ne(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(-1)) => Some(r.filterContains(t.toList))
    case Le(IntLit(0), SeqIndexOf(`e`, StrLit(t), IntLit(0))) => Some(r.filterContains(t.toList))
    case Lt(IntLit(0), Add(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(1))) => Some(r.filterContains(t.toList))
    case Ne(Add(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(1)), IntLit(0)) => Some(r.filterContains(t.toList))
    // not contain
    case Not(ListContainsSlice(`e`, StrLit(t))) => Some(r.filterNotContain(t.toList))
    case Eq(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(-1)) => Some(r.filterNotContain(t.toList))
    case Lt(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(0)) => Some(r.filterNotContain(t.toList))
    case Eq(Add(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(1)), IntLit(0)) => Some(r.filterNotContain(t.toList))
    case Le(Add(SeqIndexOf(`e`, StrLit(t), IntLit(0)), IntLit(1)), IntLit(0)) => Some(r.filterNotContain(t.toList))
    // indexOf order
    case Lt(SeqIndexOf(`e`, StrLit(s1), IntLit(0)), SeqIndexOf(`e`, StrLit(s2), IntLit(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    case Le(SeqIndexOf(`e`, StrLit(s1), IntLit(0)), SeqIndexOf(`e`, StrLit(s2), IntLit(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    // emptiness
    case Lt(IntLit(0), SeqLength(`e`)) => Some(r.filterNonEmpty)
    case Ne(SeqLength(`e`), IntLit(0)) => Some(r.filterNonEmpty)
    // charAt
    case Eq(ListAt(`e`, IntLit(i)), CharLit(c)) => Some(r.filterElemAtEq(i.intValue, c))
    case Eq(SeqSlice(`e`, IntLit(i), IntLit(j)), StrLit(s)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtEq(i.intValue, s.head))
    case Ne(ListAt(`e`, IntLit(i)), CharLit(c)) => Some(r.filterElemAtNe(i.intValue, c))
    case Ne(SeqSlice(`e`, IntLit(i), IntLit(j)), StrLit(s)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtNe(i.intValue, s.head))
    case ListContainsSlice(StrLit(s), CharToString(ListAt(`e`, IntLit(i)))) =>
      Some(r.filterElemAtIn(i.intValue, s.toList))
    case Not(ListContainsSlice(StrLit(s), CharToString(ListAt(`e`, IntLit(i))))) =>
      Some(r.filterElemAtNotIn(i.intValue, s.toList))
    // Length
    case Le(SeqLength(`e`), IntLit(n)) if 0 <= n => Some(r.filterLengthLe(n.intValue))
    case Lt(SeqLength(`e`), IntLit(n)) if 1 <= n => Some(r.filterLengthLe(n.intValue - 1))
    case Le(IntLit(n), SeqLength(`e`)) if 0 <= n => Some(r.filterLengthGe(n.intValue))
    case Lt(IntLit(n), SeqLength(`e`)) if -1 <= n => Some(r.filterLengthGe(n.intValue + 1))
    case Eq(SeqLength(`e`), IntLit(n)) if 0 <= n => Some(r.filterLengthEq(n.intValue))
    case Ne(SeqSlice(`e`, IntLit(i), IntLit(j)), StrLit("")) if 0 <= i && i + 1 == j =>
      Some(r.filterLengthGe(j.intValue))
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
    case Eq(`e`, CharLit(c)) => Some(if a.contains(c) then CharSet(c) else CharSet.empty)
    case Ne(`e`, CharLit(c)) => Some(a - c)
    // membership
    case ListContainsSlice(es, CharToString(`e`)) =>
      infer(es) match
        case TStr(r) => Some(a & r.alphabet)
        case _ => None
    case _ => None