package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
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
    case StrConst(s) => TStr(RegEx.word(s.toList))
    case Var(x) =>
      goal.premises.reverseIterator
        .collectFirst:
          case StrIn(`expr`, r) => TStr(narrow(expr, r))
        .getOrElse:
          goal.sorts(x) match
            case StrType => TStr(narrow(expr, RegEx.full))
            case ListType(StrType) => TStrSeq(RegEx.Lit(RegEx.full).star)
            case _ => throw new Exception(s"Cannot infer type for ${expr.show}")

    // Seq Operations
    case StrConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStr(r1), TStr(r2)) => TStr(narrow(expr, r1 * r2))
        case (TStrSeq(r1), TStrSeq(r2)) => TStrSeq(r1 * r2)
        case _ => throw new Exception(s"Sort Error: ${expr.show}")
    case StrLength(e) =>
      infer(e) match
        case TStr(r) => TNat(r.absLength)
        case TStrSeq(r) => TNat(r.absLength)
        case _ => throw new Exception("Expected a sequence type for SeqLength")
    case CharAt(e, ei) =>
      (infer(e), inferIndex(ei, e)) match
        case (TStr(r), index) => TChar(narrow(expr, r.absAt(index)))
        case (TStrSeq(r), index) => TStr(narrow(expr, r.absAt(index)))
        case _ => throw new Exception("Expected a sequence type for SeqAt")
    case Substr(e, ei, ej) =>
      (infer(e), inferIndex(ei, e), inferIndex(ej, e)) match
        case (TStr(r), i, j) => TStr(narrow(expr, r.absSlice(i, j)))
        case _ => throw new Exception(s"Cannot infer ${expr.show}")
    case StrStartsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrConst(s)) => TBool(r.absStartsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqStartsWith")
    case StrEndsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrConst(s)) => TBool(r.absEndsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqEndsWith")
    case StrContains(StrConst(s), e) =>
      infer(e) match
        case TStr(r) if r.isFiniteLang =>
          val results = r.getLang.map(s.contains)
          if results.forall(_ == true) then TBool(BoolSet.True)
          else if results.forall(_ == false) then TBool(BoolSet.False)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case StrContains(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrConst(s)) => TBool(r.absContains(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case StrIndexOf(e, et, IntConst(0)) =>
      (infer(e), et) match
        case (TStr(r), StrConst(s)) => TIndex(r.absIndexOfStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqIndexOf")
    case StrSplit(e, et, lim) =>
      (infer(e), et, lim) match
        case (TStr(r), StrConst(s), Some(IntConst(m))) => TStrSeq(r.absSplit(s.toList, m.intValue))
        case (TStr(r), StrConst(s), None) => TStrSeq(r.absSplitStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqSplit")
    case StrCount(e, et) =>
      (infer(e), et) match
        case (TStr(r), StrConst(s)) => TNat(r.absCountStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqCount")
    case StrReverse(e) =>
      infer(e) match
        case TStrSeq(r) => TStrSeq(r.reverse)
        case _ => throw new Exception("Expected a sequence type for SeqReverse")

    // String-specific
    case StrReplace(e, StrConst(s1), StrConst(s2)) =>
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
    case IntFormat(e, fmt) =>
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

    case StrIs(e, a) =>
      infer(e) match
        case TStr(r) => if r.alphabet.subsetOf(a) then TBool(BoolSet.True) else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a string type for StrIs")

    // String eq
    case Eq(e, StrConst(s)) =>
      infer(e) match
        case TStr(r) =>
          if r.filterEq(s.toList).isEmpty then TBool(BoolSet.False)
          else if r.filterNe(s.toList).isEmpty then TBool(BoolSet.True)
          else TBool(BoolSet.Full)
        case _ => throw new Exception("Expected a string type for Eq")
    case Ne(e, StrConst(s)) =>
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
    case IntConst(i) if 0 <= i => Left(i.intValue)
    case StrLength(`seq`) => Right(0)
    case Add(StrLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Sub(StrLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Add(IntConst(i), Sub(StrLength(`seq`), IntConst(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case StrIndexOf(`seq`, StrConst(s), IntConst(0)) => First(s.toList)
    case Add(StrIndexOf(`seq`, StrConst(s), IntConst(0)), IntConst(i)) => First(s.toList, i.intValue)
    case Sub(Add(StrIndexOf(`seq`, StrConst(s), IntConst(0)), IntConst(i)), IntConst(j)) if 0 <= j =>
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
    case StrStartsWith(`e`, StrConst(t)) => Some(r.filterStartsWith(t.toList))
    case Not(StrStartsWith(`e`, StrConst(t))) => Some(r.filterNotStartWith(t.toList))
    case StrEndsWith(`e`, StrConst(t)) => Some(r.filterEndsWith(t.toList))
    case Not(StrEndsWith(`e`, StrConst(t))) => Some(r.filterNotEndWith(t.toList))
    // equality
    case Eq(`e`, StrConst(t)) => Some(r.filterEq(t.toList))
    case Ne(`e`, StrConst(t)) => Some(r.filterNe(t.toList))
    // contains
    case StrContains(`e`, StrConst(t)) => Some(r.filterContains(t.toList))
    case Ne(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(-1)) => Some(r.filterContains(t.toList))
    case Le(IntConst(0), StrIndexOf(`e`, StrConst(t), IntConst(0))) => Some(r.filterContains(t.toList))
    case Lt(IntConst(0), Add(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(1))) => Some(r.filterContains(t.toList))
    case Ne(Add(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) => Some(r.filterContains(t.toList))
    // not contain
    case Not(StrContains(`e`, StrConst(t))) => Some(r.filterNotContain(t.toList))
    case Eq(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(-1)) => Some(r.filterNotContain(t.toList))
    case Lt(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(0)) => Some(r.filterNotContain(t.toList))
    case Eq(Add(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) => Some(r.filterNotContain(t.toList))
    case Le(Add(StrIndexOf(`e`, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) => Some(r.filterNotContain(t.toList))
    // indexOf order
    case Lt(StrIndexOf(`e`, StrConst(s1), IntConst(0)), StrIndexOf(`e`, StrConst(s2), IntConst(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    case Le(StrIndexOf(`e`, StrConst(s1), IntConst(0)), StrIndexOf(`e`, StrConst(s2), IntConst(0)))
      if s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some(r.filterIndexOfLt(s1.head, s2.head))
    // emptiness
    case Lt(IntConst(0), StrLength(`e`)) => Some(r.filterNonEmpty)
    case Ne(StrLength(`e`), IntConst(0)) => Some(r.filterNonEmpty)
    // charAt
    case Eq(CharAt(`e`, IntConst(i)), CharConst(c)) => Some(r.filterElemAtEq(i.intValue, c))
    case Eq(Substr(`e`, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtEq(i.intValue, s.head))
    case Ne(CharAt(`e`, IntConst(i)), CharConst(c)) => Some(r.filterElemAtNe(i.intValue, c))
    case Ne(Substr(`e`, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 =>
      Some(r.filterElemAtNe(i.intValue, s.head))
    // Length
    case Le(StrLength(`e`), IntConst(n)) if 0 <= n => Some(r.filterLengthLe(n.intValue))
    case Lt(StrLength(`e`), IntConst(n)) if 1 <= n => Some(r.filterLengthLe(n.intValue - 1))
    case Le(IntConst(n), StrLength(`e`)) if 0 <= n => Some(r.filterLengthGe(n.intValue))
    case Lt(IntConst(n), StrLength(`e`)) if -1 <= n => Some(r.filterLengthGe(n.intValue + 1))
    case Eq(StrLength(`e`), IntConst(n)) if 0 <= n => Some(r.filterLengthEq(n.intValue))
    case Ne(Substr(`e`, IntConst(i), IntConst(j)), StrConst("")) if 0 <= i && i + 1 == j =>
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
    case Eq(`e`, CharConst(c)) => Some(if a.contains(c) then CharSet(c) else CharSet.empty)
    case Ne(`e`, CharConst(c)) => Some(a - c)
    case _ => None