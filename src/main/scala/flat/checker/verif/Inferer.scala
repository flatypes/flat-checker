package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Index.{First, Left, Right, UnknownIndex}
import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

enum Domain:
  case Top
  case Nat(set: CountingRE)
  case Index(set: IndexSet)
  case Num(set: NatRange)
  case Bool(set: BoolSet)
  case Str(set: StrRE)
  case StrList(set: RegEx[StrRE])

  def membership(elem: Expr): List[Expr] = this match
    case Top => Nil
    case Nat(set) =>
      if set.isFinite then
        List(mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, IntConst(n))))
      else
        List(Le(IntConst(set.min), elem))

    case Index(set) =>
      val cases = ListBuffer.empty[Expr]
      if set.neg.nonEmpty then
        for i <- set.neg.toList.sorted do
          cases += Eq(elem, IntConst(i))
      if set.pos.isFinite then
        for n <- set.pos.toFinSet.toList.sorted do
          cases += Eq(elem, IntConst(n))
      else
        cases += Le(IntConst(set.pos.min), elem)
      List(mkOr(cases.toList))

    case Num(set) =>
      set.max match
        case Some(max) => List(Le(IntConst(set.min), elem), Le(elem, IntConst(max)))
        case None => List(Le(IntConst(set.min), elem))

    case Bool(set) =>
      set match
        case BoolSet.True => List(elem)
        case BoolSet.False => List(Not(elem))
        case _ => Nil

    case Str(r) =>
      r match
        case RegEx.Lit(a) => List(inCharSet(elem, a))
        case _ =>
          if r.isFiniteLang then
            List(mkOr(for s <- r.getLang.toList.sorted yield Eq(elem, StrConst(s))))
          else
            List(StrIn(elem, r))

    case StrList(_) => Nil

private def inCharSet(elem: Expr, set: CharSet): Expr =
  if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c)))
  else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c)))

object Inferer extends LazyLogging:
  def infer(expr: Expr)(using goal: Goal): Domain =
    val r = goal.premises.reverseIterator.collectFirst { case StrIn(`expr`, r) => Domain.Str(r) }
    if r.isDefined then
      return r.get

    expr match
      case StrConst(s) => Domain.Str(RegEx.word(s.toList))
      case Var(x) =>
        goal.ctx(x) match
          case StrType => Domain.Str(RegEx.full)
          case ListType(StrType) => Domain.StrList(RegEx.Lit(RegEx.full).star)
          case _ => Domain.Top

      // Equality
      case Eq(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) =>
            if r.filterEq(t.toList).isEmpty then Domain.Bool(BoolSet.False)
            else if r.filterNe(t.toList).isEmpty then Domain.Bool(BoolSet.True)
            else Domain.Bool(BoolSet.Full)
          case _ => Domain.Top
      case Ne(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) =>
            if r.filterNe(t.toList).isEmpty then Domain.Bool(BoolSet.False)
            else if r.filterEq(t.toList).isEmpty then Domain.Bool(BoolSet.True)
            else Domain.Bool(BoolSet.Full)
          case _ => Domain.Top

      // String
      case StrContains(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Bool(r.absContains(t.toList))
          case _ => Domain.Top
      case StrLength(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Nat(r.absLength)
          case _ => Domain.Top
      case CharAt(e, ei) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(RegEx.symbolSet(r.absAt(inferIndex(ei, e))))
          case _ => Domain.Top
      case Substr(e, ei, ej) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absSlice(inferIndex(ei, e), inferIndex(ej, e)))
          case _ => Domain.Top
      case StrConcat(e1, e2) =>
        (infer(e1), infer(e2)) match
          case (Domain.Str(r1), Domain.Str(r2)) => Domain.Str(r1 * r2)
          case _ => Domain.Top
      case StrStartsWith(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Bool(r.absStartsWith(t.toList))
          case _ => Domain.Top
      case StrEndsWith(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Bool(r.absEndsWith(t.toList))
          case _ => Domain.Top
      case StrIndexOf(e, StrConst(t), IntConst(0)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Index(r.absIndexOf(t.toList))
          case _ => Domain.Top
      case StrReplace(e, StrConst(t1), StrConst(t2)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absReplace(t1, t2))
          case _ => Domain.Top

      // String conversions
      case StrToInt(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Num(r.absToInt())
          case _ => Domain.Top
      case IntFormat(e, fmt) =>
        val solver = LPSolver(goal.premises)
        solver.solve(e) match
          case (Some(min), Some(max)) if 0 <= min =>
            Domain.Str(absFromInt(min, max, fmt))
          case _ =>
            val r1 = RegEx.Lit(CharSet.from(fmt.digits))
            Domain.Str(r1.plus)

      // String extensions
      case StrReverse(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.reverse)
          case _ => Domain.Top
      case StrTrim(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absTrim())
          case _ => Domain.Top
      case StrCount(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Nat(r.absCount(t.toList))
          case _ => Domain.Top
      case StrSplit(e, StrConst(t), None) =>
        infer(e) match
          case Domain.Str(r) => Domain.StrList(r.absSplit(t.toList))
          case _ => Domain.Top
      case StrJoin(_, _) => ???

      // String: others
      case StrToLower(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absMap(_.map(_.toLower)))
          case _ => Domain.Top
      case StrToUpper(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absMap(_.map(_.toUpper)))
          case _ => Domain.Top

      // List
      case ListContains(e, StrConst(t)) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Bool(r.absContains(???))
          case _ => Domain.Top
      case ListLength(e) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Nat(r.absLength)
          case _ => Domain.Top
      case ListSelect(e, ei) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Str(r.absAt(inferIndex(ei, e)))
          case _ => Domain.Top
      case ListSlice(e, ei, ej) =>
        infer(e) match
          case Domain.StrList(r) => Domain.StrList(r.absSlice(inferIndex(ei, e), inferIndex(ej, e)))
          case _ => Domain.Top
      case ListConcat(e1, e2) =>
        (infer(e1), infer(e2)) match
          case (Domain.StrList(r1), Domain.StrList(r2)) => Domain.StrList(r1 * r2)
          case _ => Domain.Top

      // Others
      case _ => Domain.Top

  private def inferIndex(idx: Expr, seq: Expr): Index = idx match
    case IntConst(i) if 0 <= i => Left(i.intValue)
    case StrLength(`seq`) => Right(0)
    case ListLength(`seq`) => Right(0)
    case Add(StrLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Add(ListLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Sub(StrLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Sub(ListLength(`seq`), IntConst(i)) if 0 <= i => Right(i.intValue)
    case Add(IntConst(i), Sub(StrLength(`seq`), IntConst(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case Add(IntConst(i), Sub(ListLength(`seq`), IntConst(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case StrIndexOf(`seq`, StrConst(s), IntConst(0)) => First(s.toList)
    case Add(StrIndexOf(`seq`, StrConst(s), IntConst(0)), IntConst(i)) => First(s.toList, i.intValue)
    case Sub(Add(StrIndexOf(`seq`, StrConst(s), IntConst(0)), IntConst(i)), IntConst(j)) if 0 <= j =>
      First(s.toList, i.intValue - j.intValue)
    case _ =>
      logger.warn(s"Cannot infer index ${idx.show} for ${seq.show}")
      UnknownIndex
