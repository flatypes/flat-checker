package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Index.{First, Left, Right}
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum Domain:
  case Top
  case Nat(set: CountingRE)
  case Index(len: Expr, set: IndexSet)
  case Num(set: NatRange)
  case Bool(set: BoolSet)
  case Str(set: StrRE)
  case StrList(set: RegEx[StrRE])

  def membership(elem: Expr): List[Expr] = this match
    case Top => Nil
    case Nat(set) =>
      if set.isFinite then
        val ns = set.toFinSet.toList.sorted
        if ns.length == ns.max - ns.min + 1 then
          List(Le(IntConst(ns.min), elem), Le(elem, IntConst(ns.max)))
        else
          List(mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, IntConst(n))))
      else
        List(Le(IntConst(set.min), elem))

    case Index(len, set) =>
      val leftCases = ListBuffer.empty[Expr]
      val rightCases = ListBuffer.empty[Expr]
      if set.neg.nonEmpty then
        leftCases += Eq(elem, IntConst(-1))
        rightCases += Eq(elem, IntConst(-1))
      if set.pos.isFinite then
        for n <- set.pos.toFinSet.toList.sorted do
          leftCases += Eq(elem, IntConst(n))
      else
        leftCases += Le(IntConst(set.pos.min), elem)
      if set.right.isFinite then
        for n <- set.right.toFinSet.toList.sorted do
          rightCases += Eq(elem, Sub(len, IntConst(n)))
      else
        rightCases += Le(elem, Sub(len, IntConst(set.right.min)))
      List(mkOr(leftCases.toList), mkOr(rightCases.toList))

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

  override def toString: String = this match
    case Top => "Top"
    case Nat(set) => s"Nat($set)"
    case Index(_, set) => s"Index($set)"
    case Num(set) => s"Num($set)"
    case Bool(set) => s"Bool($set)"
    case Str(set) => s"Str(${set.pp})"
    case StrList(set) => s"StrList(${set.pp})"

private def inCharSet(elem: Expr, set: CharSet): Expr =
  if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c)))
  else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c)))

private val symbolSet =
  given_SymbolSet_RegEx[CharSet].asInstanceOf[SymbolSet[RegEx[CharSet]] {type Symbol = List[Char]}]

object Inferer extends LazyLogging:
  def infer(expr: Expr)(using goal: Goal): Domain =
    expr match
      case StrConst(s) =>
        return Domain.Str(RegEx.word(s.toList))
      case _ =>

    val dom = goal.premises.reverseIterator.collectFirst:
      case StrIn(`expr`, r) => Domain.Str(r)
      case StrListIn(`expr`, r) => Domain.StrList(r)
    if dom.isDefined then
      return dom.get

    expr match
      case Var(x) =>
        goal.ctx(x) match
          case StrType => Domain.Str(RegEx.full)
          case ListType(StrType) => Domain.StrList(RegEx.Lit(RegEx.full).star)
          case _ => Domain.Top

      // Equality
      case Eq(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) =>
            logger.debug("Infer {} = {}: {}", e.show, t, r.pp)
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
          case Domain.Str(r) => Domain.Str(RegEx.symbolSet(r.absAt(inferIndex(ei, e, isStr = true))))
          case _ => Domain.Top
      case Substr(e, ei, ej) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absSlice(inferIndex(ei, e, isStr = true), inferIndex(ej, e, isStr = true)))
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
      case StrIndexOf(e, StrConst(t), IntConst(i)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Index(StrLength(e), r.absIndexOf(t.toList, i.toInt))
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
        e match
          case BitAnd(_, IntConst(n)) if n >= 0 =>
            Domain.Str(absFromInt(0, n.toInt, fmt))
          case _ =>
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
      case StrSplit(e, StrConst(t), _) =>
        infer(e) match
          case Domain.Str(r) => Domain.StrList(r.absSplit(t.toList))
          case _ => Domain.Top
      case StrJoin(_, _) => ???
      case StrCount(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Nat(r.absCount(t.toList))
          case _ => Domain.Top

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
      case e@ListExpr(es) if e.elemSort == StrType =>
        val rs = es.map(infer).map:
          case Domain.Str(r) => r
          case _ => RegEx.full
        Domain.StrList(RegEx.product(rs.map(RegEx.symbolSet)))
      case ListContains(e, StrConst(t)) =>
        infer(e) match
          case Domain.StrList(r) =>
            Domain.Bool(REOps.absContainsElem[RegEx[CharSet]](r)(using symbolSet)(t.toList))
          case _ => Domain.Top
      case ListLength(e) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Nat(r.absLength)
          case _ => Domain.Top
      case ListSelect(e, ei) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Str(r.absAt(inferIndex(ei, e, isStr = false)))
          case _ => Domain.Top
      case ListSlice(e, ei, ej) =>
        infer(e) match
          case Domain.StrList(r) => Domain.StrList(r.absSlice(inferIndex(ei, e, isStr = false),
            inferIndex(ej, e, isStr = false)))
          case _ => Domain.Top
      case ListConcat(e1, e2) =>
        (infer(e1), infer(e2)) match
          case (Domain.StrList(r1), Domain.StrList(r2)) => Domain.StrList(r1 * r2)
          case _ =>
            Domain.Top
      case ListIndexOf(e, StrConst(t), IntConst(i)) if i >= 0 =>
        infer(e) match
          case Domain.StrList(r) =>
            Domain.Index(ListLength(e), REOps.absIndexOfElem[RegEx[CharSet]](r)(using symbolSet)(t.toList, i.toInt))
          case _ => Domain.Top

      // Others
      case _ => Domain.Top

  private def inferIndex(idx: Expr, seq: Expr, isStr: Boolean)(using goal: Goal): Index = idx match
    case IntConst(i) if 0 <= i => Left(i.intValue)
    case StrLength(`seq`) => Right(0)
    case ListLength(`seq`) => Right(0)
    case Add(StrLength(`seq`), IntConst(i)) if 0 <= i => Right(0)
    case Add(ListLength(`seq`), IntConst(i)) if 0 <= i => Right(0)
    case Sub(StrLength(`seq`), IntConst(i)) if 0 < i => Right(i.intValue)
    case Sub(ListLength(`seq`), IntConst(i)) if 0 < i => Right(i.intValue)
    case Add(IntConst(i), Sub(StrLength(`seq`), IntConst(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case Add(IntConst(i), Sub(ListLength(`seq`), IntConst(j))) if 0 <= i - j => Right(i.intValue - j.intValue)
    case StrIndexOf(`seq`, StrConst(s), IntConst(i)) => First(s.toList, i.toInt)
    case Add(StrIndexOf(`seq`, StrConst(s), IntConst(i)), IntConst(k)) => First(s.toList, i.toInt, k.toInt)
//    case Sub(Add(StrIndexOf(`seq`, StrConst(s), IntConst(i)), IntConst(i)), IntConst(j)) if 0 <= j =>
//      First(s.toList, i.intValue - j.intValue)
    case Var(x) => getVarIndexRange(x, seq, isStr)
    case _ => Index.Range(Left(0), Right(0)) // unknown index

  private def getVarIndexRange(x: String, seq: Expr, isStr: Boolean)(using goal: Goal): Index =
    val from = goal.premises.collectFirst:
      case Le(ListIndexOf(`seq`, StrConst(t), IntConst(i)), Var(`x`)) => Index.First(List(t), i.toInt)
      case Le(Add(ListIndexOf(`seq`, StrConst(t), IntConst(i)), IntConst(k)), Var(`x`)) =>
        Index.First(List(t.toList), i.toInt, k.toInt)
      case Le(IntConst(i), Var(`x`)) => Index.Left(i.intValue)
    val until = goal.premises.collectFirst:
      case Lt(Var(`x`), StrIndexOf(`seq`, StrConst(t), IntConst(i))) => Index.First(t.toList, i.toInt)
      case Lt(Var(`x`), ListIndexOf(`seq`, StrConst(t), IntConst(i))) => Index.First(List(t.toList), i.toInt)
      case Lt(Var(`x`), Sub(StrIndexOf(`seq`, StrConst(t), IntConst(i)), IntConst(k))) =>
        Index.First(t.toList, i.toInt, k.toInt)
      case Lt(Var(`x`), Sub(ListIndexOf(`seq`, StrConst(t), IntConst(i)), IntConst(k))) =>
        Index.First(List(t.toList), i.toInt, k.toInt)
      case Lt(Var(`x`), Sub(StrLength(`seq`), IntConst(i))) => Index.Right(i.intValue)
      case Lt(Var(`x`), Sub(ListLength(`seq`), IntConst(i))) => Index.Right(i.intValue)
    Index.Range(from.getOrElse(Index.Left(0)), until.getOrElse(Index.Right(0)))

  private def solve(idx: Expr, x: String, lstLike: Expr, isStr: Boolean, preferLeft: Boolean)
                   (using goal: Goal): Index =
    val constraints = collectRelevantConstraints(x)
    val solver = LPSolver(constraints)
    // High priority: IndexAt (with potential shift)
    val finds = constraints.flatMap(getOperands).collect:
      case f@StrIndexOf(e, StrConst(t), IntConst(0)) if e == lstLike => (f, Index.First(t.toList))
      case f@ListIndexOf(e, StrConst(t), IntConst(i)) if e == lstLike => (f, Index.First(List(t.toList), i.toInt))
      case f@Sub(StrIndexOf(e, StrConst(t), IntConst(i)), IntConst(j)) if e == lstLike =>
        (f, Index.First(t.toList, i.intValue, j.intValue))
    val lbAs = ListBuffer.empty[Index]
    val ubAs = ListBuffer.empty[Index]
    for (f, index) <- finds.distinct do
      val (minK, maxK) = solver.solve(Sub(idx, f))
      for k <- minK do lbAs += index.shift(k)
      for k <- maxK do ubAs += index.shift(k)
    val lbA = lbAs.length match
      case 0 => None
      case 1 => Some(lbAs.head)
      case _ =>
        logger.trace("simplify choose head of lbAs: {}", lbAs.mkString(", "))
        Some(lbAs.head)
    val ubA = ubAs.length match
      case 0 => None
      case 1 => Some(ubAs.head)
      case _ =>
        logger.trace("simplify choose head of ubAs: {}", ubAs.mkString(", "))
        Some(ubAs.head)
    if lbA.isDefined && lbA == ubA then
      return lbA.get
    // Equal priority: Index.Left
    val (minL, maxL) = solver.solve(idx)
    val isConstLeft = minL.isDefined && minL == maxL
    val (lbL, ubL) = (minL.map(Index.Left(_)), maxL.map(Index.Left(_)))
    // Equal priority: Index.Right
    val len: Expr = if isStr then StrLength(lstLike) else ListLength(lstLike)
    val (minR1, maxR1) = solver.solve(Sub(len, idx))
    val minR = minR1 match
      case Some(n) if n >= 0 => Some(n)
      case _ => None
    val maxR = maxR1 match
      case Some(n) if n >= 0 => Some(n)
      case _ => None
    val isConstRight = minR.isDefined && minR == maxR
    val (lbR, ubR) = (maxR.map(Index.Right(_)), minR.map(Index.Right(_)))
    // Decide
    if isConstLeft && !isConstRight then
      return Index.Left(minL.get)
    if isConstRight && !isConstLeft then
      return Index.Right(minR.get)
    if isConstLeft && isConstRight then
      return if preferLeft then Index.Left(minL.get) else Index.Right(minR.get)

    // Choose interval bounds
    logger.trace("choose lb from: {}, {}, {}", lbA, lbL, lbR)
    logger.trace("choose ub from: {}, {}, {}", ubA, ubL, ubR)
    val lb = (lbA, lbL, lbR) match
      case (Some(i), None | Some(Index.Left(0)), None) => i
      case (None, Some(i), None) => i
      case (None, None | Some(Index.Left(0)), Some(i)) => i
      case (None, Some(i), Some(_)) => i // for finite list, prefer Index.Left
      // relative position >= absolute position, prefer relative
      case (Some(i), _, _) => i
      case (None, None, None) => Index.Left(0)
      case _ =>
        logger.warn("choose lb from: {}, {}, {}", lbA, lbL, lbR)
        throw UnsupportedOperationException(s"solve $x: ambiguous choice of lb")
    val ub = (ubA, ubL, ubR) match
      case (Some(i), None, None | Some(Index.Right(1))) => i
      case (None, Some(i), None | Some(Index.Right(1))) => i
      case (None, None, Some(i)) => i
      case (None, Some(_), Some(i)) => i // for finite list, prefer Index.Right
      case (Some(i), _, _) => i
      case (None, None, None) => Index.Right(0)
      case _ =>
        logger.warn("choose ub from: {}, {}, {}", ubA, ubL, ubR)
        throw UnsupportedOperationException(s"solve $x: ambiguous choice of ub")
    Index.Range(lb, ub)

  private def collectRelevantConstraints(x: String)(using goal: Goal): List[Expr] =
    val candidates: List[Expr] = goal.premises.collect { case b: (Eq | Le | Lt) => b }
    val selected = mutable.Set.empty[Int]
    val consideredVars = mutable.Set.empty[String]
    var newVars = Set(x)
    while newVars.nonEmpty do
      val thisRoundSelected = mutable.Set.empty[Int]
      for
        i <- candidates.indices
        if !selected.contains(i)
        if (candidates(i).collect { case Var(x) => x }.toSet & newVars).nonEmpty
      do
        thisRoundSelected += i
      selected ++= thisRoundSelected
      consideredVars ++= newVars
      newVars = thisRoundSelected.flatMap(candidates(_).collect { case Var(x) => x }.toSet).toSet -- consideredVars
    selected.toList.map(candidates)

  private def getOperands(expr: Expr): List[Expr] = expr match
    case Eq(e1, e2) => List(e1, e2)
    case Le(e1, e2) => List(e1, e2)
    case Lt(e1, e2) => List(e1, e2)
    case _ => Nil
