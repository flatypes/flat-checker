package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.tpd.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum Domain:
  case Top
  case Nat(set: CountingRE)
  case Index(len: Expr, set: IndexSet)
  case Num(set: NatRange)
  case Bool(set: BoolSet)
  case Char(set: CharSet)
  case Str(set: StrRE)
  case StrList(set: RegEx[StrRE])

  def membership(elem: Expr): List[Expr] = this match
    case Top => Nil
    case Nat(set) =>
      inNatSet(elem, set.toNatSet)

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

    case Char(set) =>
      if set.pos then
        List(mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c))))
      else
        List(mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c))))

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
    case Char(set) => s"Char($set)"
    case Str(set) => s"Str(${set.pp})"
    case StrList(set) => s"StrList(${set.pp})"

private def inNatSet(elem: Expr, set: NatSet): List[Expr] =
  val nats = set.nats.toList.sorted
  if set.infinite then
    List(mkOr((for n <- nats.init yield Eq(elem, IntConst(n))()) :+ Le(IntConst(nats.last), elem)))
  else
    List(mkOr(for n <- nats yield Eq(elem, IntConst(n))))

private def inCharSet(elem: Expr, set: CharSet): Expr =
  if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c)))
  else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c)))

private val symbolSet =
  given_SymbolSet_RegEx[CharSet].asInstanceOf[SymbolSet[RegEx[CharSet]] {type Symbol = List[Char]}]

object Inferer extends LazyLogging:
  def infer(expr: Expr)(using goal: Goal): Domain =
    expr match
      case CharConst(c) =>
        return Domain.Char(CharSet(c))
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

      // Char
      case CharToStr(e) =>
        infer(e) match
          case Domain.Char(a) => Domain.Str(RegEx.symbolSet(a))
          case _ => Domain.Top
      case CharIn(e, a) =>
        infer(e) match
          case Domain.Char(b) =>
            if b.subsetOf(a) then Domain.Bool(BoolSet.True)
            else if (b & a).isEmpty then Domain.Bool(BoolSet.False)
            else Domain.Bool(BoolSet.Full)
          case _ => Domain.Top

      // String
      case StrContains(e, StrConst(t)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Bool(r.absContains(t.toList))
          case _ => Domain.Top
      case StrLen(e) =>
        infer(e) match
          case Domain.Str(r) => Domain.Nat(r.absLength)
          case _ => Domain.Top
      case CharAt(e, ei) =>
        infer(e) match
          case Domain.Str(r) =>
            val index = IndexInferer.inferStrIndex(ei, e, goal)
            Domain.Char(StrREAPI.charAt(r, index))
          case _ => Domain.Top
      case Substr(e, ei, ej) =>
        infer(e) match
          case Domain.Str(r) =>
            (IndexInferer.inferStrIndex(ei, e, goal), IndexInferer.inferStrIndex(ej, e, goal)) match
              case (from: AIndex, until: AIndex) => Domain.Str(StrREAPI.substr(r, from, until))
              case _ => Domain.Top
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
          case Domain.Str(r) => Domain.Index(StrLen(e), r.absIndexOf(t.toList, i.toInt))
          case _ => Domain.Top
      case StrReplace(e, StrConst(t1), StrConst(t2)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Str(r.absReplace(t1, t2))
          case _ => Domain.Top

      // String conversions
      case StrToInt(e, IntConst(radix)) =>
        infer(e) match
          case Domain.Str(r) => Domain.Num(r.absToInt(radix.toInt))
          case _ => Domain.Top
      case IntFormat(e, fmt) =>
        e match
          case BitAnd(_, IntConst(n)) if n >= 0 =>
            Domain.Str(absFromInt(0, n.toInt, fmt))
          case _ =>
            inferInterval(e, goal) match
              case Some(Interval(min, max)) if 0 <= min =>
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
      case StrForall(e, LambdaExpr(List(x -> _), CharInCharSet(Var(x1), a))) if x1 == x =>
        infer(e) match
          case Domain.Str(r) =>
            if r.alphabet.subsetOf(a) then Domain.Bool(BoolSet.True)
            else Domain.Bool(BoolSet.Full)
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
      case ListLen(e) =>
        infer(e) match
          case Domain.StrList(r) => Domain.Nat(r.absLength)
          case _ => Domain.Top
      case ElemAt(e, ei) =>
        infer(e) match
          case Domain.StrList(r) =>
            val index = IndexInferer.inferListIndex(ei, e, goal)
            Domain.Str(ListREAPI.elemAt(r, index))
          case _ => Domain.Top
      case ListSlice(e, ei, ej) =>
        infer(e) match
          case Domain.StrList(r) =>
            (IndexInferer.inferListIndex(ei, e, goal), IndexInferer.inferListIndex(ej, e, goal)) match
              case (from: AIndex, until: AIndex) => Domain.StrList(ListREAPI.slice(r, from, until))
              case _ => Domain.Top
          case _ => Domain.Top
      case ListConcat(e1, e2) =>
        (infer(e1), infer(e2)) match
          case (Domain.StrList(r1), Domain.StrList(r2)) => Domain.StrList(r1 * r2)
          case _ =>
            Domain.Top
      case ListIndexOf(e, StrConst(t), IntConst(i)) if i >= 0 =>
        infer(e) match
          case Domain.StrList(r) =>
            Domain.Index(ListLen(e), REOps.absIndexOfElem[RegEx[CharSet]](r)(using symbolSet)(t.toList, i.toInt))
          case _ => Domain.Top

      // Others
      case _ => Domain.Top

  private object CharInCharSet:
    def unapply(e: Expr): Option[(Expr, CharSet)] = e match
      case CharIn(e1, a) => Some((e1, a))
      case StrContains(StrConst(cs), CharToStr(e1)) => Some((e1, CharSet.from(cs)))
      case _ => None

  def inferChar(expr: Expr, goal: Goal): CharSet =
    infer(expr)(using goal) match
      case Domain.Char(set) => set
      case _ => CharSet.full

  def inferStr(expr: Expr, goal: Goal): StrRE =
    infer(expr)(using goal) match
      case Domain.Str(r) => r
      case _ => RegEx.full

  def inferStrList(expr: Expr, goal: Goal): ListRE =
    infer(expr)(using goal) match
      case Domain.StrList(r) => r
      case _ => RegEx.Lit(RegEx.full).star

  private def inferInterval(expr: Expr, goal: Goal): Option[Interval] = expr match
    case Negate(e) => inferInterval(e, goal).map(-_)
    case Add(e, IntConst(i)) => inferInterval(e, goal).map(_ + i)
    case Sub(e, IntConst(i)) => inferInterval(e, goal).map(_ - i)
    case Mul(e, IntConst(i)) => inferInterval(e, goal).map(_ * i)
    case Div(e, IntConst(i)) if i != 0 => inferInterval(e, goal).map(_ / i)
    case Mod(e, IntConst(i)) if i > 0 => inferInterval(e, goal).map(_ % i)
    case _ =>
      val solver = LPSolver(goal.premises)
      solver.solve(expr) match
        case (Some(min), Some(max)) => Some(Interval(min, max))
        case _ => None