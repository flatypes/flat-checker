package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.filterIndexOfLt
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Narrower(goal: Goal) extends LazyLogging:
  private val charTypes = mutable.Map.empty[Expr, CharSet]
  private val strTypes = mutable.Map.empty[Expr, StrRE]
  private val strListTypes = mutable.Map.empty[Expr, ListRE]

  def narrow: Goal =
    val conds = ListBuffer.empty[Expr]
    goal.premises.foreach:
      case StrIn(e, r) =>
        strTypes(e) = r
      case StrListIn(e, r) =>
        strListTypes(e) = r
      case e =>
        conds += e
    conds.foreach(narrowCond(using _))
    newGoal()

  private def narrowCond(using cond: Expr): Unit =
    matchCharCond(cond).map(narrowCharTarget(_, _))
      .orElse { matchStrCond(cond).map(narrowStrTarget(_, _)) }
      .orElse { matchStrListCond(cond).map(narrowStrListTarget(_, _)) }

  private def narrowCharTarget(expr: Expr, f: CharSet => CharSet)(using cond: Expr): Unit = expr match
    case CharAt(e, ei) =>
      val index = IndexInferer.inferStrIndex(ei, e, goal)
      // Case 1: determined index
      if index.isInstanceOf[AIndex] then
        narrowStrTarget(e, StrREAPI.filterCharWith(index, f))
        return

      // Case 2: index `x + k` relative to suffix `e[x:]`
      for
        (x, k) <- ei match
          case Var(x) => Some((x, 0))
          case Add(Var(x), IntConst(k)) => Some((x, k.toInt))
          case _ => None
        suffix = Substr(e, Var(x), StrLen(e))
        r0 <- strTypes.get(suffix)
      do
        val r1 = NarrowCache.getOrElseUpdate(suffix, r0, cond, StrREAPI.filterCharWith(AIndex.Const(k), f)(r0))
        strTypes(suffix) = r1
        logger.debug("Narrow {} by {} to {}", suffix.show, cond.show, r1.pp)
        return

      // Case 3: nondetermined index
      narrowStrTarget(e, StrREAPI.filterCharWith(index, f))
      val a0 = charTypes.getOrElse(expr, Inferer.inferChar(expr, newGoal()))
      val a1 = NarrowCache.getOrElseUpdate(expr, a0, cond, f(a0))
      charTypes(expr) = a1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, a1)

    case _ =>
      val a0 = charTypes.getOrElse(expr, Inferer.inferChar(expr, newGoal()))
      val a1 = NarrowCache.getOrElseUpdate(expr, a0, cond, f(a0))
      charTypes(expr) = a1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, a1)

  @tailrec
  private def narrowStrTarget(expr: Expr, f: StrRE => StrRE)(using cond: Expr): Unit = expr match
    case Substr(e, ei, ej) =>
      (IndexInferer.inferStrIndex(ei, e, goal), IndexInferer.inferStrIndex(ej, e, goal)) match
        case (from: AIndex, until: AIndex) => narrowStrTarget(e, StrREAPI.filterSubstrWith(from, until, f))
        case _ => logger.warn("Cannot narrow {} from {} until {}", expr.show, ei.show, ej.show)
    case ElemAt(e, ei) =>
      val index = IndexInferer.inferListIndex(ei, e, goal)
      narrowStrListTarget(e, ListREAPI.filterElemWith(index, f))
    case StrConst(_) => // ignore
    case _ =>
      val r0 = strTypes.getOrElse(expr, Inferer.inferStr(expr, newGoal()))
      val r1 = NarrowCache.getOrElseUpdate(expr, r0, cond, f(r0))
      strTypes(expr) = r1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, r1.pp)

  @tailrec
  private def narrowStrListTarget(expr: Expr, f: ListRE => ListRE)(using cond: Expr): Unit = expr match
    case ListSlice(e, ei, ej) =>
      (IndexInferer.inferListIndex(ei, e, goal), IndexInferer.inferListIndex(ej, e, goal)) match
        case (from: AIndex, until: AIndex) => narrowStrListTarget(e, ListREAPI.filterSliceWith(from, until, f))
        case _ => logger.warn("Cannot narrow {} from {} until {}", expr.show, ei.show, ej.show)
    case _ =>
      val r0 = strListTypes.getOrElse(expr, Inferer.inferStrList(expr, newGoal()))
      val r1 = NarrowCache.getOrElseUpdate(expr, r0, cond, f(r0))
      strListTypes(expr) = r1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, r1.pp)

  private def newGoal(): Goal =
    goal.copy(premises = goal.premises
      ++ (for (e, a) <- charTypes yield CharIn(e, a)())
      ++ (for (e, r) <- strTypes yield StrIn(e, r)())
      ++ (for (e, r) <- strListTypes yield StrListIn(e, r)()))

  private def matchCharCond(cond: Expr): Option[(Expr, CharSet => CharSet)] = cond match
    // e ∈ a
    case CharIn(e, a) => Some((e, _ & a))
    case Not(CharIn(e, a)) => Some((e, _ & ~a))
    // e == c
    case Eq(e, CharConst(c)) => Some((e, _ & CharSet(c)))
    case Ne(e, CharConst(c)) => Some((e, _ - c))
    // e in cs
    case StrContains(StrConst(cs), CharToStr(e)) => Some((e, _ & CharSet.from(cs)))
    case Not(StrContains(StrConst(cs), CharToStr(e))) => Some((e, _ & ~CharSet.from(cs)))
    case _ => None

  private def matchStrCond(cond: Expr): Option[(Expr, StrRE => StrRE)] = cond match
    // e == s
    case Eq(e, StrConst(s)) => Some((e, StrREAPI.filterEq(s)))
    case Ne(e, StrConst(s)) => Some((e, StrREAPI.filterNe(s)))
    // s in e
    case StrContains(e, StrConst(s)) => Some((e, StrREAPI.filterContains(s)))
    case Not(StrContains(e, StrConst(s))) => Some((e, StrREAPI.filterNotContain(s)))
    // |e| == n
    case StrLenCmp(e, op, i) => op match
      case CmpOp.EQ =>
        if i >= 0 then Some((e, StrREAPI.filterLengthEq(i))) else Some((e, _ => RegEx.Zero()))
      case CmpOp.NE =>
        if i == 0 then Some((e, StrREAPI.filterLengthGe(1)))
        else if i > 0 then Some((e, StrREAPI.filterLengthNe(i)))
        else None
      case CmpOp.LE =>
        if i >= 0 then Some((e, StrREAPI.filterLengthLe(i))) else Some((e, _ => RegEx.Zero()))
      case CmpOp.LT =>
        if i > 0 then Some((e, StrREAPI.filterLengthLe(i - 1))) else Some((e, _ => RegEx.Zero()))
      case CmpOp.GE =>
        if i > 0 then Some((e, StrREAPI.filterLengthGe(i))) else None
      case CmpOp.GT =>
        if i >= 0 then Some((e, StrREAPI.filterLengthGe(i + 1))) else None

    // e.startsWith(s)
    case StrStartsWith(e, StrConst(s)) => Some((e, StrREAPI.filterStartsWith(s)))
    case Not(StrStartsWith(e, StrConst(s))) => Some((e, StrREAPI.filterNotStartWith(s)))
    case StrEndsWith(e, StrConst(s)) => Some((e, StrREAPI.filterEndsWith(s)))
    case Not(StrEndsWith(e, StrConst(s))) => Some((e, StrREAPI.filterNotEndWith(s)))

    // e.indexOf(c1, i) < e.indexOf(c2, i)
    case Cmp(StrIndexOf(e, StrConst(s1), IntConst(i)), CmpOp.LT | CmpOp.LE,
      StrIndexOf(e1, StrConst(s2), IntConst(i1)))
      if e1 == e && i1 == i && s1.length == 1 && s2.length == 1 && s1 != s2 =>
      val c1 = s1.head
      val c2 = s2.head
      Some((e, r => r.filterIndexOfLt(c1, c2, i.toInt)))

    // e.indexOf(s, i) == k
    case StrIndexOfCmp(e, s, i, op, e1) => (op, e1) match
      case (CmpOp.EQ, IntConst(k)) =>
        if k >= 0 then Some((e, StrREAPI.filterIndexOfEq(s, i, k.toInt)))
        else if k == -1 then Some((e, StrREAPI.filterIndexOfNeg(s, i)))
        else Some((e, _ => RegEx.Zero()))
      case (CmpOp.NE, IntConst(-1)) =>
        Some((e, StrREAPI.filterIndexOfGe(s, i, 0)))
      case (CmpOp.NE, Sub(StrLen(`e`), IntConst(1))) =>
        Some((e, StrREAPI.filterIndexOfNeLast(s, i)))
      case (CmpOp.LE, IntConst(k)) =>
        if k >= 0 then Some((e, StrREAPI.filterIndexOfLe(s, i, k.toInt)))
        else if k == -1 then Some((e, StrREAPI.filterIndexOfNeg(s, i)))
        else Some((e, _ => RegEx.Zero()))
      case (CmpOp.LT, IntConst(k)) =>
        if k > 0 then Some((e, StrREAPI.filterIndexOfLe(s, i, k.toInt - 1)))
        else if k == 0 then Some((e, StrREAPI.filterIndexOfNeg(s, i)))
        else Some((e, _ => RegEx.Zero()))
      case (CmpOp.GE, IntConst(k)) =>
        if k >= 0 then Some((e, StrREAPI.filterIndexOfGe(s, i, k.toInt)))
        else None
      case (CmpOp.GT, IntConst(k)) =>
        if k >= -1 then Some((e, StrREAPI.filterIndexOfGe(s, i, k.toInt + 1)))
        else None
      case (CmpOp.EQ | CmpOp.GE | CmpOp.GT, StrLen(`e`)) => Some((e, _ => RegEx.Zero()))
      case (CmpOp.EQ | CmpOp.GE | CmpOp.GT, Add(StrLen(`e`), IntConst(i))) if i > 0 => Some((e, _ => RegEx.Zero()))
      case _ => None

    // e.forall(lambda x -> ex)
    case StrForall(e, LambdaExpr(List(x -> _), ex)) =>
      matchCharCond(ex) match
        case Some((Var(`x`), f)) => Some((e, StrREAPI.filterForall(f)))
        case _ => None
    case Not(StrForall(e, LambdaExpr(List(x -> _), ex))) =>
      matchCharCond(Not(ex)) match
        case Some((Var(`x`), f)) => Some((e, StrREAPI.filterExists(f)))
        case _ => None
    case _ => None

  object Cmp:
    def unapply(expr: Expr): Option[(Expr, CmpOp, Expr)] = expr match
      case Eq(e1, e2) => Some((e1, CmpOp.EQ, e2))
      case Ne(e1, e2) => Some((e1, CmpOp.NE, e2))
      case Le(e1, e2) => Some((e1, CmpOp.LE, e2))
      case Lt(e1, e2) => Some((e1, CmpOp.LT, e2))
      case _ => None

  object StrLenCmp:
    def unapply(expr: Expr): Option[(Expr, CmpOp, Int)] =
      for
        (e1, op, e2) <- Cmp.unapply(expr)
        len <- expr.collectFirst { case e@StrLen(_) => e }
        (resultOp, result) <- InequalitySolver.isolate(len, (e1, op, e2))
        i <- result match {case IntConst(i) => Some(i); case _ => None}
      yield (len.str, resultOp, i.toInt)

  object StrIndexOfCmp:
    def unapply(expr: Expr): Option[(Expr, String, Int, CmpOp, Expr)] =
      for
        (e1, op, e2) <- Cmp.unapply(expr)
        (target, e, s, i) <- expr.collectFirst:
          case target@StrIndexOf(e, StrConst(s), IntConst(i)) if i >= 0 => (target, e, s, i.toInt)
        (resultOp, result) <- InequalitySolver.isolate(target, (e1, op, e2))
      yield (e, s, i, resultOp, result)

  private def matchStrListCond(cond: Expr): Option[(Expr, ListRE => ListRE)] = cond match
    // s in e
    case ListContains(e, StrConst(s)) => Some((e, ListREAPI.filterContains(s)))
    case Not(ListContains(e, StrConst(s))) => Some((e, ListREAPI.filterNotContain(s)))
    case _ => None

object NarrowCache:
  private val cache = mutable.Map.empty[(Expr, Any, Expr), Any]
  private var hit = 0
  private var miss = 0

  def getOrElseUpdate[T](target: Expr, old: T, cond: Expr, result: => T): T =
    cache.get((target, old, cond)) match
      case Some(value) =>
        hit += 1
        value.asInstanceOf[T]
      case None =>
        miss += 1
        val value = result
        cache((target, old, cond)) = value
        value

  def hitRate: Double =
    val total = hit + miss
    if total == 0 then 0.0 else hit.toDouble / total