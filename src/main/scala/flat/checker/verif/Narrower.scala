package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Narrower(goal: Goal) extends LazyLogging:
  private val charTypes = mutable.Map.empty[Expr, CharSet]
  private val strTypes = mutable.Map.empty[Expr, CharRE]
  private val strListTypes = mutable.Map.empty[Expr, StrRE]

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
      index match
        case index: StrIndex =>
          narrowStrTarget(e, _.filterElemWith(index, f))
          return
        case _ =>

      // Case 2: index `x + k` relative to suffix `e[x:]`
      for
        (x, k) <- ei match
          case Var(x) => Some((x, 0))
          case Add(Var(x), IntConst(k)) => Some((x, k.toInt))
          case _ => None
        suffix = Substr(e, Var(x), StrLen(e))
        r0 <- strTypes.get(suffix)
      do
        val r1 = NarrowCache.getOrElseUpdate(suffix, r0, cond, r0.filterElemWith(ILeft(k), f))
        strTypes(suffix) = r1
        logger.debug("Narrow {} by {} to {}", suffix.show, cond.show, r1.pp)
        return

      // Case 3: nondetermined index
      val IndexRange(from, until) = index.asInstanceOf[StrIndexRange]
      (from, until) match
        case (ILeft(n), _) =>
          narrowStrTarget(e, _.filterSliceFromLeft(n, until, _.filterExists(f)))
        case (_, IRight(n)) =>
          narrowStrTarget(e, _.filterSliceUntilRight(from, n, _.filterExists(f)))
        case _ =>
          logger.warn("fallback [{}, {}) to [0, -1)", from, until)
          narrowStrTarget(e, _.filterExists(f))

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
  private def narrowStrTarget(expr: Expr, f: CharRE => CharRE)(using cond: Expr): Unit = expr match
    case Substr(e, ei, ej) =>
      (IndexInferer.matchStrIndex(ei, e, goal), IndexInferer.matchStrIndex(ej, e, goal)) match
        case (Some(ILeft(n)), Some(until: StrIndex)) => narrowStrTarget(e, _.filterSliceFromLeft(n, until, f))
        case (Some(from: StrIndex), Some(IRight(n))) => narrowStrTarget(e, _.filterSliceUntilRight(from, n, f))
        case _ => logger.warn("Cannot narrow {} from {} until {}", expr.show, ei.show, ej.show)
    case ElemAt(e, ei) =>
      val index = IndexInferer.inferListIndex(ei, e, goal)
      index match
        case index: StrListIndex => narrowStrListTarget(e, _.filterElemWith(index, f))
        case IndexRange(ILeft(n), until) => narrowStrListTarget(e, _.filterSliceFromLeft(n, until, _.filterExists(f)))
        case IndexRange(from, IRight(n)) => narrowStrListTarget(e, _.filterSliceUntilRight(from, n, _.filterExists(f)))
        case IndexRange(from, until) =>
          logger.warn("fallback [{}, {}) to [0, -1)", from, until)
          narrowStrListTarget(e, _.filterExists(f))
    case StrConst(_) => // ignore
    case _ =>
      val r0 = strTypes.getOrElse(expr, Inferer.inferStr(expr, newGoal()))
      val r1 = NarrowCache.getOrElseUpdate(expr, r0, cond, f(r0))
      strTypes(expr) = r1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, r1.pp)

  @tailrec
  private def narrowStrListTarget(expr: Expr, f: StrRE => StrRE)(using cond: Expr): Unit = expr match
    case ListSlice(e, ei, ej) =>
      (IndexInferer.matchListIndex(ei, e, goal), IndexInferer.matchListIndex(ej, e, goal)) match
        case (Some(ILeft(n)), Some(until: StrListIndex)) => narrowStrListTarget(e, _.filterSliceFromLeft(n, until, f))
        case (Some(from: StrListIndex), Some(IRight(n))) => narrowStrListTarget(e, _.filterSliceUntilRight(from, n, f))
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

  given Conversion[String, List[Char]] = _.toList

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

  private def matchStrCond(cond: Expr): Option[(Expr, CharRE => CharRE)] = cond match
    // e == s
    case Eq(e, StrConst(s)) => Some((e, _.filterEq(s)))
    case Ne(e, StrConst(s)) => Some((e, _.filterNe(s)))
    // s in e
    case StrContains(e, StrConst(s)) => Some((e, _.filterContains(s)))
    case Not(StrContains(e, StrConst(s))) => Some((e, _.filterNotContain(s)))
    // |e| op v
    case StrLenCmp(e, op, v) => Some((e, _.filterLengthCmp(op, v)))

    // e.startsWith(s)
    case StrStartsWith(e, StrConst(s)) => Some((e, _.filterStartsWith(s)))
    case Not(StrStartsWith(e, StrConst(s))) => Some((e, _.filterNotStartWith(s)))
    case StrEndsWith(e, StrConst(s)) => Some((e, _.filterEndsWith(s)))
    case Not(StrEndsWith(e, StrConst(s))) => Some((e, _.filterNotEndWith(s)))

    // e.indexOf(c1, i) < e.indexOf(c2, i)
    case Cmp(StrIndexOf(e, StrConst(s1), IntConst(i)), CmpOp.LT | CmpOp.LE,
      StrIndexOf(e1, StrConst(s2), IntConst(j)))
      if e1 == e && s1.length == 1 && s2.length == 1 && s1 != s2 =>
      Some((e, _.filterFindLtFind(s1, i.toInt, IFirst(s2, ILeft(j.toInt), 0))))

    // e.indexOf(s, i) == k
    case StrIndexOfCmp(e, s, i, op, e1) => (op, e1) match
      case (_, IntConst(v)) if i >= 0 => Some((e, _.filterFindCmp(s, ILeft(i), op, v.toInt)))
      case (CmpOp.NE, Sub(StrLen(`e`), IntConst(1))) =>
        Some((e, _.filterFindCmpRight(s, ILeft(i), op, 1)))
      case (CmpOp.EQ | CmpOp.GE | CmpOp.GT, StrLen(`e`)) => Some((e, _ => RegEx.Zero()))
      case (CmpOp.EQ | CmpOp.GE | CmpOp.GT, Add(StrLen(`e`), IntConst(i))) if i > 0 => Some((e, _ => RegEx.Zero()))
      case _ => None

    // e.forall(lambda x -> ex)
    case StrForall(e, LambdaExpr(List(x -> _), ex)) =>
      matchCharCond(ex) match
        case Some((Var(`x`), f)) => Some((e, _.filterForall(f)))
        case _ => None
    case Not(StrForall(e, LambdaExpr(List(x -> _), ex))) =>
      matchCharCond(Not(ex)) match
        case Some((Var(`x`), f)) => Some((e, _.filterExists(f)))
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

  private def matchStrListCond(cond: Expr): Option[(Expr, StrRE => StrRE)] = cond match
    // s in e
    case ListContains(e, StrConst(s)) => Some((e, _.filterContains(List(s))))
    case Not(ListContains(e, StrConst(s))) => Some((e, _.filterNotContainElem(s)))
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