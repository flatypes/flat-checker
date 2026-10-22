package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.domain.Prettifier.pp
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.annotation.tailrec
import scala.collection.mutable

class Narrower(goal: Goal) extends LazyLogging:
  private val charTypes = mutable.Map.empty[Expr, CharSet]
  private val strTypes = mutable.Map.empty[Expr, StrRE]
  private val strListTypes = mutable.Map.empty[Expr, ListRE]

  def narrow: Goal =
    goal.premises.foreach:
      case CharIn(e, a) =>
        charTypes(e) = a
      case StrIn(e, r) =>
        strTypes(e) = r
      case StrListIn(e, r) =>
        strListTypes(e) = r
      case _ =>

    goal.premises.foreach(narrowCond(using _))
    newGoal()

  private def narrowCond(using cond: Expr): Unit = cond match
    // Char equality
    case Eq(e, CharConst(c)) => narrowChar(e, _ & CharSet(c))
    case Ne(e, CharConst(c)) => narrowChar(e, _ - c)
    // Str equality
    case Eq(e, StrConst(s)) => narrowStr(e, StrREAPI.filterEq(s))
    case Ne(e, StrConst(s)) => narrowStr(e, StrREAPI.filterNe(s))
    // Char membership
    case CharIn(e, a) => narrowChar(e, _ & a)
    case Not(CharIn(e, a)) => narrowChar(e, _ & ~a)
    // Length inequalities
    case Eq(StrLength(e), IntConst(n)) => narrowStr(e, StrREAPI.filterLengthEq(n.toInt))
    case Le(StrLength(e), IntConst(n)) => narrowStr(e, StrREAPI.filterLengthLe(n.toInt))
    case Le(IntConst(n), StrLength(e)) => narrowStr(e, StrREAPI.filterLengthGe(n.toInt))
    case Lt(StrLength(e), IntConst(n)) => narrowStr(e, StrREAPI.filterLengthLe(n.toInt - 1))
    case Lt(IntConst(n), StrLength(e)) => narrowStr(e, StrREAPI.filterLengthGe(n.toInt + 1))
    // Contains
    case StrContains(e, CharConst(c)) => narrowStr(e, StrREAPI.filterContains(c))
    case StrContains(e, StrConst(s)) => narrowStr(e, StrREAPI.filterContains(s))
    case Not(StrContains(e, CharConst(c))) => narrowStr(e, StrREAPI.filterNotContain(c))
    case Not(StrContains(e, StrConst(s))) => narrowStr(e, StrREAPI.filterNotContain(s))
    // Prefix & Suffix
    case StrStartsWith(e, StrConst(s)) => narrowStr(e, StrREAPI.filterStartsWith(s))
    case Not(StrStartsWith(e, StrConst(s))) => narrowStr(e, StrREAPI.filterNotStartWith(s))
    case StrEndsWith(e, StrConst(s)) => narrowStr(e, StrREAPI.filterEndsWith(s))
    case Not(StrEndsWith(e, StrConst(s))) => narrowStr(e, StrREAPI.filterNotEndWith(s))
    // IndexOf is negative
    case Eq(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(-1)) =>
      narrowStr(e, StrREAPI.filterIndexOfNeg(t, k.toInt))
    case Le(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(-1)) =>
      narrowStr(e, StrREAPI.filterIndexOfNeg(t, k.toInt))
    case Lt(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(0)) =>
      narrowStr(e, StrREAPI.filterIndexOfNeg(t, k.toInt))
    // IndexOf inequalities
    case Eq(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(n)) if n >= 0 =>
      narrowStr(e, StrREAPI.filterIndexOfEq(t, k.toInt, n.toInt))
    case Le(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(n)) if n >= 0 =>
      narrowStr(e, StrREAPI.filterIndexOfLe(t, k.toInt, n.toInt))
    case Lt(StrIndexOf(e, StrConst(t), IntConst(k)), IntConst(n)) if n > 0 =>
      narrowStr(e, StrREAPI.filterIndexOfLe(t, k.toInt, n.toInt - 1))
    case Le(IntConst(n), StrIndexOf(e, StrConst(t), IntConst(k))) if n >= 0 =>
      narrowStr(e, StrREAPI.filterIndexOfGe(t, k.toInt, n.toInt))
    case Lt(IntConst(n), StrIndexOf(e, StrConst(t), IntConst(k))) if n >= -1 =>
      narrowStr(e, StrREAPI.filterIndexOfGe(t, k.toInt, n.toInt + 1))
    case Le(StrLength(e1), StrIndexOf(e, StrConst(t), IntConst(k))) if e1 == e => // |s| <= s.indexOf(t, k)
      narrowStr(e, r => RegEx.Zero())
    case Lt(StrLength(e1), StrIndexOf(e, StrConst(t), IntConst(k))) if e1 == e => // |s| < s.indexOf(t, k)
      narrowStr(e, r => RegEx.Zero())
    // Forall
    case StrForall(e, LambdaExpr(List(x -> _), StrContains(StrConst(cs), CharToStr(Var(x1))))) if x1 == x =>
      narrowStr(e, StrREAPI.filterForall(_ & CharSet.from(cs)))
    case Not(StrForall(e, LambdaExpr(List(x -> _), StrContains(StrConst(cs), CharToStr(Var(x1)))))) if x1 == x =>
      narrowStr(e, StrREAPI.filterExists(_ & ~CharSet.from(cs)))
    case _ => ()

  private def narrowChar(expr: Expr, f: CharSet => CharSet)(using cond: Expr): Unit = expr match
    case CharAt(e, ei) =>
      IndexInferer.inferStrIndex(ei, e, goal) match
        case AIndex.Left(i) => narrowStr(e, StrREAPI.filterElemWith(i, f))
        case at =>
          throw new UnsupportedOperationException(s"narrow ${expr.show} at $at")
    case _ =>
      val a0 = charTypes.getOrElse(expr, Inferer.inferChar(expr, newGoal()))
      val a1 = NarrowCache.getOrElseUpdate(expr, a0, cond, f(a0))
      charTypes(expr) = a1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, a1)

  @tailrec
  private def narrowStr(expr: Expr, f: StrRE => StrRE)(using cond: Expr): Unit = expr match
    case Substr(e, ei, ej) =>
      (IndexInferer.inferStrIndex(ei, e, goal), IndexInferer.inferStrIndex(ej, e, goal)) match
        case (AIndex.Left(i), AIndex.Right(0)) => narrowStr(e, StrREAPI.filterSliceWith(i, f))
        case (AIndex.Right(i), AIndex.Right(0)) => narrowStr(e, StrREAPI.filterSliceWith(-i, f))
        case (AIndex.Of(s, i, k), AIndex.Right(0)) => narrowStr(e, StrREAPI.filterSliceWith(s, i, k, f))
        case (AIndex.Left(i), AIndex.Left(j)) => narrowStr(e, StrREAPI.filterSliceWith(i, j, f))
        case (AIndex.Left(i), AIndex.Right(j)) => narrowStr(e, StrREAPI.filterSliceWith(i, -j, f))
        case (AIndex.Left(i), AIndex.Of(s, j, k)) => narrowStr(e, StrREAPI.filterSliceWith(i, s, j, k, f))
        case (AIndex.Of(s, i, k), AIndex.Right(j)) => narrowStr(e, StrREAPI.filterSliceWith(s, i, k, -j, f))
        case (start, end) =>
          throw new UnsupportedOperationException(s"narrow ${expr.show} from $start until $end")
    case ListSelect(e, ei) =>
      IndexInferer.inferListIndex(ei, e, goal) match
        case AIndex.Left(i) => narrowStrList(e, ListREAPI.filterElemWith(i, f))
        case AIndex.Right(i) if i > 0 => narrowStrList(e, ListREAPI.filterElemWith(-i, f))
        case AIndexRange(from, until) =>
          val f1 = ListREAPI.filterExists(f)
          (from, until) match
            case (AIndex.Left(i), AIndex.Right(0)) => narrowStrList(e, ListREAPI.filterSliceWith(i, f1))
            case (AIndex.Right(i), AIndex.Right(0)) => narrowStrList(e, ListREAPI.filterSliceWith(-i, f1))
            case (AIndex.Of(s, i, k), AIndex.Right(0)) => narrowStrList(e, ListREAPI.filterSliceWith(s, i, k, f1))
            case (AIndex.Left(i), AIndex.Left(j)) => narrowStrList(e, ListREAPI.filterSliceWith(i, j, f1))
            case (AIndex.Left(i), AIndex.Right(j)) => narrowStrList(e, ListREAPI.filterSliceWith(i, -j, f1))
            case (AIndex.Left(i), AIndex.Of(s, j, k)) => narrowStrList(e, ListREAPI.filterSliceWith(i, s, j, k, f1))
            case (AIndex.Of(s, i, k), AIndex.Right(j)) => narrowStrList(e, ListREAPI.filterSliceWith(s, i, k, -j, f1))
            case (start, end) =>
              throw new UnsupportedOperationException(s"narrow ${expr.show} from $start until $end")
        case at =>
          throw new UnsupportedOperationException(s"narrow ${expr.show} at $at")
    case _ =>
      val r0 = strTypes.getOrElse(expr, Inferer.inferStr(expr, newGoal()))
      val r1 = NarrowCache.getOrElseUpdate(expr, r0, cond, f(r0))
      strTypes(expr) = r1
      logger.debug("Narrow {} by {} to {}", expr.show, cond.show, r1.pp)

  @tailrec
  private def narrowStrList(expr: Expr, f: ListRE => ListRE)(using cond: Expr): Unit = expr match
    case ListSlice(e, IntConst(i), ListLength(e1)) if e1 == e =>
      narrowStrList(e, ListREAPI.filterSliceWith(i.toInt, f))
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