package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.RegEx.sum
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object NarrowerCache:
  private val strCache = mutable.Map.empty[(StrRE, Expr), StrRE]
  private val listCache = mutable.Map.empty[(RegEx[StrRE], Expr), RegEx[StrRE]]
  private var hit = 0
  private var miss = 0

  def getOrElseUpdate(input: StrRE, premise: Expr, result: => StrRE): StrRE =
    strCache.get((input, premise)) match
      case Some(r) =>
        hit += 1
        r
      case None =>
        miss += 1
        val r1 = result
        strCache((input, premise)) = r1
        r1

  def getOrElseUpdateList(input: RegEx[StrRE], premise: Expr, result: => RegEx[StrRE]): RegEx[StrRE] =
    listCache.get((input, premise)) match
      case Some(r) =>
        hit += 1
        r
      case None =>
        miss += 1
        val r1 = result
        listCache((input, premise)) = r1
        r1

  def hitRate: Double =
    val total = hit + miss
    if total == 0 then 0.0 else hit.toDouble / total

class Narrower(goal: Goal) extends LazyLogging:
  private val strs = mutable.Map.empty[Expr, StrRE]
  private val lists = mutable.Map.empty[Expr, RegEx[StrRE]]
  private val used = ListBuffer.empty[Expr]

  def narrow: Goal =
    for premise <- goal.premises; (e, f) <- matchList.lift(premise) do
      if isTarget(e) then
        used += premise
        val r0 = getList(e)
        val r = NarrowerCache.getOrElseUpdateList(r0, premise, f(r0))
        logger.debug("Narrow {} by {} to {}", e.show, premise.show, r.pp)
        lists(e) = r

    for premise <- goal.premises; if !used.contains(premise); (e, f) <- matchPremise.lift(premise) do
      e match
        case Substr(e, IntConst(0), StrIndexOf(e1, StrConst(t), IntConst(0)))
          if e1 == e && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterPrefixUntilIndexOf(t, r0, f))
          logger.debug("Narrow {} by {} to {}", e.show, premise.show, r.pp)
          strs(e) = r
        case Substr(e, StrIndexOf(e1, StrConst(t), IntConst(0)), StrLength(e2))
          if e1 == e && e2 == e && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterSuffixFromIndexOf(t, 0, r0, f))
          logger.debug("Narrow {} by {} to {}", e.show, premise.show, r.pp)
          strs(e) = r
        case Substr(e, Add(StrIndexOf(e1, StrConst(t), IntConst(0)), IntConst(k)), StrLength(e2))
          if e1 == e && e2 == e && 0 <= k && k <= t.length && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterSuffixFromIndexOf(t, k.toInt, r0, f))
          logger.debug("Narrow {} by {} to {}", e.show, premise.show, r.pp)
          strs(e) = r
        case _ =>
          if isTarget(e) then
            used += premise
            val r0 = get(e)
            val r = NarrowerCache.getOrElseUpdate(r0, premise, f(r0))
            logger.debug("Narrow {} by {} to {}", e.show, premise.show, r.pp)
            strs(e) = r

    currentGoal()

  private def isTarget(expr: Expr): Boolean =
    strs.contains(expr)
      || goal.premises.exists { case StrIn(`expr`, _) => true; case _ => false }
      || goal.conclusion.collectFirst { case `expr` => true }.isDefined

  private def get(expr: Expr): StrRE =
    if strs.contains(expr) then
      return strs(expr)

    goal.premises.diff(used).reverse.collectFirst { case StrIn(`expr`, r) => r }
      .getOrElse:
        Inferer.infer(expr)(using currentGoal()) match
          case Domain.Str(r) => r
          case _ => RegEx.full

  private def getList(expr: Expr): RegEx[StrRE] =
    if lists.contains(expr) then
      return lists(expr)

    Inferer.infer(expr)(using currentGoal()) match
      case Domain.StrList(r) => r
      case _ => RegEx.full[StrRE]

  private def currentGoal(): Goal =
    goal.copy(premises = goal.premises.diff(used)
      ++ strs.map { (e, r) => StrIn(e, r)() }
      ++ lists.map { (e, r) => StrListIn(e, r)() })

  private def matchPremise: PartialFunction[Expr, (Expr, StrRE => StrRE)] = {
    // Length
    case Eq(StrLength(e), IntConst(n)) if n >= 0 => (e, _.filterLengthEq(n.toInt))
    case Ne(StrLength(e), IntConst(0)) => (e, _.filterNonEmpty)
    case Le(StrLength(e), IntConst(n)) if n >= 0 => (e, _.filterLengthLe(n.toInt))
    case Lt(StrLength(e), IntConst(n)) if n >= 1 => (e, _.filterLengthLe(n.toInt - 1))
    case Le(IntConst(n), StrLength(e)) if n >= 0 => (e, _.filterLengthGe(n.toInt))
    case Lt(IntConst(n), StrLength(e)) if n >= -1 => (e, _.filterLengthGe(n.toInt + 1))

    // CharAt: e[i] == c, e[i] != c, e[i] in cs, e[i] !in cs
    case Eq(CharAt(e, IntConst(i)), CharConst(c)) => (e, _.filterElemAtEq(i.toInt, c))
    case Eq(Substr(e, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 =>
      (e, _.filterElemAtEq(i.toInt, s.head))
    case Ne(CharAt(e, IntConst(i)), CharConst(c)) => (e, _.filterElemAtNe(i.toInt, c))
    case Ne(Substr(e, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 =>
      (e, _.filterElemAtNe(i.toInt, s.head))
    case StrContains(CharAt(e, IntConst(i)), StrConst(cs)) => (e, _.filterElemWith(i.toInt, _ & CharSet.from(cs)))
    case Not(StrContains(CharAt(e, IntConst(i)), StrConst(cs))) => (e, _.filterElemWith(i.toInt, _ -- cs))

    // Contain: e.contains(t)
    case StrContains(e, StrConst(t)) => (e, _.filterContains(t.toList))
    case Not(StrContains(e, StrConst(t))) => (e, _.filterNotContain(t.toList))

    // StartsWith: e.startsWith(t), !e.startsWith(t), e[:|t|] == t, e[:|t|] != t
    case StrStartsWith(e, StrConst(t)) => (e, _.filterStartsWith(t.toList))
    case Not(StrStartsWith(e, StrConst(t))) => (e, _.filterNotStartWith(t.toList))
    case Eq(Substr(e, IntConst(0), IntConst(n)), StrConst(t)) if t.length == n => (e, _.filterStartsWith(t.toList))
    case Ne(Substr(e, IntConst(0), IntConst(n)), StrConst(t)) if t.length == n => (e, _.filterNotStartWith(t.toList))

    // EndsWith: e.endsWith(t), !e.endsWith(t)
    case StrEndsWith(e, StrConst(t)) => (e, _.filterEndsWith(t.toList))
    case Not(StrEndsWith(e, StrConst(t))) => (e, _.filterNotEndWith(t.toList))

    // Equality: e == t, e != t
    case Eq(e, StrConst(t)) => (e, _.filterEq(t.toList))
    case Ne(e, StrConst(t)) => (e, _.filterNe(t.toList))

    // IndexOf: e.indexOf(t, i) <= n
    case Eq(StrIndexOf(e, StrConst(t), IntConst(i)), IntConst(n)) =>
      if n >= 0 then (e, _.filterIndexOfEq(t.toList, i.toInt, n.toInt))
      else if n == -1 then (e, _.filterIndexOfNeg(t.toList, i.toInt))
      else (e, _ => RegEx.Zero())
    case Le(StrIndexOf(e, StrConst(t), IntConst(i)), IntConst(n)) =>
      if n >= 0 then (e, _.filterIndexOfLe(t.toList, i.toInt, n.toInt))
      else if n == -1 then (e, _.filterIndexOfNeg(t.toList, i.toInt))
      else (e, _ => RegEx.Zero())
    case Lt(StrIndexOf(e, StrConst(t), IntConst(i)), IntConst(n)) =>
      if n >= 1 then (e, _.filterIndexOfLe(t.toList, i.toInt, n.toInt - 1))
      else if n == 0 then (e, _.filterIndexOfNeg(t.toList, i.toInt))
      else (e, _ => RegEx.Zero())
    case Le(IntConst(n), StrIndexOf(e, StrConst(t), IntConst(i))) =>
      if n >= 0 then (e, _.filterIndexOfGe(t.toList, i.toInt, n.toInt))
      else (e, identity)
    case Le(StrLength(e), StrIndexOf(e1, StrConst(_), IntConst(_))) if e1 == e => (e, _ => RegEx.Zero())
    case Lt(IntConst(n), StrIndexOf(e, StrConst(t), IntConst(i))) =>
      if n >= -1 then (e, _.filterIndexOfGe(t.toList, i.toInt, n.toInt + 1))
      else (e, identity)
    case Lt(StrLength(e), StrIndexOf(e1, StrConst(_), IntConst(_))) if e1 == e => (e, _ => RegEx.Zero())

    // IndexOf: e.indexOf(c1) < e.indexOf(c2)
    case Lt(StrIndexOf(e, StrConst(t1), IntConst(0)), StrIndexOf(e1, StrConst(t2), IntConst(0)))
      if e1 == e && t1.length == 1 && t2.length == 1 && t1 != t2 =>
      (e, _.filterIndexOfLt(t1.head, t2.head))
    case Le(StrIndexOf(e, StrConst(t1), IntConst(0)), StrIndexOf(e1, StrConst(t2), IntConst(0)))
      if e1 == e && t1.length == 1 && t2.length == 1 && t1 != t2 =>
      (e, _.filterIndexOfLt(t1.head, t2.head))

    // Chars in: s in A*
    case StrIn(CharAt(e, IntConst(i)), RegEx.Star(RegEx.Lit(a))) => (e, _.filterElemWith(i.toInt, _ & a))
    case Not(StrIn(CharAt(e, IntConst(i)), RegEx.Star(RegEx.Lit(a)))) => (e, _.filterElemWith(i.toInt, _ & ~a))
    case StrIn(e, RegEx.Star(RegEx.Lit(a))) => (e, _.filterForallCharIn(a))
    case Not(StrIn(e, RegEx.Star(RegEx.Lit(a)))) => (e, _.filterExistsCharIn(~a))
  }

  private val symbolSet =
    given_SymbolSet_RegEx[CharSet].asInstanceOf[SymbolSet[RegEx[CharSet]] {type Symbol = List[Char]}]

  private def matchList: PartialFunction[Expr, (Expr, RegEx[StrRE] => RegEx[StrRE])] = {
    // List select: contains, not contains
    case StrContains(ListSelect(e, IntConst(i)), StrConst(t)) if i >= 0 =>
      (e, _.filterElemWith(i.toInt, _.filterContainsStr(t)))
    case StrContains(ListSelect(e, Sub(ListLength(e1), IntConst(i))), StrConst(t)) if e1 == e && i > 0 =>
      (e, _.filterElemLastWith(-i.toInt, _.filterContainsStr(t)))
    case Not(StrContains(ListSelect(e, IntConst(i)), StrConst(t))) if i >= 0 =>
      (e, _.filterElemWith(i.toInt, _.filterNotContainStr(t)))
    case Not(StrContains(ListSelect(e, Sub(ListLength(e1), IntConst(i))), StrConst(t))) if e1 == e && i > 0 =>
      (e, _.filterElemLastWith(-i.toInt, _.filterNotContainStr(t)))

    // List select: eq, not eq
    case Eq(ListSelect(e, IntConst(i)), StrConst(t)) if i >= 0 =>
      (e, _.filterElemWith(i.toInt, _.filterEq(t.toList)))
    case Eq(ListSelect(e, Sub(ListLength(e1), IntConst(i))), StrConst(t)) if e1 == e && i > 0 =>
      (e, _.filterElemLastWith(-i.toInt, _.filterEq(t.toList)))
    case Ne(ListSelect(e, IntConst(i)), StrConst(t)) if i >= 0 =>
      (e, _.filterElemWith(i.toInt, _.filterNe(t.toList)))
    case Ne(ListSelect(e, Sub(ListLength(e1), IntConst(i))), StrConst(t)) if e1 == e && i > 0 =>
      (e, _.filterElemLastWith(-i.toInt, _.filterNe(t.toList)))
    case Eq(ListSelect(e, Var(x)), StrConst(t))
      if goal.premises.collectFirst {
        case Lt(ListIndexOf(`e`, StrConst(_), IntConst(i)), Var(`x`)) => i >= 0
      }.isDefined
        && goal.premises.collectFirst { case Lt(Var(`x`), Sub(ListLength(`e`), IntConst(_))) => true }.isDefined =>
      val (s, i) =
        goal.premises.collectFirst { case Lt(ListIndexOf(`e`, StrConst(s), IntConst(i)), Var(`x`)) => (s, i) }.get
      val j = goal.premises.collectFirst { case Lt(Var(`x`), Sub(ListLength(`e`), IntConst(j))) => j }.get
      (e, r => REOps.filterSliceWith2(r)(using symbolSet)((List(s.toList), i.toInt, 1), -j.toInt,
        r => REOps.filterContains(r)(using symbolSet)(List(t.toList))))

    // List substr
    case Not(ListContains(ListSlice(e, IntConst(i), Sub(ListLength(e1), IntConst(j))), StrConst(t)))
      if e1 == e && i >= 0 && j > 0 =>
      (e, _.filterSliceWith(i.toInt, -j.toInt, r => REOps.filterNotContainElem(r)(using symbolSet)(t.toList)))
  }

  private def filterPrefixUntilIndexOf(t: String, r: StrRE, f: StrRE => StrRE): StrRE =
    val rt = RegEx.word(t.toList)
    sum(for (r1, r2) <- r.findFirst(t.toList); rf = f(r1); if rf.nonEmpty yield rf * rt * r2)

  private def filterSuffixFromIndexOf(t: String, k: Int, r: StrRE, f: StrRE => StrRE): StrRE =
    require(0 <= k && k <= t.length)
    val rt1 = RegEx.word(t.toList.take(k))
    val rt2 = RegEx.word(t.toList.drop(k))
    sum(for (r1, r2) <- r.findFirst(t.toList); rf = f(rt2 * r2); if rf.nonEmpty yield r1 * rt1 * rf)