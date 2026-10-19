package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.RegEx.sum
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{CharSet, RegEx, StrRE, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object NarrowerCache:
  private val cache = mutable.Map.empty[(StrRE, Expr), StrRE]
  private var hit = 0
  private var miss = 0

  def getOrElseUpdate(input: StrRE, premise: Expr, result: => StrRE): StrRE =
    cache.get((input, premise)) match
      case Some(r) =>
        hit += 1
        r
      case None =>
        miss += 1
        val r1 = result
        cache((input, premise)) = r1
        r1

  def hitRate: Double =
    val total = hit + miss
    if total == 0 then 0.0 else hit.toDouble / total

class Narrower(goal: Goal) extends LazyLogging:
  private val ctx = mutable.Map.empty[Expr, StrRE]
  private val used = ListBuffer.empty[Expr]

  def narrow: Goal =
    for premise <- goal.premises; (e, f) <- matchPremise.lift(premise) do
      e match
        case Substr(e, IntConst(0), StrIndexOf(e1, StrConst(t), IntConst(0)))
          if e1 == e && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterPrefixUntilIndexOf(t, r0, f))
          logger.debug("Narrow {} to {} by {}", e.show, r.pp, premise.show)
          ctx(e) = r
        case Substr(e, StrIndexOf(e1, StrConst(t), IntConst(0)), StrLength(e2))
          if e1 == e && e2 == e && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterSuffixFromIndexOf(t, 0, r0, f))
          logger.debug("Narrow {} to {} by {}", e.show, r.pp, premise.show)
          ctx(e) = r
        case Substr(e, Add(StrIndexOf(e1, StrConst(t), IntConst(0)), IntConst(k)), StrLength(e2))
          if e1 == e && e2 == e && 0 <= k && k <= t.length && isTarget(e) =>
          used += premise
          val r0 = get(e)
          val r = NarrowerCache.getOrElseUpdate(r0, premise, filterSuffixFromIndexOf(t, k.toInt, r0, f))
          logger.debug("Narrow {} to {} by {}", e.show, r.pp, premise.show)
          ctx(e) = r
        case _ =>
          if isTarget(e) then
            used += premise
            val r0 = get(e)
            val r = NarrowerCache.getOrElseUpdate(r0, premise, f(r0))
            logger.debug("Narrow {} to {} by {}", e.show, r.pp, premise.show)
            ctx(e) = r
    currentGoal()

  private def isTarget(expr: Expr): Boolean =
    ctx.contains(expr)
      || goal.premises.exists { case StrIn(`expr`, _) => true; case _ => false }
      || goal.conclusion.collectFirst { case `expr` => true }.isDefined

  private def get(expr: Expr): StrRE =
    if ctx.contains(expr) then
      return ctx(expr)

    goal.premises.diff(used).reverse.collectFirst { case StrIn(`expr`, r) => r }
      .getOrElse:
        Inferer.infer(expr)(using currentGoal()) match
          case Domain.Str(r) => r
          case _ => RegEx.full

  private def currentGoal(): Goal =
    goal.copy(premises = goal.premises.diff(used) ++ ctx.map { (e, r) => StrIn(e, r)() })

  private def matchPremise: PartialFunction[Expr, (Expr, StrRE => StrRE)] = {
    // Length
    case Eq(StrLength(e), IntConst(n)) if n >= 0 => (e, _.filterLengthEq(n.toInt))
    case Ne(StrLength(e), IntConst(0)) => (e, _.filterNonEmpty)
    case Le(StrLength(e), IntConst(n)) if n >= 0 => (e, _.filterLengthLe(n.toInt))
    case Lt(StrLength(e), IntConst(n)) if n >= 1 => (e, _.filterLengthLe(n.toInt - 1))
    case Le(IntConst(n), StrLength(e)) if n >= 0 => (e, _.filterLengthGe(n.toInt))
    case Lt(IntConst(n), StrLength(e)) if n >= -1 => (e, _.filterLengthGe(n.toInt + 1))
    // case Ne(Substr(e, IntConst(i), IntConst(j)), StrConst("")) if 0 <= i && i + 1 == j && ctx.contains(e) =>
    // Some(e -> ctx(e).filterLengthGe(j.intValue))

    // CharAt: e[i] == c, e[i] != c, e[i] in cs, e[i] !in cs
    case Eq(CharAt(e, IntConst(i)), CharConst(c)) => (e, _.filterElemAtEq(i.toInt, c))
    // case Eq(Substr(e, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 && ctx.contains(e) =>
    // Some(e -> ctx(e).filterElemAtEq(i.intValue, s.head))
    case Ne(CharAt(e, IntConst(i)), CharConst(c)) => (e, _.filterElemAtNe(i.toInt, c))
    case StrContains(CharAt(e, IntConst(i)), StrConst(cs)) => (e, _.filterElemAt(i.toInt, _ & CharSet.from(cs)))
    case Not(StrContains(CharAt(e, IntConst(i)), StrConst(cs))) => (e, _.filterElemAt(i.toInt, _ -- cs))

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
    case StrIn(CharAt(e, IntConst(i)), RegEx.Star(RegEx.Lit(a))) => (e, _.filterElemAt(i.toInt, _ & a))
    case Not(StrIn(CharAt(e, IntConst(i)), RegEx.Star(RegEx.Lit(a)))) => (e, _.filterElemAt(i.toInt, _ & ~a))
    case StrIn(e, RegEx.Star(RegEx.Lit(a))) => (e, _.filterForallCharIn(a))
    case Not(StrIn(e, RegEx.Star(RegEx.Lit(a)))) => (e, _.filterExistsCharIn(~a))
  }

  private def filterPrefixUntilIndexOf(t: String, r: StrRE, f: StrRE => StrRE): StrRE =
    val rt = RegEx.word(t.toList)
    sum(for (r1, r2) <- r.findFirst(t.toList); rf = f(r1); if rf.nonEmpty yield rf * rt * r2)

  private def filterSuffixFromIndexOf(t: String, k: Int, r: StrRE, f: StrRE => StrRE): StrRE =
    require(0 <= k && k <= t.length)
    val rt1 = RegEx.word(t.toList.take(k))
    val rt2 = RegEx.word(t.toList.drop(k))
    sum(for (r1, r2) <- r.findFirst(t.toList); rf = f(rt2 * r2); if rf.nonEmpty yield r1 * rt1 * rf)