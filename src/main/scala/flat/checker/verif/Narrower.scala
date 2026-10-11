package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.*
import flat.checker.domain.{StrRE, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

import scala.collection.mutable

object Narrower extends LazyLogging:
  def narrow(goal: Goal): Goal =
    val ctx = mutable.Map.from(goal.premises.collect { case StrIn(e, r) => e -> r })
    for premise <- goal.premises; (e, r) <- process(premise, ctx.toMap) do
      ctx(e) = r
      logger.debug("Narrow {} to {} by {}", e.show, r.pp, premise.show)
    goal.copy(premises = goal.premises ++ ctx.map { (e, r) => StrIn(e, r)() })

  private def process(premise: Expr, ctx: Map[Expr, StrRE]): Option[(Expr, StrRE)] = premise match
    // prefix, suffix
    case StrStartsWith(e, StrConst(t)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterStartsWith(t.toList))
    case Eq(Substr(e, IntConst(0), IntConst(n)), StrConst(t)) if t.length == n && ctx.contains(e) =>
      Some(e -> ctx(e).filterStartsWith(t.toList))
    case Not(StrStartsWith(e, StrConst(t))) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotStartWith(t.toList))
    case Ne(Substr(e, IntConst(0), IntConst(n)), StrConst(t)) if t.length == n && ctx.contains(e) =>
      Some(e -> ctx(e).filterNotStartWith(t.toList))
    case StrEndsWith(e, StrConst(t)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterEndsWith(t.toList))
    case Not(StrEndsWith(e, StrConst(t))) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotEndWith(t.toList))
    // equality
    case Eq(e, StrConst(t)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterEq(t.toList))
    case Ne(e, StrConst(t)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNe(t.toList))
    // contains
    case StrContains(e, StrConst(t)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterContains(t.toList))
    case Ne(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(-1)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterContains(t.toList))
    case Le(IntConst(0), StrIndexOf(e, StrConst(t), IntConst(0))) if ctx.contains(e) =>
      Some(e -> ctx(e).filterContains(t.toList))
    case Lt(IntConst(0), Add(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(1))) if ctx.contains(e) =>
      Some(e -> ctx(e).filterContains(t.toList))
    case Ne(Add(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterContains(t.toList))
    // not contain
    case Not(StrContains(e, StrConst(t))) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotContain(t.toList))
    case Eq(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(-1)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotContain(t.toList))
    case Lt(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(0)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotContain(t.toList))
    case Eq(Add(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotContain(t.toList))
    case Le(Add(StrIndexOf(e, StrConst(t), IntConst(0)), IntConst(1)), IntConst(0)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNotContain(t.toList))
    // indexOf order
    case Lt(StrIndexOf(e, StrConst(s1), IntConst(0)), StrIndexOf(e1, StrConst(s2), IntConst(0)))
      if e1 == e && s1.length == 1 && s2.length == 1 && s1 != s2 && ctx.contains(e) =>
      Some(e -> ctx(e).filterIndexOfLt(s1.head, s2.head))
    case Le(StrIndexOf(e, StrConst(s1), IntConst(0)), StrIndexOf(e1, StrConst(s2), IntConst(0)))
      if e1 == e && s1.length == 1 && s2.length == 1 && s1 != s2 && ctx.contains(e) =>
      Some(e -> ctx(e).filterIndexOfLt(s1.head, s2.head))
    // emptiness
    case Lt(IntConst(0), StrLength(e)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNonEmpty)
    case Ne(StrLength(e), IntConst(0)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterNonEmpty)
    // charAt
    case Eq(CharAt(e, IntConst(i)), CharConst(c)) if ctx.contains(e) =>
      Some(e -> ctx(e).filterElemAtEq(i.intValue, c))
    case Eq(Substr(e, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 && ctx.contains(e) =>
      Some(e -> ctx(e).filterElemAtEq(i.intValue, s.head))
    case Ne(CharAt(e, IntConst(i)), CharConst(c)) => Some(e -> ctx(e).filterElemAtNe(i.intValue, c))
    case Ne(Substr(e, IntConst(i), IntConst(j)), StrConst(s)) if j == i + 1 && s.length == 1 && ctx.contains(e) =>
      Some(e -> ctx(e).filterElemAtNe(i.intValue, s.head))
    // Length
    case Le(StrLength(e), IntConst(n)) if 0 <= n && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthLe(n.intValue))
    case Lt(StrLength(e), IntConst(n)) if 1 <= n && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthLe(n.intValue - 1))
    case Le(IntConst(n), StrLength(e)) if 0 <= n && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthGe(n.intValue))
    case Lt(IntConst(n), StrLength(e)) if -1 <= n && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthGe(n.intValue + 1))
    case Eq(StrLength(e), IntConst(n)) if 0 <= n && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthEq(n.intValue))
    case Ne(Substr(e, IntConst(i), IntConst(j)), StrConst("")) if 0 <= i && i + 1 == j && ctx.contains(e) =>
      Some(e -> ctx(e).filterLengthGe(j.intValue))
    // otherwise
    case _ => None