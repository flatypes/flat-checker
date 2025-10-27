package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.ast.*
import flat.regex.*
import flat.regex.NarrowOps.*
import flat.regex.RegEx.*

import scala.annotation.tailrec

/** Type narrowing. */
class Narrower(using config: Config) extends LazyLogging:
  /** Performs type narrowing on the given `ctx`. */
  def narrow(ctx: PrfCtx): List[Expr] =
    val res: Map[Expr, RegEx] = Map.from(for x <- ctx.types.strVars yield Var(x) -> ctx.getLang(Var(x)))
    val res1 = iterate(ctx.premises, res)(using ctx)
    List.from(for es -> r <- res1 yield TypeTest(es, LangType(r)))

  @tailrec
  private def iterate(hypotheses: List[Expr], res: Map[Expr, RegEx])(using ctx: PrfCtx): Map[Expr, RegEx] =
    hypotheses match
      case Nil => res
      case e :: rest =>
        val res1 = narrowBy(e, res)
        for
          es <- res1.keys
          if !res.contains(es) || res1(es) != res(es)
        do logger.debug(s"Narrow $es: ${res1(es)}")
        if res1.values.exists(_.isEmpty) then res1
        else iterate(rest, res1)

  private def narrowBy(hypothesis: Expr, res: Map[Expr, RegEx])(using ctx: PrfCtx): Map[Expr, RegEx] =
    hypothesis match
      case Cmp(op, Length(es@Var(_)), Const(n: Int)) =>
        val r = res(es)
        val r1 = mkIntervals(op, n) match
          case List(interval) => r.narrowByLength(interval)
          case List(interval1, interval2) => r.narrowByLength(interval1) | r.narrowByLength(interval2)
          case _ => assert(false)
        res + (es -> r1)
      case Cmp(op, Find(es@Var(_), Const(t: String)), Const(n: Int)) if t.length == 1 =>
        val c = t.head
        val r = res(es)
        val intervals = mkIntervals(op, n)
        val notFound = intervals.exists(_.contains(-1))
        val r1 = intervals.map(_ & Interval()).filterNot(_.isEmpty) match
          case Nil => RENone
          case List(interval) => r.narrowByFirstIndexOf(c, interval)
          case List(interval1, interval2) => r.narrowByFirstIndexOf(c, interval1).narrowByFirstIndexOf(c, interval2)
          case _ => assert(false)
        val r2 = r1 | (if notFound then r.narrowByNotContain(c) else RENone)
        res + (es -> r2)
      case Cmp(op@(EQ | NE), CharAt(es@Var(_), ei), Const(t: String)) if t.length == 1 =>
        val c = t.head
        val cs = op match
          case EQ => CharSet(c)
          case NE => CharSet.not(c)
        val p = for
          (base, r) <- ctx.lookupSuffixLang(es)
          k <- ei.diffNonneg(base)
        yield Substr(es, base, Length(es)) -> r.narrowByChatAt(k, cs)
        if p.isDefined then
          return res + p.get
        // Ordinary
        val indexInferer = new IndexInferer
        val r = res(es)
        val r1 = indexInferer.infer(ei, es) match
          case IndexL(k) => r.narrowByChatAt(k, cs)
          case IndexR(k) if k > 0 => r.reverse.narrowByChatAt(k - 1, cs).reverse
          case _: IndexInterval =>
            op match
              case EQ => r.narrowByContain(c)
              case NE => r.narrowByContainNot(c)
          case _ => r
        res + (es -> r1)
      case Cmp(op@(EQ | NE), es@Var(_), Const(t: String)) =>
        val r = res(es)
        val r1 = op match
          case EQ => r.narrowByEq(t)
          case NE => r.narrowByNotEq(t)
        res + (es -> r1)
      case _ => res

  private def mkIntervals(op: CmpOp, n: Int): List[Interval] = op match
    case EQ => List(Interval.at(n))
    case NE => List(Interval(ub = n - 1), Interval(lb = n + 1))
    case LE => List(Interval(ub = n))
    case LT => List(Interval(ub = n - 1))
    case GE => List(Interval(lb = n))
    case GT => List(Interval(lb = n + 1))
