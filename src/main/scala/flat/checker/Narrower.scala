package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.Printer.{ppExpr, ppRE}
import flat.checker.ast.*
import flat.regex.*
import flat.regex.NarrowOps.*
import flat.regex.RegEx.*

import scala.annotation.tailrec

/** Type narrowing. */
class Narrower(using config: Config) extends LazyLogging:
  type Domain = RegEx | AList

  extension (dom: Domain)
    def isEmpty: Boolean = dom match
      case r: RegEx => r.isEmpty
      case l: AList => l.isEmpty

  /** Performs type narrowing on the given `ctx`. */
  def narrow(ctx: PrfCtx): List[Expr] =
    val res: Map[Expr, Domain] = Map.from(for x <- ctx.varCtx.strVars yield Var(x) -> ctx.getLang(Var(x)))
    val res1 = iterate(ctx.premises, res)(using ctx)
    res1.toList.map:
      case (e, r: RegEx) => TypeTest(e, LangType(r))
      case (e, l: AList) => ListHasType(e, l)

  @tailrec
  private def iterate(hypotheses: List[Expr], res: Map[Expr, Domain])(using ctx: PrfCtx): Map[Expr, Domain] =
    hypotheses match
      case Nil => res
      case e :: rest =>
        val res1 = narrowBy(e, res)
        for
          es <- res1.keys
          if !res.contains(es) || res1(es) != res(es)
        do
          logger.debug(s"Narrow $es: ${
            res1(es) match
              case r: RegEx => ppRE(r)
              case _ => "<AList>"
          }")
        if res1.values.exists(_.isEmpty) then res1
        else iterate(rest, res1)

  private def narrowBy(hypothesis: Expr, res: Map[Expr, Domain])(using ctx: PrfCtx): Map[Expr, Domain] =
    hypothesis match
      case Cmp(op, Length(es@Var(_)), Const(n: Int)) =>
        val r = res(es).asInstanceOf[RegEx]
        val r1 = mkIntervals(op, n) match
          case List(interval) => r.narrowByLength(interval)
          case List(interval1, interval2) => r.narrowByLength(interval1) | r.narrowByLength(interval2)
          case _ => assert(false)
        res + (es -> r1)
      case Cmp(op, Find(es@Var(_), Const(t: String)), Const(n: Int)) if t.length == 1 =>
        val c = t.head
        val r = res(es).asInstanceOf[RegEx]
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
        val r = res(es).asInstanceOf[RegEx]
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
        val r = res(es).asInstanceOf[RegEx]
        val r1 = op match
          case EQ => r.narrowByEq(t)
          case NE => r.narrowByNotEq(t)
        res + (es -> r1)

      // narrow AList
      case InfixOf(Const(t: String), ListGet(e, ei)) if t.length == 1 =>
        val c = t.head
        getList(e, res) match
          case Some(list) =>
            val indexInferer = new IndexInferer
            val list1 = indexInferer.infer(ei, e, isStr = false) match
              case IndexL(i) => list.narrow(i, _.narrowByContain(c))
              case IndexR(i) if i > 0 => list.narrowRight(i, _.narrowByContain(c))
              case _ =>
                logger.trace(s"unsupported index: ${ppExpr(ei)} of list ${ppExpr(e)}")
                list
            res + (e -> list1)
          case None => res
      case Not(InfixOf(Const(t: String), ListGet(e, ei))) if t.length == 1 =>
        val c = t.head
        getList(e, res) match
          case Some(list) =>
            val indexInferer = new IndexInferer
            val list1 = indexInferer.infer(ei, e, isStr = false) match
              case IndexL(i) => list.narrow(i, _.narrowByNotContain(c))
              case IndexR(i) if i > 0 => list.narrowRight(i, _.narrowByNotContain(c))
              case _ =>
                logger.trace(s"unsupported index: ${ppExpr(ei)} of list ${ppExpr(e)}")
                list
            res + (e -> list1)
          case None => res
      case Cmp(op@(EQ | NE), ListGet(e, ei), Const(t: String)) =>
        getList(e, res) match
          case Some(list) =>
            val indexInferer = new IndexInferer
            val list1 = indexInferer.infer(ei, e, isStr = false) match
              case IndexL(i) =>
                op match
                  case EQ => list.narrow(i, _.narrowByEq(t))
                  case NE => list.narrow(i, _.narrowByNotEq(t))
              case IndexR(i) if i > 0 =>
                op match
                  case EQ => list.narrowRight(i, _.narrowByEq(t))
                  case NE => list.narrowRight(i, _.narrowByNotEq(t))
              case other =>
                logger.trace(s"unsupported index: $other (${ppExpr(ei)} of list ${ppExpr(e)})")
                list
            res + (e -> list1)
          case None => res
      case Not(ListContains(ListSlice(e, ei, ej), Const(t: String))) =>
        getList(e, res) match
          case Some(list) =>
            val indexInferer = new IndexInferer
            val list1 =
              (indexInferer.infer(ei, e, isStr = false),
                indexInferer.infer(ej, e, isStr = false, preferIndexL = false)) match
                case (IndexL(i), IndexR(j)) if j > 0 => list.narrowEach(i, j, _.narrowByNotEq(t))
                case other =>
                  logger.trace(s"unsupported slice: $other (${ppExpr(ei)}:${ppExpr(ej)} of ${ppExpr(e)})")
                  list
            res + (e -> list1)
          case None => res

      // otherwise
      case _ => res

  private def getList(lst: Expr, res: Map[Expr, Domain])(using ctx: PrfCtx): Option[AList] = res.get(lst) match
    case Some(l: AList) => Some(l)
    case _ =>
      val ctx1 = ctx ++ List.from(res.collect { case (e, l: AList) => ListHasType(e, l) })
      val inferer = new Inferer(using ctx = ctx1)
      inferer.inferStrList(lst) match
        case l: AList => Some(l)
        case _ => None

  private def mkIntervals(op: CmpOp, n: Int): List[Interval] = op match
    case EQ => List(Interval.at(n))
    case NE => List(Interval(ub = n - 1), Interval(lb = n + 1))
    case LE => List(Interval(ub = n))
    case LT => List(Interval(ub = n - 1))
    case GE => List(Interval(lb = n))
    case GT => List(Interval(lb = n + 1))
