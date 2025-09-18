package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.RegEx.*

import scala.collection.mutable.ListBuffer

class HintSynth(using ctx: PrfCtx, config: Config) extends LazyLogging:
  private val hints = ListBuffer.empty[Expr]

  private val types = ctx.types

  private val inferer = new Inferer

  private val smtSolver = new SMTSolver

  def collectHints(seed: Expr): List[Expr] =
    val temps = seed.collect {
      case e@Length(_) => HLength(e)
      case e@CharAt(_, _) => HChars(e)
      case e@Find(_, _) => HFind(e)
      case e@InfixOf(_, _) => HTest(e)
      case Cmp(EQ | NE, e, Const(s: String)) => HEq(e, s)
      case e@Var(_) if ctx.exists {
        case Cmp(_, CharAt(_, ei), _) => ei == e
        case Cmp(_, ei, Find(_, _)) => ei == e
      } => HIndex(e)
    }.distinct
    hints.clear()
    for temp <- temps do temp()
    hints.toList.distinct

  sealed trait HintTemp extends LazyLogging:
    def apply(): Unit

  final case class HLength(expr: Length) extends HintTemp:
    def apply(): Unit =
      // Optional attempt
      for
        (eb, r) <- ctx.lookupSuffixLang(expr.str)
        if smtSolver.canProve(mkAnd(GE(eb, 0), LT(eb, expr)))
      do
        val r1 = RERefiner.refineByLen(r, (GT, 0))
        exprIn(SUB(expr, eb), r1.length)
      // Ordinary attempt
      val r = inferer.inferLang(expr.str)
      exprIn(expr, r.length)

  private def exprIn(expr: Expr, interval: Interval): Unit = interval match
    case Interval(n1, n2: Int) if n1 == n2 => hints += EQ(expr, n1)
    case Interval(n1, n2: Int) => hints += mkAnd(GE(expr, n1), LE(expr, n2))
    case Interval(n1, _) => hints += GE(expr, n1)

  final case class HIndex(expr: Var) extends HintTemp:
    def apply(): Unit =
      ctx.foreach {
        case Cmp(EQ, CharAt(es, ei@Var(_)), Const(s: String)) if s.length == 1 =>
          val r = inferer.inferLang(es)
          val c = s.head
          val r1 = r.findPrefix(c)
          if r1 != RENone then
            val k1 = r1.length.lb
            hints += GE(ei, k1)
            val r2 = r.reverse.findSuffix(c)
            val k2 = r2.length.lb
            hints += LT(ei, SUB(Length(es), k2))
        case Cmp(NE, CharAt(es, ei@Var(_)), Const(s: String)) if s.length == 1 =>
          val r = inferer.inferLang(es)
          val c = s.head
          r match
            case REConcat(REStar(RELit(cs)), r1) if cs.isSingletonOf(c) =>
              r1.length.ub match
                case k1: Int => hints += GE(ei, SUB(Length(es), k1))
                case _ =>
            case _ =>
        case Cmp(_, e1, Find(es, Const(s: String))) if e1 == expr && s.length == 1 && ctx.exists(_ == InfixOf(s, es)) =>
          val c = s.head
          ctx.foreach:
            case InfixOf(Const(s1: String), e) if e == es && s1.length == 1 && s1.head != c =>
              val c1 = s1.head
              val r = inferer.inferLang(es)
              if firstOccurBefore(r, c, c1) then
                hints += LT(Find(es, c.toString), Find(es, c1.toString))
              else if firstOccurBefore(r, c1, c) then
                hints += LT(Find(es, c1.toString), Find(es, c.toString))
            case _ =>
        case _ =>
      }

  private def firstOccurBefore(r: RegEx, c1: Char, c2: Char): Boolean = !r.findPrefix(c1).alphabet.contains(c2)

  final case class HChars(expr: CharAt) extends HintTemp:
    def apply(): Unit =
      inferer.inferLang(expr) match
        case RELit(cs) if cs.polarity =>
          val cases = for c <- cs.chars yield EQ(expr, c.toString)
          hints += mkOr(cases.toList)
        case _ => throw InternalError()

  extension (index: Index)
    private def concretize(str: Expr): Expr = index match
      case IndexL(k) => k
      case IndexR(k) => SUB(Length(str), k)
      case IndexAt(t) => Find(str, t)
      case IndexShifted(b, k) => ADD(b.concretize(str), k)
      case IndexInterval(lb, ub) => throw IllegalArgumentException(s"cannot concretize $index")

    /** Generates a Boolean expression that encodes the concrete `idx` (of `str`) is in this abstract `index`. */
    def constraint(idx: Expr, str: Expr): Expr = index match
      case _: BasicIndex | IndexShifted => EQ(idx, concretize(str))
      case IndexInterval(lb, ub) => mkAnd(GE(idx, lb.concretize(str)), LE(idx, ub.concretize(str)))

  final case class HFind(expr: Find) extends HintTemp:
    def apply(): Unit =
      val (found, results) = inferer.inferFind(expr)
      found match
        case BoolSet.True => for index <- results do hints += index.constraint(expr, expr.str)
        case BoolSet.False => hints += EQ(expr, -1)
        case BoolSet.All => for index <- results do hints += mkOr(index.constraint(expr, expr.str), EQ(expr, -1))

  final case class HEq(str: Expr, target: String) extends HintTemp:
    def apply(): Unit =
      val r = inferer.inferLang(str)
      for ss <- AOps.tryEnumerate(r) do
        val cases = for s <- ss yield EQ(str, s)
        hints += mkOr(cases.toList)
      r match
        case RELit(cs) if !cs.polarity =>
          for c <- cs.chars do
            hints += NE(str, c.toString)
        case _ =>

  final case class HTest(expr: InfixOf) extends HintTemp:
    def apply(): Unit =
      inferer.inferTest(expr) match
        case BoolSet.True => hints += expr
        case BoolSet.False => hints += Not(expr)
        case _ =>
