package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.NarrowOps.*
import flat.regex.RegEx.*

import scala.collection.mutable.ListBuffer

/** Lemma Synthesizer. */
final class LemmaSynth(using config: Config) extends LazyLogging:
  /** Lemma Sketch. */
  sealed trait Sketch extends LazyLogging:
    def apply(using ctx: PrfCtx): List[Expr]

  /** Synthesizes lemmas according to the given `sketches`. */
  def synth(sketches: List[Sketch])(using ctx: PrfCtx): List[Expr] = sketches.flatMap(_.apply)

  final case class InferLang(str: Expr, target: Option[String] = None) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val inferer = new Inferer
      val r = inferer.inferLang(str)
      val l1 =
        for
          t <- target
          if !r.contains(t)
        yield NE(str, t)
      val l2 = if r.isSmall then mkOr(r.words.map(EQ(str, _))) else TypeTest(str, LangType(r))
      l1.toList :+ l2

  extension (re: RegEx)
    /** Tests if this regular language is *small*: free of Kleene stars and big CS. */
    private def isSmall: Boolean = re match
      case RENone => true
      case RENull => true
      case RELit(cs) => cs.size <= 20
      case REConcat(r1, r2) => r1.isSmall && r2.isSmall
      case REUnion(r1, r2) => r1.isSmall && r2.isSmall
      case REStar(_) => false

    private def words: Set[String] = re match
      case RENone => Set.empty
      case RENull => Set("")
      case RELit(cs) => cs.toSet.map(_.toString)
      case REConcat(r1, r2) =>
        for
          w1 <- r1.words
          w2 <- r2.words
        yield w1 + w2
      case REUnion(r1, r2) => r1.words | r2.words
      case REStar(_) => Set.empty

  final case class InferTest(test: StrTest) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val inferer = new Inferer
      inferer.inferTest(test) match
        case BoolSet.True => List(test)
        case BoolSet.False => List(Not(test))
        case _ => Nil

  final case class InferLength(str: Expr) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val e = Length(str)
      ctx.lookupSuffixLang(str) match
        case Some((eb, r)) if ctx.isValid(And(GE(eb, 0), LT(eb, e))) =>
          val r1 = r.narrowByLength(Interval(lb = 1))
          List(inInterval(SUB(e, eb), r1.length))
        case _ =>
          val inferer = new Inferer
          val len = inferer.inferLength(str)
          if len == Interval(lb = 0) then Nil else List(inInterval(e, len))

  private def inInterval(expr: Expr, interval: Interval): Expr = interval match
    case Interval(n: Int, Inf) => GE(expr, n)
    case Interval(n1: Int, n2: Int) if n1 == n2 => EQ(expr, n1)
    case Interval(n1: Int, n2: Int) => And(GE(expr, n1), LE(expr, n2))
    case Interval(Inf, _) => assert(false)

  final case class InferFind(str: Expr, pat: String) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val idx = Find(str, pat)
      val inferer = new Inferer
      val (found, indices) = inferer.inferFind(str, pat)
      found match
        case BoolSet.True => indices.map(inIndex(idx, _, str))
        case BoolSet.False => List(EQ(idx, -1))
        case BoolSet.All =>
          indices.filterNot(_ == IndexInterval(IndexL(0), IndexR(1)))
            .map(i => Or(EQ(idx, -1), inIndex(idx, i, str)))

  private def inIndex(idx: Expr, index: Index, str: Expr): Expr = index match
    case _: BasicIndex | IndexShifted => EQ(idx, index.concretize(str))
    case IndexInterval(lb, ub) => And(GE(idx, lb.concretize(str)), LE(idx, ub.concretize(str)))

  final case class InferIndexCharAt(str: Expr, idx: Expr, cs: CharSet) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val inferer = new Inferer
      val r = inferer.inferLang(str)
      val r1 = r.splitPrefix(cs)
      val r2 = r.splitSuffix(cs)
      if !r1.isEmpty && !r2.isEmpty then
        val len1 = r1.length
        val l1 = if len1 == Interval(lb = 0) then Nil else List(inInterval(idx, len1))
        val len2 = r2.length
        val l2 = if len2 == Interval(lb = 1) then Nil else List(negInInterval(idx, str, len2))
        l1 ++ l2
      else
        Nil

  /** |str| - idx in interval */
  private def negInInterval(idx: Expr, str: Expr, interval: Interval): Expr = interval match
    case Interval(n: Int, Inf) => LE(idx, SUB(Length(str), n))
    case Interval(n1: Int, n2: Int) if n1 == n2 => EQ(idx, SUB(Length(str), n1))
    case Interval(n1: Int, n2: Int) => And(GE(idx, SUB(Length(str), n2)), LE(idx, SUB(Length(str), n1)))
    case Interval(Inf, _) => assert(false)

  final case class InferIndexCmpFind(idx: Expr, str: Expr, c: Char) extends Sketch:
    def apply(using ctx: PrfCtx): List[Expr] =
      val inferer = new Inferer
      val r = inferer.inferLang(str)
      val ls = ListBuffer.empty[Expr]
      ctx.premises.foreach:
        case Cmp(_, e1, Find(e2, Const(t: String))) if e1 == idx && e2 == str && t.length == 1 && t.head != c =>
          val c1 = t.head
          if findLT(r, c, c1) then
            ls += LT(Find(str, c.toString), Find(str, c1.toString))
          else if findLT(r, c1, c) then
            ls += LT(Find(str, c1.toString), Find(str, c.toString))
        case _ =>
      ls.toList

  /** Tests if the first index of `c1` is always ''less than'' the first occurrence of `c2`. */
  private def findLT(re: RegEx, c1: Char, c2: Char): Boolean = !re.findPrefix(c1).alphabet.contains(c2)