package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.RegEx.*

/** Lemma Synthesizer. */
class LemmaSynth(using config: Config) extends LazyLogging:
  /** Lemma Sketch. */
  sealed trait Sketch extends LazyLogging:
    def apply(using ctx: PrfCtx): Expr

  /** Synthesizes lemmas according to the given `sketches`. */
  def synth(sketches: List[Sketch])(using ctx: PrfCtx): List[Expr] = sketches.map(_.apply)

  final case class InferLang(str: Expr, target: Option[String] = None) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val inferer = new Inferer
      val r = inferer.inferLang(str)
      val e1: Expr = target match
        case Some(t) if !r.contains(t) => NE(str, t)
        case _ => true
      val e2 = if r.isSmall then mkOr(r.words.map(EQ(str, _))) else TypeTest(str, LangType(r))
      mkAnd(e1, e2)

  extension (re: RegEx)
    /** Tests if this regular language is *small*: free of Kleene stars and negative charsets. */
    private def isSmall: Boolean = re match
      case RENone => true
      case RENull => true
      case RELit(cs) => cs.polarity
      case REConcat(r1, r2) => r1.isSmall && r2.isSmall
      case REUnion(r1, r2) => r1.isSmall && r2.isSmall
      case REStar(_) => false

    private def words: Set[String] = re match
      case RENone => Set.empty
      case RENull => Set("")
      case RELit(cs) => if cs.polarity then cs.chars.map(_.toString) else Set.empty
      case REConcat(r1, r2) =>
        for
          w1 <- r1.words
          w2 <- r2.words
        yield w1 + w2
      case REUnion(r1, r2) => r1.words | r2.words
      case REStar(_) => Set.empty

  final case class InferTest(test: InfixOf) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val inferer = new Inferer
      inferer.inferTest(test) match
        case BoolSet.True => test
        case BoolSet.False => Not(test)
        case _ => true

  private val smtSolver = new SMTSolver

  final case class InferLength(str: Expr) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val len = Length(str)
      ctx.lookupSuffixLang(str) match
        case Some((eb, r)) if smtSolver.canProve(And(GE(eb, 0), LT(eb, len))) =>
          val r1 = RERefiner.refineByLen(r, (GT, 0))
          inInterval(SUB(len, eb), r1.length)
        case _ =>
          val inferer = new Inferer
          inInterval(len, inferer.inferLength(str))

  private def inInterval(expr: Expr, interval: Interval): Expr = interval match
    case Interval(0, Inf) => true
    case Interval(n1, Inf) => GE(expr, n1)
    case Interval(n1, n2: Int) => And(GE(expr, n1), LE(expr, n2))

  private def firstOccurBefore(r: RegEx, c1: Char, c2: Char): Boolean =
    !r.findPrefix(c1).alphabet.contains(c2)

  final case class InferFirstIndexOf(str: Expr, pat: String) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val idx = Find(str, pat)
      val inferer = new Inferer
      val (found, indices) = inferer.inferFind(str, pat)
      found match
        case BoolSet.True => mkAnd(indices.flatMap(_.constraint(idx, str)))
        case BoolSet.False => EQ(idx, -1)
        case BoolSet.All => Or(EQ(idx, -1), mkAnd(indices.flatMap(_.constraint(idx, str))))

  extension (index: Index)
    private def concretize(str: Expr): Expr = index match
      case IndexL(k) => k
      case IndexR(k) => SUB(Length(str), k)
      case IndexAt(t) => Find(str, t)
      case IndexShifted(b, k) => ADD(b.concretize(str), k)
      case IndexInterval(lb, ub) => throw IllegalArgumentException(s"cannot concretize $index")

    /** Generates a Boolean expression that encodes the concrete `idx` (of `str`) is in this abstract `index`. */
    private def constraint(idx: Expr, str: Expr): List[Expr] = index match
      case _: BasicIndex | IndexShifted => List(EQ(idx, concretize(str)))
      case IndexInterval(lb, ub) => List(GE(idx, lb.concretize(str)), LE(idx, ub.concretize(str)))

  final case class InferIndex(eq: EQ.type | NE.type, str: Expr, idx: Expr, c: Char) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val inferer = new Inferer
      val r = inferer.inferLang(str)
      val r1 = r.splitPrefix(c)
      val r2 = r.splitSuffix(c)
      if !r1.isEmpty && !r2.isEmpty then
        val e1 = inInterval(idx, r1.length)
        val e2 = inInterval(SUB(Length(str), idx), r2.length)
        eq match
          case EQ => And(e1, e2)
          case NE => Or(Not(e1), Not(e2))
      else
        true
