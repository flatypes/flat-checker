package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.NarrowOps.*
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

  final case class InferTest(test: Expr) extends Sketch:
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
          val r1 = r.narrowByLength(Interval(lb = 1))
          inInterval(SUB(len, eb), r1.length)
        case _ =>
          val inferer = new Inferer
          inInterval(len, inferer.inferLength(str))

  private def inInterval(expr: Expr, interval: Interval): Expr = interval match
    case Interval(0, Inf) => true
    case Interval(n1: Int, Inf) => GE(expr, n1)
    case Interval(n1: Int, n2: Int) => And(GE(expr, n1), LE(expr, n2))
    case _ => assert(false)

  final case class InferFirstIndexOf(str: Expr, pat: String) extends Sketch:
    def apply(using ctx: PrfCtx): Expr =
      val idx = Find(str, pat)
      val inferer = new Inferer
      val (found, indices) = inferer.inferFind(str, pat)
      found match
        case BoolSet.True => mkAnd(indices.flatMap(_.constraint(idx, str)))
        case BoolSet.False => EQ(idx, -1)
        case BoolSet.All => Or(EQ(idx, -1), mkAnd(indices.flatMap(_.constraint(idx, str))))

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
