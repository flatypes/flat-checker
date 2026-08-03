package flat.checker.verif

import flat.checker.domain.*
import flat.checker.flan.Sort
import flat.checker.flan.tpd.*
import flat.checker.verif.Simplifier.simplify
import flat.checker.verif.Type.*

import scala.collection.mutable.ListBuffer

final case class Goal(premises: List[Expr], conclusion: Expr)
                     (using val sorts: Map[String, Sort]):
  private val solver = SMTSolver()

  private var solverSetup = false

  def have(sides: Expr*): Boolean =
    sides.forall: e =>
      e.simplify match
        case Const(true) => true
        case Const(false) => false
        case e =>
          if !solverSetup then
            premises.foreach(solver.add)
            solverSetup = true
          solver.prove(e)

  def collectFirstInPremises[T](pf: PartialFunction[Expr, T]): Option[T] =
    premises.collectFirst(Function.unlift(_.collectFirst(pf)))

trait LemmaSketch:
  def inst(goal: Goal): Option[Expr]

object LemmaSketches:
  /** If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)` */
  private val stringSliceCount: LemmaSketch = goal =>
    for
      (s, i, j, t) <- goal.conclusion.collectFirst:
        case SeqCount(SeqSlice(e, ei, ej), et) if goal.have(Lt(ei, ej), Eq(SeqLength(et), 1)) => (e, ei, ej, et)
      n <- goal.premises.collectFirst:
        case Eq(SeqSelect(e, ei), ec) if goal.have(Eq(e, s), Eq(ei, i), Eq(CharToString(ec), t)) => 1
        case Ne(SeqSelect(e, ei), ec) if goal.have(Eq(e, s), Eq(ei, i), Eq(CharToString(ec), t)) => 0
    yield
      Eq(SeqCount(SeqSlice(s, i, j), t), Add(SeqCount(SeqSlice(s, Add(i, 1), j), t), n))

  /** !s.trim.contains(" ") */
  private val stringTrimNoWhiteSpace: LemmaSketch = goal =>
    for s <- goal.conclusion.collectFirst { case Not(SeqContains(StringTrim(e), _)) => e }
      yield Not(SeqContains(StringTrim(s), Const(" ")))

  /** If !s.contains(c), then !s.trim.contains(c) */
  private val stringTrimNotContainTrans: LemmaSketch = goal =>
    for
      (s, c) <- goal.conclusion.collectFirst:
        case Not(SeqContains(StringTrim(e), et))
          if goal.have(Eq(SeqLength(et), 1), Not(SeqContains(e, et))) => (e, et)
    yield Not(SeqContains(StringTrim(s), c))

  private val strLength: LemmaSketch = goal =>
    val inferer = Inferer(goal)
    goal.conclusion.collectFirst:
      case e: SeqLength =>
        inferer.infer(e) match
          case TNat(r) =>
            if r.isFinite then
              mkOr(for n <- r.toFinSet.toList.sorted yield Eq(e, Const(n)))
            else
              Le(r.min, e)
          case _ => Const(true)

  private val strPrefix: LemmaSketch = goal =>
    val inferer = Inferer(goal)
    goal.conclusion.collectFirst:
      case e@SeqStartsWith(_, Const(_: String)) =>
        inferer.infer(e) match
          case TBool(BoolSet.True) => e
          case TBool(BoolSet.False) => Not(e)
          case _ => Const(true)
      case e@SeqEndsWith(_, Const(_: String)) =>
        inferer.infer(e) match
          case TBool(BoolSet.True) => e
          case TBool(BoolSet.False) => Not(e)
          case _ => Const(true)

  private val strAt: LemmaSketch = goal =>
    val inferer = Inferer(goal)
    goal.conclusion match
      case Eq(e@SeqSelect(_, _), Const(c: Char)) =>
        inferer.infer(e) match
          case TChar(a) if a.isSingleton && a.contains(c) => Some(goal.conclusion)
          case _ => None
      case Ne(e@SeqSelect(_, _), Const(c: Char)) =>
        inferer.infer(e) match
          case TChar(a) if !a.contains(c) => Some(goal.conclusion)
          case _ => None
      case _ => None

  private val strIndexOf: LemmaSketch = goal =>
    val inferer = Inferer(goal)
    val result = goal.conclusion match
      case Eq(e: SeqIndexOf, Const(_: Int)) => Some(e)
      case Eq(Const(_: Int), e: SeqIndexOf) => Some(e)
      case Ne(e: SeqIndexOf, Const(_: Int)) => Some(e)
      case Ne(Const(_: Int), e: SeqIndexOf) => Some(e)
      case Le(e: SeqIndexOf, Const(_: Int)) => Some(e)
      case Le(Const(_: Int), e: SeqIndexOf) => Some(e)
      case Lt(e: SeqIndexOf, Const(_: Int)) => Some(e)
      case Lt(Const(_: Int), e: SeqIndexOf) => Some(e)
      case _ => None

    result match
      case Some(ei) =>
        inferer.infer(ei) match
          case TIndex(indexSet) =>
            val choices = ListBuffer.empty[Expr]
            for i <- indexSet.neg.toList.sorted do
              choices += Eq(ei, Const(i))
            if indexSet.pos.isFinite then
              for n <- indexSet.pos.toFinSet.toList.sorted do
                choices += Eq(ei, Const(n))
            else
              choices += Le(indexSet.pos.min, ei)
            Some(mkOr(choices.toList))
          case _ => None
      case None => None

  val all: List[LemmaSketch] = List(stringSliceCount, stringTrimNoWhiteSpace, stringTrimNotContainTrans,
    strLength, strPrefix, strAt, strIndexOf)