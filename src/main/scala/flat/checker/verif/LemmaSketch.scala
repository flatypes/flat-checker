package flat.checker.verif

import flat.checker.flan.Sort
import flat.checker.flan.tpd.*
import flat.checker.verif.Simplifier.simplify

final class Goal(val premises: List[Expr], val conclusion: Expr)
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

  val all: List[LemmaSketch] = List(stringSliceCount)