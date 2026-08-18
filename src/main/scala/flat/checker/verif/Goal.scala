package flat.checker.verif

import flat.checker.flan.tpd.*
import flat.checker.verif.Simplifier.simplify
import flat.checker.verif.Type.*

final case class Goal(premises: List[Expr], conclusion: Expr)
                     (using val sorts: Map[String, Type]):
  private val solver = SMTSolver()

  private var solverSetup = false

  def have(sides: Expr*): Boolean =
    sides.forall: e =>
      e.simplify match
        case BoolLit(true) => true
        case BoolLit(false) => false
        case e =>
          if !solverSetup then
            premises.foreach(solver.add)
            solverSetup = true
          solver.prove(e)

  def collectFirstInPremises[T](pf: PartialFunction[Expr, T]): Option[T] =
    premises.collectFirst(Function.unlift(_.collectFirst(pf)))
