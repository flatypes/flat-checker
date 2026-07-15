package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

class Prover extends LazyLogging:
  def prove(goal: Goal): Boolean =
    goal.conclusion match
      case Const(true) => true
      case Const(false) => false
      case _ =>
        val solver = SMTSolver(using goal.sorts)
        goal.premises.foreach(solver.add)
        if solver.prove(goal.conclusion) then
          true
        else
          val lemmas = LemmaSketches.all.flatMap(_.inst(goal))
          if lemmas.nonEmpty then
            logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
            lemmas.foreach(solver.add)
            solver.prove(goal.conclusion)
          else
            false
