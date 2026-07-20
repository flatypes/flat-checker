package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.regex.{RESub, RegEx}

class Prover extends LazyLogging:
  def prove(goal: Goal): Boolean =
    goal.conclusion match
      // trivial
      case Const(true) => true
      // refinement type checking
      case StringInLang(e, r) =>
        val goal1 = narrow(goal)
        val inferer = Inferer(goal1)
        inferer.infer(e) match
          case r1: RegEx =>
            if RESub.check(r1, r) then
              true
            else
              logger.debug("{} inferred: {}", e.show, r1.show)
              false
      case _ =>
        val solver = SMTSolver(using goal.sorts)
        goal.premises.foreach(solver.add)
        if solver.prove(goal.conclusion) then
          true
        else
          val goal1 = narrow(goal)
          val lemmas = LemmaSketches.all.flatMap(_.inst(goal1))
          if lemmas.nonEmpty then
            logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
            lemmas.foreach(solver.add)
            solver.prove(goal1.conclusion)
          else
            false


  def narrow(goal: Goal): Goal =
    val narrower = Narrower(goal)
    narrower.narrow