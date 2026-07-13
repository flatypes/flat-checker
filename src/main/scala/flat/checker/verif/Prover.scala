package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.Simplifier.simplify

class Prover extends LazyLogging:
  def prove(assertion: Expr, ctx: PrfCtx): Boolean =
    val conclusion = assertion.simplify
    val result = conclusion match
      case Const(true) => true
      case Const(false) => false
      case _ =>
        val solver = SMTSolver(using ctx.vars)
        ctx.premises.foreach(solver.assume)
        solver.prove(conclusion)
    if !result then
      logger.debug("Failed Goal:\n{}{}{}", ctx.premises.map(_.show).map(_ + "\n").mkString, "-" * 20 + "\n",
        conclusion.show)
    result