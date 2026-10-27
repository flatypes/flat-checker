package flat.checker.verif

import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

object Synthesizer:
  def synthesizeLemmas(goal: Goal): List[Expr] =
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.foreach: expr =>
      Inferer.infer(expr)(using goal) match
        case Domain.Top => false
        case dom =>
          lemmas ++= dom.membership(expr)
          true

    lemmas.toList
