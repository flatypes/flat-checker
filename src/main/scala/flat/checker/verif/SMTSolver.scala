package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Sort
import flat.checker.flan.tpd.*
import io.github.cvc5

class SMTSolver(using vars: Map[String, Type]) extends LazyLogging:
  private val encoder = SMTEncoder()
  private val slv = cvc5.Solver(encoder.tm)
  slv.setOption("tlimit-per", "3000")

  def push(): Unit = slv.push()

  def pop(): Unit = slv.pop()

  def add(cond: Expr): Unit =
    val term = encoder.encodeExpr(cond)(using Map.empty)
    slv.assertFormula(term)

  def prove(cond: Expr): Boolean =
    slv.push()
    val term = encoder.encodeExpr(cond)(using Map.empty)
    slv.assertFormula(term.notTerm)
    val result = slv.checkSat()
    slv.pop()
    result.isUnsat
