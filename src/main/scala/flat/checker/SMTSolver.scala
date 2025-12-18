package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.ast.*
import io.github.cvc5

final class SMTSolver(using config: Config, varCtx: VarCtx) extends LazyLogging:
  private val encoder = new SMTEncoder
  private val slv = cvc5.Solver(encoder.tm)

  def push(): Unit = slv.push()

  def pop(): Unit = slv.pop()

  def assume(cond: Expr): Unit =
    val t = encoder.encodeExpr(cond)
    slv.assertFormula(t)

  def proves(conclusion: Expr): Boolean =
    slv.push()
    val t = encoder.encodeExpr(conclusion)
    slv.assertFormula(t.notTerm)
    val result = slv.checkSat()
    slv.pop()
    result.isUnsat

  def solve(conclusion: Expr): Either[String, Unit] =
    slv.push()
    val t = encoder.encodeExpr(conclusion)
    slv.assertFormula(t.notTerm)
    val slvResult = slv.checkSat()
    val result =
      if slvResult.isUnsat then
        Right(())
      else if slvResult.isSat then
        val model = for (x, t) <- encoder.getCtx yield x + " = " + slv.getValue(t).toString
        Left(model.mkString("\n"))
      else
        Left(slvResult.getUnknownExplanation.toString)
    slv.pop()
    result
