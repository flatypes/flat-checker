package flat.checker

import flat.checker.Formula.*
import flat.checker.core.{Expr, TypeTest}
import flat.{Config, checker}
import io.github.cvc5

import scala.collection.mutable.ListBuffer

class VCExtract(input: os.Path)(using types: Types, config: Config) extends VCPrf:
  private val smtSolver = SMTSolver(using config)

  override def proveSubGoal(goal: Goal)(using ctx: PrfCtx): Unit =
    val Goal(e, _) = goal
    val (slv, ctx) = smtSolver.create(e)
    outputQuery(slv, ctx)

  override def proveHasType(goal: HasType)(using ctx: PrfCtx): Unit =
    val HasType(e, t, _) = goal
    val (slv, ctx) = smtSolver.create(TypeTest(e, t))
    outputQuery(slv, ctx)

  private val outputPath: os.Path = os.Path(config.extractTo.get.getAbsolutePath) / input.baseName
  private var nextQuery = 0

  private def outputQuery(slv: cvc5.Solver, ctx: Map[String, cvc5.Term]): Unit =
    val buf = ListBuffer.empty[String]
    buf += s"; Input: $input"
    buf += "(set-logic ALL)"
    for (x, t) <- ctx do
      buf += s"(declare-const $x ${t.getSort})"
    for assertion <- slv.getAssertions do
      buf += s"(assert $assertion)"
    buf += "(check-sat)"
    buf += "(exit)"

    nextQuery += 1
    os.write.over(outputPath / s"$nextQuery.smt2", buf.mkString("\n"), createFolders = true)

  type Config = Nothing

  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Config] =
    throw IllegalStateException()
