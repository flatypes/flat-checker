package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.VC.*
import flat.checker.core.{Program, TypeTest}
import flat.{Config, checker}
import io.github.cvc5

import scala.collection.mutable.ListBuffer

class VCExtract(using config: Config) extends LazyLogging:
  def extract(program: Program, input: os.Path): Unit =
    val vc = VCGen.generate(program)

    given types: Types = Types.from(program.vars)

    vcCounter = 1
    process(vc, PrfCtx.empty)(using input)

  private var vcCounter = 1

  private val smtSolver = new SMTSolver

  private def process(vc: VC, ctx: PrfCtx)(using input: os.Path): Unit = vc match
    case True =>
    case Goal(c, _) =>
      val (slv, m) = smtSolver.create(c)(using ctx)
      outputQuery(slv, m)
      vcCounter += 1
    case HasType(e, t, _) =>
      val (slv, m) = smtSolver.create(TypeTest(e, t))(using ctx)
      outputQuery(slv, m)
      vcCounter += 1
    case InferType(e, _) =>
      logger.warn(s"Ignore InferType goal: $e")
    case LAnd(vc1, vc2) => process(vc1, ctx); process(vc2, ctx)
    case LImp(h, vc) => process(vc, ctx + h)

  private def outputQuery(slv: cvc5.Solver, ctx: Map[String, cvc5.Term])(using input: os.Path): Unit =
    val buf = ListBuffer.empty[String]
    buf += s"; Input: $input"
    buf += "(set-logic ALL)"
    for (x, t) <- ctx do
      buf += s"(declare-const $x ${t.getSort})"
    for assertion <- slv.getAssertions do
      buf += s"(assert $assertion)"
    buf += "(check-sat)"
    buf += "(exit)"

    val outputPath: os.Path = os.Path(config.extractOutput.getAbsolutePath) / input.baseName
    os.write.over(outputPath / s"$vcCounter.smt2", buf.mkString("\n"), createFolders = true)
