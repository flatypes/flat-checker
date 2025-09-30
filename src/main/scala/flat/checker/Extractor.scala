package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.VC.*
import flat.checker.core.{Program, TypeTest}
import flat.{Config, checker}
import io.github.cvc5

import scala.collection.mutable.ListBuffer

class Extractor(using config: Config) extends LazyLogging:
  def extract(program: Program, input: os.Path): Unit =
    val vc = VCGen.generate(program)

    given types: Types = Types.from(program.vars)

    vcCounter = 1
    process(vc, PrfCtx.empty)(using input)

  private var vcCounter = 1

  private val slv = new SMTSolver

  private def process(vc: VC, ctx: PrfCtx)(using input: os.Path): Unit = vc match
    case True =>
    case Goal(c, _) =>
      val task = slv.create(c)(using ctx)
      outputQuery(task)
      vcCounter += 1
    case HasType(e, t, _) =>
      val task = slv.create(TypeTest(e, t))(using ctx)
      outputQuery(task)
      vcCounter += 1
    case InferType(e, _) =>
      logger.warn(s"Ignore InferType goal: $e")
    case LAnd(vc1, vc2) => process(vc1, ctx); process(vc2, ctx)
    case LImp(h, vc) => process(vc, ctx + h)

  private def outputQuery(task: slv.Task)(using input: os.Path): Unit =
    val cmds = ListBuffer.empty[String]
    cmds += s"; Input: $input"
    cmds += "(set-logic ALL)"
    for (x, t) <- task.consts do
      cmds += s"(declare-const $x ${t.getSort})"
    for assertion <- task.assertions do
      cmds += s"(assert $assertion)"
    cmds += "(check-sat)"
    cmds += "(exit)"

    val outputPath = config.extractOutput.get / input.baseName
    os.write.over(outputPath / s"$vcCounter.smt2", cmds.mkString("\n"), createFolders = true)
