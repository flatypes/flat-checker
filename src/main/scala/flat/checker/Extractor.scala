package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.VC.*
import flat.checker.core.{Expr, Program, TypeTest}
import flat.{Config, checker}
import io.github.cvc5

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Extractor(using config: Config) extends LazyLogging:
  def extract(program: Program, input: os.Path): Unit =
    require(config.extractMode)
    val outputDir = config.extractOutput / input.baseName
    val vc = VCGen.generate(program)

    given types: Types = Types.from(program.vars)

    val vars = mutable.Map.empty[String, cvc5.Term]
    collectVars(vc)(using vars = vars)
    val assertions = vars.flatMap { case x -> t => slv.encodeHasType(t, types(x)) }
    val t = encode(vc)(using vars.toMap)
    save(vars.toMap, assertions.toList, t, input, outputDir / "0.smt2")

    if config.extractSubgoals then
      val extractor = SubgoalExtractor(using input, outputDir)
      extractor.process(vc, PrfCtx.empty)

  private val slv = new SMTSolver

  private def collectVars(vc: VC)(using vars: mutable.Map[String, cvc5.Term], types: Types): Unit = vc match
    case True =>
    case HasType(e, _, _) => collectVars(e)
    case InferType(_, _) => throw IllegalArgumentException()
    case Goal(b, _) => collectVars(b)
    case LAnd(vc1, vc2) => collectVars(vc1); collectVars(vc2)
    case LImp(b, vc) => collectVars(b); collectVars(vc)

  private def collectVars(expr: Expr)(using vars: mutable.Map[String, cvc5.Term], types: Types): Unit =
    for x <- expr.collectVars do
      if !vars.contains(x) then
        vars(x) = slv.mkConst(x, types(x))

  private def encode(vc: VC)(using vars: Map[String, cvc5.Term]): cvc5.Term = vc match
    case True => slv.encodeExpr(true)
    case Goal(c, _) => slv.encodeExpr(c)
    case HasType(e, t, _) => slv.encodeExpr(TypeTest(e, t))
    case InferType(_, _) => throw IllegalArgumentException()
    case LAnd(vc1, vc2) => encode(vc1).andTerm(encode(vc2))
    case LImp(p, vc) => slv.encodeExpr(p).impTerm(encode(vc))

  private def save(vars: Map[String, cvc5.Term], assertions: List[cvc5.Term], conclusion: cvc5.Term,
                   input: os.Path, output: os.Path): Unit =
    val cmds = ListBuffer.empty[String]
    cmds += s"; Input: $input"
    cmds += "(set-logic ALL)"
    for (x, t) <- vars do
      cmds += s"(declare-const $x ${t.getSort})"
    for assertion <- assertions do
      cmds += s"(assert $assertion)"
    cmds += s"(assert ${conclusion.notTerm})"
    cmds += "(check-sat)"
    cmds += "(exit)"
    os.write.over(output, cmds.mkString("\n"), createFolders = true)

  private class SubgoalExtractor(using input: os.Path, outputDir: os.Path):
    private var vcCounter = 1

    def process(vc: VC, ctx: PrfCtx): Unit = vc match
      case True =>
      case Goal(c, _) =>
        val task = slv.create(ctx)
        val tc = slv.encodeExpr(c)(using task.consts)
        save(task.consts, task.assertions, tc, input, outputDir / s"$vcCounter.smt2")
        vcCounter += 1
      case HasType(e, t, _) =>
        val task = slv.create(ctx)
        val tc = slv.encodeExpr(TypeTest(e, t))(using task.consts)
        save(task.consts, task.assertions, tc, input, outputDir / s"$vcCounter.smt2")
        vcCounter += 1
      case InferType(e, _) =>
        logger.warn(s"Ignore InferType goal: $e")
      case LAnd(vc1, vc2) => process(vc1, ctx); process(vc2, ctx)
      case LImp(h, vc) => process(vc, ctx + h)
