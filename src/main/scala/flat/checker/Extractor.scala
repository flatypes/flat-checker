package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.ast.{FunDef, Module}
import flat.{Config, checker}

class Extractor(using config: Config) extends LazyLogging:
  def extract(module: Module, input: os.Path): Unit =
    require(module.body.length == 1)
    extract(module.body.head, input)

  def extract(funDef: FunDef, input: os.Path): Unit = ???
//    val varDefs = (funDef.params :+ funDef.returns) ++ funDef.locals
//
//    given types: Types = Types.from(varDefs.map(v => v.name -> v.typ))
//
//    private val slv = new SMTSolver
//
//    val vc = VCGen.generate(types, funDef.body)
//    vcCounter = 1
//    process(vc, PrfCtx.empty)(using input)

//  private var vcCounter = 1

//  private def process(vc: Formula, ctx: PrfCtx)(using input: os.Path): Unit = vc match
//    case True =>
//    case Goal(c, _) =>
//      val task = slv.create(ctx)
//      val tc = slv.encodeExpr(c)(using task.consts)
//      outputQuery(task, tc)
//      vcCounter += 1
//    case HasType(e, t, _) =>
//      val task = slv.create(ctx)
//      val tc = slv.encodeExpr(TypeTest(e, t))(using task.consts)
//      outputQuery(task, tc)
//      vcCounter += 1
//    case InferType(e, _) =>
//      logger.warn(s"Ignore InferType goal: $e")
//    case LAnd(vc1, vc2) => process(vc1, ctx); process(vc2, ctx)
//    case LImp(h, vc) => process(vc, ctx + h)
//
//  private def outputQuery(task: slv.Task, conclusion: cvc5.Term)(using input: os.Path): Unit =
//    val cmds = ListBuffer.empty[String]
//    cmds += s"; Input: $input"
//    cmds += "(set-logic ALL)"
//    for (x, t) <- task.consts do
//      cmds += s"(declare-const $x ${t.getSort})"
//    for assertion <- task.assertions do
//      cmds += s"(assert $assertion)"
//    cmds += s"(assert ${conclusion.notTerm})"
//    cmds += "(check-sat)"
//    cmds += "(exit)"
//
//    val outputPath = config.extractOutput.get / input.baseName
//    os.write.over(outputPath / s"$vcCounter.smt2", cmds.mkString("\n"), createFolders = true)
