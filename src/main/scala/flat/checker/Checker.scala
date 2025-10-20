package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Printer.{ppCtx, ppExpr, ppVCGoal}
import flat.checker.ast.{FunDef, Module}
import flat.{Config, Issuer, checker}

/** Core Type Checker. */
class Checker(using config: Config) extends LazyLogging:
  /** Issuer: maintains the diagnostics during type checking. */
  val issuer = new Issuer

  /** Type-checks a `module` in the core language. */
  def check(module: Module): Unit =
    for f <- module.body do check(f)

  def check(funDef: FunDef): Unit =
    val varDefs = (funDef.params :+ funDef.returns) ++ funDef.locals
    val types = Types.from(varDefs.map(v => v.name -> v.typ))
    val vc = VCGenerator.generate(funDef.body, types)
    nextGoal = 1
    discharge(vc, PrfCtx.empty(using types))(using new Prover(using types = types))

  private var nextGoal = 1

  private def discharge(vc: VC, ctx: PrfCtx)(using prover: Prover): Unit = vc match
    case VCTrue =>
    case VCInfer(e) =>
      logger.info("")
      logger.info("Goal {}: {} ⇒ {} : ?", nextGoal, ppCtx(ctx), ppExpr(e))
      val r = prover.infer(e, ctx)
      issuer.report(TypeInferred(r.toString, e.loc))
    case VCImp(eb, vc) => discharge(vc, ctx + eb)
    case VCAnd(vc1, vc2) => discharge(vc1, ctx); discharge(vc2, ctx)
    case g: VCGoal =>
      logger.info("")
      logger.info(s"Goal {}: {} ⇒ {}", nextGoal, ppCtx(ctx), ppVCGoal(g))
      for mc <- config.metrics do
        mc.push("vcs")
        mc.put("#", nextGoal)
        mc.put("kind", g.getClass.toString)
        mc.timeStart("time/verif")
      val succeed = prover.prove(g.cond, ctx)
      for mc <- config.metrics do
        mc.timePause("time/verif")
        mc.put("succeed", succeed)
        mc.pop()

      if !succeed then
        logger.info(s"Goal {} NOT PROVED", nextGoal)
        issuer.report(g.diagnostic(""))

      nextGoal += 1
