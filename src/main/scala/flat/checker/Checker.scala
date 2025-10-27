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
    run(vc)(using new Verifier(using types = types))

  //    nextGoal = 1
//    discharge(vc, PrfCtx.empty(using types))(using new Prover(using types = types))
  private var nextGoal = 1

  def run(vc: VC)(using prover: Verifier): Boolean = vc match
    case VCTrue => true
    case VCImp(b, vc) => prover.assume(b); run(vc)
    case VCGroup(sides, mains) =>
      // Prove all side conditions first: if any fails, immediately give up this group.
      val result1 = sides.forall(vc => prover.locally { run(vc) })
      if !result1 then
        return false
      // Prove all main goals: even if any fails, still try others to collect more error messages.
      mains.map(vc => prover.locally { run(vc) }).forall(_ == true)
    case VCInfer(e) =>
      logger.info("")
      logger.info("Goal {}: {} ⇒ {} : ?", nextGoal, ppCtx(prover.getCtx), ppExpr(e))
      val r = prover.infer(e)
      issuer.report(TypeInferred(r.toString, e.loc))
      nextGoal += 1
      true
    case g: VCGoal =>
      logger.info("")
      logger.info(s"Goal {}: {} ⇒ {}", nextGoal, ppCtx(prover.getCtx), ppVCGoal(g))
      for mc <- config.metrics do
        mc.push("vcs")
        mc.put("#", nextGoal)
        mc.put("kind", g.getClass.toString)
        mc.timeStart("time/verif")
      val succeed = prover.prove(g.cond)
      for mc <- config.metrics do
        mc.timePause("time/verif")
        mc.put("succeed", succeed)
        mc.pop()
      if !succeed then
        logger.info(s"Goal {} NOT PROVED", nextGoal)
        issuer.report(g.diagnostic(""))
      nextGoal += 1
      succeed

  private def discharge(vc: VC, ctx: PrfCtx)(using prover: Verifier): Boolean = ???
/* vc match
  case VCTrue => true
  case VCImp(eb, vc) => discharge(vc, ctx + eb)
  case VCGroup(sides, mains) =>
    if sides.exists(!discharge(_, ctx)) then
      return false
    val results = mains.map(discharge(_, ctx))
    results.forall(_ == true)
  case VCInfer(e) =>
    logger.info("")
    logger.info("Goal {}: {} ⇒ {} : ?", nextGoal, ppCtx(ctx), ppExpr(e))
    val r = prover.infer(e, ctx)
    issuer.report(TypeInferred(r.toString, e.loc))
    true
  case g: VCGoal =>
    logger.info("")
    logger.info(s"Goal {}: {} ⇒ {}", nextGoal, ppCtx(ctx), ppVCGoal(g))
    for mc <- config.metrics do
      mc.push("vcs")
      mc.put("#", nextGoal)
      mc.put("kind", g.getClass.toString)
      mc.timeStart("time/verif")
    val succeed = prover.prove(g.cond)
    for mc <- config.metrics do
      mc.timePause("time/verif")
      mc.put("succeed", succeed)
      mc.pop()

    if !succeed then
      logger.info(s"Goal {} NOT PROVED", nextGoal)
      issuer.report(g.diagnostic(""))

    nextGoal += 1
    succeed
*/