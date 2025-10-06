package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.VC.*
import flat.checker.core.Program
import flat.{Config, Issuer, checker}

/** Core Type Checker. */
class Checker(using config: Config) extends LazyLogging:
  /** Issuer: maintains the diagnostics during type checking. */
  val issuer = new Issuer

  /** Type-checks a `program` in the core language. */
  def check(program: Program): Unit =
    val vc = VCGen.generate(program)

    given types: Types = Types.from(program.vars)

    vcCounter = 1
    discharge(vc, PrfCtx.empty)(using new Prover)

  private var vcCounter = 1

  private def discharge(vc: VC, ctx: PrfCtx)(using prover: Prover): Unit = vc match
    case True =>
    case LAnd(vc1, vc2) => discharge(vc1, ctx); discharge(vc2, ctx)
    case LImp(h, vc) => discharge(vc, ctx + h)
    case _ =>
      logger.info("")
      logger.info(s"VC $vcCounter: $ctx ⇒ ${
        vc match
          case Goal(c, _) => c
          case HasType(e, t, _) => s"$e : $t"
          case InferType(e, _) => s"$e : ?"
      }")
      for mc <- config.metrics do
        mc.push("subgoals")
        mc.put("#", vcCounter)
        val kind = vc match
          case _: Goal => "normal"
          case _: HasType => "check type"
          case _: InferType => "infer type"
          case _ => assert(false)
        mc.put("kind", kind)
        mc.timeStart("time/verif")

      val succeed = vc match
        case Goal(c, err) =>
          if prover.prove(c, ctx) then true
          else
            logger.info(s"VC $vcCounter NOT PROVED")
            issuer.report(err)
            false
        case HasType(e, t, _) =>
          prover.check(e, t, ctx) match
            case Right(_) => true
            case Left(actual) =>
              logger.info(s"VC $vcCounter NOT PROVED")
              issuer.report(TypeMayMismatch(t.toString, actual, e.loc))
              false
        case InferType(e, _) =>
          val r = prover.infer(e, ctx)
          issuer.report(TypeInferred(r.toString, e.loc))
          true
        case _ => assert(false)

      for mc <- config.metrics do
        mc.timePause("time/verif")
        mc.put("succeed", succeed)
        mc.pop()

      vcCounter += 1
