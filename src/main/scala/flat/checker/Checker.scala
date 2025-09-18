package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Formula.*
import flat.checker.core.Program
import flat.{Config, Issuer}

class Checker(using config: Config) extends LazyLogging:
  def check(program: Program): Unit =
    val vc = VCGen.generate(program)

    given types: Types = Types.from(program.vars)

    discharge(vc, PrfCtx.empty)(using new Prover)

  val issuer = new Issuer

  private var vcCounter = 1

  private def discharge(vc: Formula, ctx: PrfCtx)(using prover: Prover): Unit = vc match
    case True =>
    case Goal(c, err) =>
      logger.info("")
      logger.info(s"VC $vcCounter: $ctx ⇒ $c")
      prover.prove(c, ctx) match
        case Left(_) =>
          logger.info(s"VC $vcCounter NOT PROVED")
          issuer.report(err)
        case Right(_) =>
      vcCounter += 1
    case HasType(e, t, _) =>
      logger.info("")
      logger.info(s"VC $vcCounter: $ctx ⇒ $e : $t")
      prover.check(e, t, ctx) match
        case Left(actual) =>
          logger.info(s"VC $vcCounter NOT PROVED")
          issuer.report(TypeMayMismatch(t.toString, actual, e.loc))
        case Right(_) =>
      vcCounter += 1
    case InferType(e, _) =>
      logger.info("")
      logger.info(s"VC $vcCounter: $ctx ⇒ $e : ?")
      val r = prover.infer(e, ctx)
      issuer.report(TypeInferred(r.toString, e.loc))
      vcCounter += 1
    case LAnd(vc1, vc2) => discharge(vc1, ctx); discharge(vc2, ctx)
    case LImp(h, vc) => discharge(vc, ctx + h)
