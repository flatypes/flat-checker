package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Issuer
import flat.checker.core.{Expr, TypeTest}

trait VCPrf(using types: Types) extends LazyLogging:

  import Formula.*

  val issuer = new Issuer

  def prove(goal: Formula)(using ctx: PrfCtx = PrfCtx.empty): Unit = goal match
    case True =>
    case HasType(e, t, _) =>
      logger.info("")
      logger.info(s"Goal: $ctx ⇒ $e : $t")
      process(TypeTest(e, t)) match
        case Right(config) =>
          logger.info(s"Proved $config")
        case Left(actual) =>
          issuer.report(TypeMayMismatch(t.toString, actual, e.loc))
          logger.info(s"Failed")
    case Goal(e, err) =>
      logger.info("")
      logger.info(s"Goal: $ctx ⇒ $e")
      process(e) match
        case Right(config) =>
          logger.info(s"Proved $config")
        case Left(_) =>
          issuer.report(err)
          logger.info(s"Failed")
    case LAnd(goal1, goal2) =>
      prove(goal1)
      prove(goal2)
    case LImp(e, goal) =>
      prove(goal)(using ctx + e)

  protected type Config

  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Config]