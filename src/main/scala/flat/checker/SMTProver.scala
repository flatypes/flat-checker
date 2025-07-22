package flat.checker

import flat.Config
import flat.checker.core.Expr

class SMTProver(path: os.Path)(using types: Types, config: Config) extends VCPrf(path):

  import flat.checker.SolverResult.*

  private val smtSolver = SMTSolver(using config)

  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting] =
    smtSolver.prove(conclusion) match
      case Valid => Right(Setting(withSMT = true))
      case Invalid(_) => Left("")
