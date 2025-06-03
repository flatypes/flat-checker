package flat.checker

import flat.Config
import flat.checker.core.Expr

class SMTProver(using types: Types, config: Config) extends VCPrf:

  import flat.checker.SolverResult.*

  private val smtSolver = SMTSolver(using config)

  object Dummy:
    override def toString: String = ""

  protected type Config = Dummy.type

  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Config] =
    smtSolver.prove(conclusion) match
      case Valid => Right(Dummy)
      case Invalid(_) => Left("")
