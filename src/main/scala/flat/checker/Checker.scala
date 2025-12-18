package flat.checker

import com.typesafe.scalalogging.LazyLogging
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
    val vc = VCGenerator.generate(funDef)
    val verifier = new Verifier(using config, issuer)
    verifier.verify(vc)(using VarCtx.from(funDef))