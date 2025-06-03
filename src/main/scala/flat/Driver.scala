package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.py.{Transpiler, Unpickler}
import flat.checker.{Prover, SMTProver, Types, VCGen}

final case class Config(inputs: Seq[String] = Seq.empty,
                        smtOnly: Boolean = false, smtTimeLimit: Int = 3000,
                        fastExit: Boolean = false)

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputs.nonEmpty, "no input files")
    for input <- config.inputs do
      val path = os.Path(java.nio.file.Paths.get(input).toAbsolutePath)
      if os.isFile(path) then
        checkPython(path)
      else if os.isDir(path) then
        for file <- os.list(path).filter(_.ext == "py") do
          checkPython(file)

  private def checkPython(path: os.Path)(using config: Config): Unit =
    logger.info("")
    logger.info(s"Checking: $path")
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val programs = transpiler.transpile(tree)
    for program <- programs do
      val vc = VCGen.generate(program)
      val types = Types.from(program.vars)
      val prover = if config.smtOnly then SMTProver(using types = types) else Prover(using types = types)
      prover.prove(vc)
      if config.fastExit then prover.issuer.ensureNoError()
      else prover.issuer.print()
