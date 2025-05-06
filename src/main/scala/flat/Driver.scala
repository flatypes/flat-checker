package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.Program
import flat.checker.py.{Transpiler, Unpickler}
import flat.checker.{Types, VCGen, VCProver}

object Driver extends LazyLogging:
  def check(programs: List[Program]): Unit =
    for program <- programs do
      val vc = VCGen.generate(program)
      logger.debug(s"VC: $vc")
      val types = Types.from(program.vars)
      val prover = new VCProver(using types = types)
      prover.prove(vc)
    logger.info("Check OK")

  def checkPython(path: os.Path): Unit =
    logger.info(s"Checking: $path")
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val programs = transpiler.transpile(tree)
    check(programs)

  def main(args: Array[String]): Unit =
    logger.debug("Logger starts")
    if args.isEmpty then
      System.err.println("No input files")
      System.exit(1)
    for input <- args do
      val path = os.Path(java.nio.file.Paths.get(input).toAbsolutePath)
      if os.isFile(path) then
        checkPython(path)
      else if os.isDir(path) then
        for file <- os.list(path).filter(_.ext == "py") do
          checkPython(file)
