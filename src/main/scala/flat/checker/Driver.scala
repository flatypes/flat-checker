package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.backend.Checker
import flat.checker.backend.core.Program
import flat.checker.py.{Transpiler, Unpickler}

object Driver extends LazyLogging:
  def check(programs: List[Program]): Unit =
    val checker = new Checker
    for program <- programs do checker.check(program)
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
