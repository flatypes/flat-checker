package flat.checker

import flat.checker.ast.FunDef
import flat.checker.py.{Transpiler, Unpickler}

object Driver:
  def check(script: Seq[FunDef]): Unit =
    println(s"Input:\n$script")
    val typer = new Typer
    typer.process(script)
    println("Type check OK")

  def checkPython(path: os.Path): Unit =
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val ir = transpiler.transpile(tree)
    check(ir)

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("No input files")
      System.exit(1)
    for path <- args do
      checkPython(os.Path(java.nio.file.Paths.get(path).toAbsolutePath))