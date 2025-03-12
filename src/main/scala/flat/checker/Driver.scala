package flat.checker

import flat.checker.ast.FunDef
import flat.checker.py.{Transpiler, Unpickler}

object Driver:
  def check(script: Seq[FunDef]): Unit =
    val typer = new Typer
    typer.process(script)
    println("Check OK")

  def checkPython(path: os.Path): Unit =
    println(s"Checking: $path")
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val ir = transpiler.transpile(tree)
    check(ir)

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("No input files")
      System.exit(1)
    for input <- args do
      val path = os.Path(java.nio.file.Paths.get(input).toAbsolutePath)
      if os.isFile(path) then
        checkPython(path)
      else if os.isDir(path) then
        for file <- os.list(path) do checkPython(file)
