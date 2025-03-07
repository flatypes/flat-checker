package flat.checker

import flat.checker.ast.FunDef
import flat.checker.frontend.python

object Driver:
  def check(script: Seq[FunDef]): Unit =
    println(s"Input:\n$script")
    val issuer = new Issuer
    val typer = new Typer
    typer.process(script)
    typer.issuer.print()
    if typer.issuer.noError then println("Type check OK")

  def checkPython(astJSONPath: os.Path): Unit =
    val json = os.read(astJSONPath)
    val tree = python.Parser(json)
    val issuer = new Issuer
    val checker = python.GlobalChecker(issuer)
    val program = checker.check(tree)
    issuer.print()
    if issuer.noError then check(program)

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("No input files")
      System.exit(1)
    for path <- args do
      checkPython(os.Path(java.nio.file.Paths.get(path).toAbsolutePath))