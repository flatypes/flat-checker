package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.parsing.Parser
import flat.checker.typing.Checker
import flat.checker.verif.Verifier
import flat.checker.{Reporter, Source}
import flat.util.{Aggregator, MetricCollector}
import scopt.OParser

import java.nio.file.Path

object CLI extends LazyLogging:
  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("flat-checker"),
      head("flat-checker", "dev"),
      // arg: input files
      arg[Path]("<file>...")
        .unbounded()
        .required()
        .action { (p, c) => c.copy(inputs = c.inputs :+ os.Path(p.toAbsolutePath)) }
        .text("input files/directories"),
      // option: --no-error
      opt[Unit]("no-error")
        .action { (_, c) => c.copy(noError = true) }
        .text("ensure no type errors (if not, exit on first error)"),
      // option: --metrics
      opt[Path]("metrics")
        .action: (p, c) =>
          c.copy(metrics = Some(MetricCollector(os.Path(p.toAbsolutePath), Aggregator.AllCount, Aggregator.AllTime)))
        .valueName("<file>")
        .text("collect and save statistical metrics to a JSON file"),
      // option: --smt-time-limit
      opt[Int]("smt-time-limit")
        .action { (n, c) => c.copy(smtTimeLimit = n) }
        .valueName("<time>")
        .text("time limit per SMT query in ms (default 3000)"),
      // option: -h, --help
      help('h', "help")
        .text("print this usage text"),
      // available subcommands
      note("\nAvailable subcommand: extract")
    )

  private val extractBuilder = OParser.builder[ExtractConfig]

  private val extractParser =
    import extractBuilder.*
    OParser.sequence(
      programName("flat-checker extract"),
      // arg: input directory
      arg[Path]("<dir>")
        .required()
        .action { (p, c) => c.copy(input = os.Path(p.toAbsolutePath)) }
        .text("input directory"),
      // option: -o <dir> (required)
      opt[Path]('o', "output")
        .required()
        .action { (p, c) => c.copy(output = os.Path(p.toAbsolutePath)) }
        .valueName("<dir>")
        .text("extract to this directory"),
      // option: --multi-goals
      opt[Unit]("multi-goals")
        .action { (_, c) => c.copy(multiGoals = true) }
        .text("split VCs into multiple goals"),
      // option: -h, --help
      help('h', "help")
        .text("print this usage text")
    )

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      Console.err.println(OParser.usage(parser))
      System.exit(1)

    // normal command
    // wip: flan file
    runFlan(args.toSeq.map(Path.of(_).toAbsolutePath).map(os.Path(_)))

  private def runFlan(inputs: Seq[os.Path]): Unit =
    val paths = inputs.flatMap(collectFlan)
    if paths.isEmpty then
      Console.err.println("No input files!")
      System.exit(1)

    for path <- paths.sorted do
      logger.info("Checking: {}", path)
      val source = Source.fromPath(path)
      val reporter = Reporter(source)
      val parser = Parser(using reporter)
      val tree = parser.parse(source)
      if reporter.hasError then
        reporter.printTo(Console.err)
        System.exit(1)

      val typeChecker = Checker(using reporter)
      val program = typeChecker.checkProgram(tree)
      if reporter.hasError then
        reporter.printTo(Console.err)
        System.exit(1)

      val verifier = Verifier(using reporter)
      verifier.verify(???)
      if reporter.hasError then
        reporter.printTo(Console.err)
        System.exit(1)

      reporter.printTo(Console.err) // print warnings if any
      Console.println("Typing success: " + path.toString)

  private def collectFlan(path: os.Path): Seq[os.Path] =
    if os.isDir(path) then os.walk(path).filter(_.ext == "flan")
    else if os.isFile(path) && path.ext == "flan" then Seq(path)
    else Seq.empty