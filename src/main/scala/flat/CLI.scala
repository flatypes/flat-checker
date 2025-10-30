package flat

import flat.util.{Aggregator, MetricCollector}
import scopt.OParser

import java.nio.file.Path

object CLI:
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
        .text("input files/dirs"),
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
      // subcommand: extract
      cmd("extract")
        .text("Extract VCs only, without doing type checking")
        .action((_, c) => c.copy(extractMode = true))
        .children(
          // option: -o <dir> (required)
          opt[Path]('o', "output")
            .required()
            .action { (p, c) => c.copy(extractOutput = Some(os.Path(p.toAbsolutePath))) }
            .valueName("<dir>")
            .text("extract to this directory"),
          // option: --multi-goals
          opt[Unit]("multi-goals")
            .action { (_, c) => c.copy(extractMultiGoals = true) }
            .text("split VCs into multiple goals")
        )
    )

  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, Config()) match
      case Some(config) => Driver.run(using config)
      case _ =>