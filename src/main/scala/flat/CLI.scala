package flat

import scopt.OParser

import java.io.File

object CLI:
  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("flat-checker"),
      head("flat-checker", "dev"),
      // arg: input files
      arg[String]("<file>...")
        .unbounded()
        .required()
        .action { (f, c) => c.copy(inputFiles = c.inputFiles :+ f) }
        .text("input files/dirs"),
      // option --fast-exit
      opt[Unit]("fast-exit")
        .action { (_, c) => c.copy(fastExit = true) }
        .text("immediately exit upon the first error occurred"),
      // option --stat
      opt[File]("stat")
        .action { (f, c) => c.copy(recorder = Some(Recorder(f))) }
        .valueName("<file>")
        .text("save statistics to a JSON file"),
      // option --smt-time-limit
      opt[Int]("smt-time-limit")
        .action { (n, c) => c.copy(smtTimeLimit = n) }
        .valueName("<time>")
        .text("time limit per SMT query in ms (default 3000)"),
      // option -h
      help('h', "help")
        .text("print this usage text"),
      // subcommand: extract
      cmd("extract")
        .text("Extract VCs only, without doing type checking")
        .action((_, c) => c.copy(extractMode = true))
        .children(
          opt[File]('o', "output")
            .required()
            .action { (f, c) => c.copy(extractOutput = f) }
            .valueName("<dir>")
            .text("extract to this directory"),
        )
    )

  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, Config()) match
      case Some(config) => Driver.run(using config)
      case _ =>