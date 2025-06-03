package flat

import scopt.OParser

object CLI:
  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("flat-checker"),
      head("flat-checker", "dev"),
      // option --smt-only
      opt[Unit]("smt-only")
        .action { (_, c) => c.copy(smtOnly = true) }
        .text("solving only using SMT"),
      // option --smt-time-limit
      opt[Int]("smt-time-limit")
        .action { (n, c) => c.copy(smtTimeLimit = n) }
        .valueName("<time>")
        .text("time limit per SMT query in ms (default 3000)"),
      // option --fast-exit
      opt[Unit]("fast-exit")
        .action { (_, c) => c.copy(fastExit = true) }
        .text("immediately exit upon the first error occurred"),
      help('h', "help").text("print this usage text"),
      arg[Seq[String]]("<file>...")
        .unbounded()
        .required()
        .action { (fs, c) => c.copy(inputs = fs) }
        .text("input files/folders")
    )

  def main(args: Array[String]): Unit =
    OParser.parse(parser, args, Config()) match
      case Some(config) => Driver.run(using config)
      case _ =>