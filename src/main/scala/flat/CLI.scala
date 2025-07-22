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
      // option --smt-extract
      opt[File]("extract-only")
        .action { (f, c) => c.copy(extractTo = Some(f)) }
        .valueName("<folder>")
        .text("do not solve but only extract proof obligations as SMT queries"),
      // option --stat
      opt[File]("stat")
        .action { (f, c) => c.copy(recorder = Some(Recorder(f))) }
        .valueName("<file>")
        .text("save statistics to a JSON file"),
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