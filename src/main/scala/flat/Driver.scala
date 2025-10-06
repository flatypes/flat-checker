package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.core.Program
import flat.checker.py.{Transpiler, Unpickler}
import flat.util.MetricCollector

final case class Config(inputs: Seq[os.Path] = Seq.empty,
                        fastExit: Boolean = false, smtTimeLimit: Int = 3000, metrics: Option[MetricCollector] = None,
                        extractMode: Boolean = false, extractOutput: os.Path = null, extractSubgoals: Boolean = false)

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputs.nonEmpty, "no inputs")
    val paths: Seq[os.Path] = config.inputs.flatMap: path =>
      if os.isFile(path) then Seq(path)
      else if os.isDir(path) then os.list(path).filter(_.ext == "py")
      else Seq.empty
    checkFiles(paths)

  private def checkFiles(paths: Seq[os.Path])(using config: Config): Unit = paths match
    case Seq() => done
    case path +: rest =>
      logger.info("")
      logger.info(s"Checking: $path")
      for mc <- config.metrics do
        mc.push("files")
        mc.put("path", path.toString)
        mc.timeStart("time/transpile")
      val programs = transpile(path)
      for mc <- config.metrics do
        mc.timePause("time/transpile")

      if config.extractMode then
        for mc <- config.metrics do
          mc.timeStart("time/extract")
        val extractor = new Extractor
        programs.foreach(extractor.extract(_, path))
        for mc <- config.metrics do
          mc.timePause("time/extract")
          mc.pop()
        checkFiles(rest)
        return

      for mc <- config.metrics do
        mc.timeStart("time/check")
      val checker = new Checker
      programs.foreach(checker.check)
      if checker.issuer.noError then
        println(s"$path: Type CHECKED")
      else
        println(s"$path: Type ERROR")
        checker.issuer.print()
      for mc <- config.metrics do
        mc.timePause("time/check")
        mc.put("succeed", checker.issuer.noError)
        mc.pop()

      if config.fastExit && !checker.issuer.noError then done else checkFiles(rest)

  private inline def transpile(path: os.Path): List[Program] =
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    transpiler.transpile(tree)

  private inline def done(using config: Config): Unit =
    for mc <- config.metrics do
      mc.save(indent = 2, sortKeys = true)