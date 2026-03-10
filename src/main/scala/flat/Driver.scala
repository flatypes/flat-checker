package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.ast.Module
import flat.checker.py.{Transpiler, Unpickler}
import flat.util.MetricCollector

final case class Config(inputs: Seq[os.Path] = Seq.empty, noError: Boolean = false, smtTimeLimit: Int = 3000,
                        metrics: Option[MetricCollector] = None, extractMode: Boolean = false)

final case class ExtractConfig(input: os.Path = null, output: os.Path = null, multiGoals: Boolean = false)

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputs.nonEmpty, "no inputs")
    for input <- config.inputs do
      for path <- collectPy(input) do
        logger.info("")
        logger.info("Checking: {}", path)
        for mc <- config.metrics do
          mc.push("files")
          mc.put("path", path.toString)
          mc.timeStart("time/transpile")
        val module = transpile(path)
        for mc <- config.metrics do
          mc.timePause("time/transpile")
        for mc <- config.metrics do
          mc.timeStart("time/check")
        val checker = new verifier.Verifier
        val report = checker.verify(module)
        for mc <- config.metrics do
          mc.timePause("time/check")
          mc.put("succeed", report.noError)
          mc.pop()
        if report.noError then
          logger.info("Type CHECKED")
        else if config.noError then
          done
          System.exit(1)
    done

  private def collectPy(path: os.Path): Seq[os.Path] =
    if os.isFile(path) then
      if path.ext == "py" then
        Seq(path)
      else
        logger.warn("Ignored: {}", path)
        Seq.empty
    else
      os.walk(path).filter(_.ext == "py").sorted

  private def transpile(path: os.Path): Module =
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    transpiler.transpile(tree)

  private inline def done(using config: Config): Unit =
    for mc <- config.metrics do
      mc.save(indent = 2, sortKeys = true)

  def runExtract(using config: ExtractConfig): Unit =
    require(os.isDir(config.input))
    for path <- collectPy(config.input) do
      logger.info("Extracting: {}", path)
      val module = transpile(path)
      val extractor = new Extractor
      extractor.extract(module, path)
