package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.ast.Module
import flat.checker.parsing.FlanParsers
import flat.checker.py.{Transpiler, Unpickler}
import flat.util.MetricCollector

final case class Config(inputs: Seq[os.Path] = Seq.empty, noError: Boolean = false, smtTimeLimit: Int = 3000,
                        metrics: Option[MetricCollector] = None, extractMode: Boolean = false)

final case class ExtractConfig(input: os.Path = null, output: os.Path = null, multiGoals: Boolean = false)

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputs.nonEmpty, "no inputs")
    for input <- config.inputs do
      for path <- collectFlan(input) do
        logger.info("")
        logger.info("Checking: {}", path)
        val reporter = new Reporter(Source.fromPath(path))
        val parser = new FlanParsers(path.toString, os.read(path))(using reporter)
        val mod = parser.parse()
        if reporter.hasErrors then
          reporter.printTo(System.err)
        else
          logger.info("Parsed: {}", path)

    done

  private def collectFlan(path: os.Path): Seq[os.Path] =
    if os.isFile(path) then
      if path.ext == "flan" then
        Seq(path)
      else
        logger.warn("Ignored: {}", path)
        Seq.empty
    else
      os.walk(path).filter(_.ext == "flan").sorted

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
