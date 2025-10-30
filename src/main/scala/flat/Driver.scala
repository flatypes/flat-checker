package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.ast.Module
import flat.checker.py.{Transpiler, Unpickler}
import flat.util.MetricCollector

final case class Config(inputs: Seq[os.Path] = Seq.empty,
                        noError: Boolean = false, smtTimeLimit: Int = 3000, metrics: Option[MetricCollector] = None,
                        extractMode: Boolean = false, extractOutput: Option[os.Path] = None,
                        extractMultiGoals: Boolean = false)

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputs.nonEmpty, "no inputs")
    val paths = config.inputs.flatMap(collectPy)
    if config.extractMode then
      require(config.inputs.length == 1, "extract mode only supports a single dir")
      extract(paths)
    else
      check(paths)

  private def collectPy(path: os.Path): Seq[os.Path] =
    if os.isFile(path) then
      if path.ext == "py" then
        Seq(path)
      else
        logger.warn("Ignored: {}", path)
        Seq.empty
    else
      os.walk(path).filter(_.ext == "py").sorted

  private def extract(paths: Seq[os.Path])(using config: Config): Unit =
    val inDir = config.inputs.head
    for path <- paths do
      logger.info("Extracting: {}", path)
      val module = transpile(path)
      val extractor = new Extractor
      val relPath = (path / "..").relativeTo(inDir)
      val outDir = config.extractOutput.get / relPath / path.baseName
      extractor.extract(module, outDir)

  private def transpile(path: os.Path): Module =
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    transpiler.transpile(tree)

  private def check(paths: Seq[os.Path])(using config: Config): Unit = paths match
    case Seq() => done
    case path +: rest =>
      logger.info("")
      logger.info(s"Checking: $path")
      for mc <- config.metrics do
        mc.push("files")
        mc.put("path", path.toString)
        mc.timeStart("time/transpile")
      val module = transpile(path)
      for mc <- config.metrics do
        mc.timePause("time/transpile")

      if config.extractMode then
        val extractor = new Extractor
        extractor.extract(module, path)
        check(rest)
        return

      for mc <- config.metrics do
        mc.timeStart("time/check")
      val checker = new Checker
      checker.check(module)
      if checker.issuer.noError then
        logger.info("Type CHECKED")
      else
        checker.issuer.print()
      for mc <- config.metrics do
        mc.timePause("time/check")
        mc.put("succeed", checker.issuer.noError)
        mc.pop()

      if config.noError && !checker.issuer.noError then done else check(rest)

  private inline def done(using config: Config): Unit =
    for mc <- config.metrics do
      mc.save(indent = 2, sortKeys = true)
