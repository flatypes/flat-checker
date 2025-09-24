package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.py.{Transpiler, Unpickler}

import java.io.File
import scala.collection.mutable.ListBuffer

final case class Config(inputFiles: Seq[String] = Seq.empty, fastExit: Boolean = false, smtTimeLimit: Int = 3000,
                        extractMode: Boolean = false, extractOutput: File = File(""),
                        recorder: Option[Recorder] = None)

final class Recorder(output: File):
  private val path = os.Path(output.getAbsolutePath)
  require(path.ext == "json", "expect a JSON file")

  private val records = ListBuffer.empty[ujson.Obj]

  def append(record: ujson.Obj): Unit = records += record

  def save(): Unit =
    os.write.over(path, ujson.write(ujson.Arr.from(records), indent = 2))

object Driver extends LazyLogging:
  def run(using config: Config): Unit =
    require(config.inputFiles.nonEmpty, "no input files")
    for input <- config.inputFiles do
      val path = os.Path(java.nio.file.Paths.get(input).toAbsolutePath)
      if os.isFile(path) then
        checkFile(path)
      else if os.isDir(path) then
        for file <- os.list(path).filter(_.ext == "py") do
          checkFile(file)
    for recorder <- config.recorder do
      recorder.save()

  private def checkFile(path: os.Path)(using config: Config): Unit =
    logger.info("")
    logger.info(s"Checking: $path")
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val programs = transpiler.transpile(tree)
    for program <- programs do
      if config.extractMode then
        val extractor = new Extractor
        extractor.extract(program, path)
      else
        val checker = new Checker
        checker.check(program)
        if config.fastExit then
          checker.issuer.ensureNoError()
        else
          checker.issuer.print()
