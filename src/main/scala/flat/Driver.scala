package flat

import com.typesafe.scalalogging.LazyLogging
import flat.checker.*
import flat.checker.py.{Transpiler, Unpickler}

import java.io.File
import scala.collection.mutable.ListBuffer

final case class Config(inputs: Seq[String] = Seq.empty,
                        smtOnly: Boolean = false, smtTimeLimit: Int = 3000,
                        fastExit: Boolean = false, extractTo: Option[File] = None,
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
    require(config.inputs.nonEmpty, "no input files")
    for input <- config.inputs do
      val path = os.Path(java.nio.file.Paths.get(input).toAbsolutePath)
      if os.isFile(path) then
        tryCheckPython(path)
      else if os.isDir(path) then
        for file <- os.list(path).filter(_.ext == "py") do
          tryCheckPython(file)
    for recorder <- config.recorder do
      recorder.save()

  private def tryCheckPython(path: os.Path)(using config: Config): Unit =
    try checkPython(path)
    catch
      case ex: Exception =>
        for recorder <- config.recorder do
          recorder.append(ujson.Obj(
            "file" -> ujson.Str(path.toString),
            "success" -> ujson.Bool(false),
            "fatal error" -> ujson.Bool(true),
          ))
        if config.fastExit then throw ex
        else ex.printStackTrace()

  private def checkPython(path: os.Path)(using config: Config): Unit =
    logger.info("")
    logger.info(s"Checking: $path")
    val unpickler = Unpickler(path)
    val tree = unpickler.getTree
    val transpiler = new Transpiler
    val programs = transpiler.transpile(tree)
    for program <- programs do
      val vc = VCGen.generate(program)
      val types = Types.from(program.vars)
      val prover =
        if config.extractTo.isDefined then VCExtract(path)(using types = types)
        else if config.smtOnly then SMTProver(path)(using types = types)
        else Prover(path)(using types = types)
      prover.prove(vc)
      if config.fastExit then prover.issuer.ensureNoError()
      else prover.issuer.print()
      for recorder <- config.recorder do
        recorder.append(prover.getRecord)
