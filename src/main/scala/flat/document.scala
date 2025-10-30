package flat

import scala.collection.mutable.ListBuffer

class Document private(val name: String, content: String):
  private val lines = content.split('\n')

  def getLine(row: Int): String = lines(row)

object Document:
  def fromText(name: String, content: String): Document = Document(name, content)

  def fromPath(path: os.Path): Document = Document(path.toString, os.read(path))

final case class Position(row: Int, offset: Int)

final case class Location(doc: Document, start: Position, end: Position)

trait Locational:
  protected var optLoc: Option[Location] = None

  def loc: Location =
    require(optLoc.isDefined, s"missing location for node $this")
    optLoc.get

  def setLocation(newLoc: Location): this.type =
    this.optLoc = Some(newLoc)
    this

  def copyLocation(from: Locational): this.type =
    this.optLoc = from.optLoc
    this

class Diagnostic(val loc: Location, val message: String,
                 val detail: String = "", val severity: DiagnosticSeverity = DiagnosticSeverity.ERROR):
  def longString: String =
    val buf = ListBuffer.empty[String]
    val lineNumberWidth = (loc.end.row + 1).toString.length
    buf += "-".repeat(lineNumberWidth) + s" [$severity] " + message + ": " + loc.doc.name
    for row <- loc.start.row to loc.end.row do
      val lineNumber = (row + 1).toString
      val code = loc.doc.getLine(row)
      buf += lineNumber.padTo(lineNumberWidth, ' ') + " |" + code
      val caretOffset = if row == loc.start.row then loc.start.offset else 0
      val indentation = " ".repeat(lineNumberWidth) + " |" + " ".repeat(caretOffset)
      buf += indentation + "^".repeat(if row == loc.end.row then loc.end.offset - caretOffset else code.length)
      if detail.nonEmpty then
        for detailLine <- detail.split('\n') do
          buf += indentation + detailLine
    buf.map(_ + '\n').mkString

enum DiagnosticSeverity:
  case ERROR
  case WARN
  case INFO
