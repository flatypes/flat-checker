package flat.checker

import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

class Document private(val name: String, val content: String):
  def getLine(row: Int): String = content.split('\n')(row)

object Document:
  def fromText(name: String, content: String): Document = Document(name, content)

  def fromPath(path: os.Path): Document = Document(path.toString, os.read(path))

final case class Position(row: Int, offset: Int)

final case class Location(doc: Document, start: Position, end: Position)

trait Locational:
  var loc: Location = uninitialized

  def setLocation(newLoc: Location): this.type =
    this.loc = newLoc
    this

  def copyLocation(from: Locational): this.type =
    this.loc = from.loc
    this

class Diagnostic(val loc: Location, val severity: DiagnosticSeverity, val message: String,
                 val explanations: Seq[String] = Seq()):
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
      for explanation <- explanations do buf += indentation + explanation
    buf.map(_ + '\n').mkString

enum DiagnosticSeverity:
  case ERROR
  case WARN
  case INFO
  case HINT

import flat.checker.DiagnosticSeverity.ERROR

class SyntaxError(loc: Location, details: Seq[String]) extends Diagnostic(loc, ERROR, "Syntax error", details)

class NameError(loc: Location, details: Seq[String]) extends Diagnostic(loc, ERROR, "Name Error", details)

class TypeError(loc: Location, details: Seq[String]) extends Diagnostic(loc, ERROR, "Type Error", details)
