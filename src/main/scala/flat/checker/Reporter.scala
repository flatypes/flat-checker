package flat.checker

import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Range}

import java.io.PrintStream
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

given Ordering[Range] with
  def compare(r1: Range, r2: Range): Int =
    if r1.getStart.getLine < r2.getStart.getLine then -1
    else if r1.getStart.getLine > r2.getStart.getLine then 1
    else if r1.getStart.getCharacter < r2.getStart.getCharacter then -1
    else if r1.getStart.getCharacter > r2.getStart.getCharacter then 1
    else 0

final class Reporter(source: Source):
  private val store = mutable.Map.empty[String, ListBuffer[Diagnostic]]

  def report(uri: String, diagnostic: Diagnostic): Unit =
    store.get(uri) match
      case Some(buf) =>
        buf += diagnostic
      case None =>
        store(uri) = ListBuffer(diagnostic)

  def getDiagnostics(uri: String): List[Diagnostic] =
    store.get(uri) match
      case Some(buf) => buf.toList.sortBy(_.getRange)
      case None => Nil

  def hasErrors: Boolean = store.values.exists(_.exists(_.getSeverity == DiagnosticSeverity.Error))

  def printTo(out: PrintStream): Unit =
    for uri <- store.keys; diagnostic <- getDiagnostics(uri) do
      val range = diagnostic.getRange
      val lineNoWidth = (range.getEnd.getLine + 1).toString.length
      val header = "-".repeat(lineNoWidth)
      val message = diagnostic.getMessage.getLeft
      val title = message.take(message.indexOf(':'))
      val position = s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1}"
      out.println(s"$header $title: ${source.uri}:$position")

      val blankLineNo = " ".repeat(lineNoWidth)
      var caretStart = 0
      for line <- range.getStart.getLine to range.getEnd.getLine do
        val lineNo = (line + 1).toString.padTo(lineNoWidth, ' ')
        val sourceLine = source.getLine(line)
        out.println(s"$lineNo |$sourceLine")
        caretStart = if line == range.getStart.getLine then range.getStart.getCharacter else 0
        val caretEnd = if line == range.getEnd.getLine then range.getEnd.getCharacter else sourceLine.length
        val caretLine = " ".repeat(caretStart) + "^" * (caretEnd - caretStart)
        out.println(s"$blankLineNo |$caretLine")

      val contentLines = message.drop(message.indexOf(':') + 1).trim.split('\n')
      for contentLine <- contentLines do
        val descLine = " ".repeat(caretStart) + contentLine
        out.println(s"$blankLineNo |$descLine")