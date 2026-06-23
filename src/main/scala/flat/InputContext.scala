package flat

import org.eclipse.lsp4j.Diagnostic

import java.io.PrintStream

class InputContext(sources: Map[String, String]):
  def printDiagnostics(uri: String, diagnostics: List[Diagnostic]): Unit =
    for d <- diagnostics do
      printDiagnostic(uri, d)

  private def printDiagnostic(uri: String, diagnostic: Diagnostic, out: PrintStream = System.err): Unit =
    val range = diagnostic.getRange
    val lineNoWidth = (range.getEnd.getLine + 1).toString.length
    val header = "-".repeat(lineNoWidth)
    val message = diagnostic.getMessage.getLeft
    val title = message.take(message.indexOf(':'))
    val position = s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1}"
    out.println(s"$header $title: $uri:$position")

    val blankLineNo = " ".repeat(lineNoWidth)
    var caretStart = 0
    for line <- range.getStart.getLine to range.getEnd.getLine do
      val lineNo = (line + 1).toString.padTo(lineNoWidth, ' ')
      val sourceLine = getSourceLine(uri, line)
      out.println(s"$lineNo |$sourceLine")
      caretStart = if line == range.getStart.getLine then range.getStart.getCharacter else 0
      val caretEnd = if line == range.getEnd.getLine then range.getEnd.getCharacter else sourceLine.length
      val caretLine = " ".repeat(caretStart) + "^" * (caretEnd - caretStart)
      out.println(s"$blankLineNo |$caretLine")

    val desc = message.drop(message.indexOf(':') + 1).trim
    if desc.nonEmpty then
      val descLine = " ".repeat(caretStart) + desc
      out.println(s"$blankLineNo |$descLine")

  private def getSourceLine(uri: String, line: Int): String =
    val text = sources(uri)
    text.split('\n')(line)
