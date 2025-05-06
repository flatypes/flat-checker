package flat

import flat.DiagnosticSeverity.{ERROR, FATAL}
import flat.{Diagnostic, DiagnosticSeverity}

import java.io.PrintStream
import scala.collection.mutable.ListBuffer

class Issuer:
  private val diagnostics = ListBuffer.empty[Diagnostic]

  def report(diagnostic: Diagnostic): Unit =
    diagnostics += diagnostic

  def ensureNoError(): Unit =
    for diagnostic <- diagnostics do System.err.print(diagnostic.longString)
    if diagnostics.exists(_.severity == ERROR) then System.exit(1)

  def print(to: PrintStream = System.err): Unit =
    for diagnostic <- diagnostics do to.print(diagnostic.longString)

  def noError: Boolean = diagnostics.forall(_.severity != DiagnosticSeverity.ERROR)