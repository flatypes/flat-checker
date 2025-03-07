package flat.checker

import flat.checker.DiagnosticSeverity.FATAL

import java.io.PrintStream
import scala.collection.mutable.ListBuffer

class Issuer:
  private val diagnostics = ListBuffer.empty[Diagnostic]

  def report(diagnostic: Diagnostic): Unit =
    if diagnostic.severity == FATAL then
      throw RuntimeException("Fatal error detected:\n" + diagnostic.longString)
    diagnostics += diagnostic

  def print(to: PrintStream = System.err): Unit =
    for diagnostic <- diagnostics do to.print(diagnostic.longString)

  def noError: Boolean = diagnostics.forall(_.severity != DiagnosticSeverity.ERROR)