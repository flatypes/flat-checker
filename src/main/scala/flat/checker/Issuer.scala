package flat.checker

import java.io.PrintStream
import scala.collection.mutable.ListBuffer

class Issuer:
  private val diagnostics = ListBuffer.empty[Diagnostic]

  def report(diagnostic: Diagnostic): Unit =
    diagnostics += diagnostic

  def print(to: PrintStream = System.err): Unit =
    for diagnostic <- diagnostics do to.print(diagnostic.longString)

  def noError: Boolean = diagnostics.forall(_.severity != DiagnosticSeverity.ERROR)