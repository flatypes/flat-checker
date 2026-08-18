package flat.checker.verif

import flat.checker.Reportable
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Range}

abstract class VerifError(val tag: String, range: Range, message: String) extends Reportable:
  override def diagnostic: Diagnostic =
    Diagnostic(range, "Verification Error: " + message, DiagnosticSeverity.Error, "verifier")

final class IndexOutOfBoundsError(range: Range)
  extends VerifError("index", range, "index out of bounds")

final class IndexNegError(range: Range)
  extends VerifError("key", range, "index might be negative")

final class EmptyIndexRangeError(range: Range)
  extends VerifError("index", range, "index range might be empty")

final class KeyNotExistError(range: Range)
  extends VerifError("key", range, "key might not exist in the map")

final class ReftNotProvedError(range: Range)
  extends VerifError("reft", range, "type refinement could not be proved on this value")

final class PreNotProvedError(range: Range)
  extends VerifError("pre", range, "a precondition for this call could not be proved")

final class PostNotProvedError(range: Range)
  extends VerifError("post", range, "a postcondition could not be proved on this return path")

final class InvNotProvedOnEntryError(range: Range)
  extends VerifError("inv entry", range, "this loop invariant could not be proved on entry")

final class InvNotMaintainedError(range: Range)
  extends VerifError("inv maintain", range, "this loop invariant could not be proved to be maintained")

final class AssertNotProvedError(range: Range)
  extends VerifError("assert", range, "this assertion could not be proved")
