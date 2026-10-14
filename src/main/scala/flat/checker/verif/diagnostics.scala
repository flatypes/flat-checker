package flat.checker.verif

import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Range as Span}

class ProofError(val span: Span, val message: String)
  extends Diagnostic(span, s"Proof Error: $message", DiagnosticSeverity.Error, "flat-checker")

final class IndexOutOfBounds(span: Span)
  extends ProofError(span, s"index might be out of bounds")

final class KeyNotExist(span: Span)
  extends ProofError(span, s"key might not exist in the map")

final class ReftNotProved(span: Span)
  extends ProofError(span, s"type refinement not proved on this value")

final class PreNotProved(span: Span)
  extends ProofError(span, s"a precondition for this call could not be proved")

final class PostNotProved(span: Span)
  extends ProofError(span, s"a postcondition could not be proved on this return path")

final class InvNotProvedOnEntry(span: Span)
  extends ProofError(span, s"this loop invariant could not be proved on entry")

final class InvNotMaintained(span: Span)
  extends ProofError(span, s"this loop invariant could not be proved to be maintained")

final class AssertNotProved(span: Span)
  extends ProofError(span, s"this assertion could not be proved")

