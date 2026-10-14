package flat.checker.typing

import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Range as Span}

class NameError(val span: Span, val message: String)
  extends Diagnostic(span, s"Name Error: $message", DiagnosticSeverity.Error, "flat-checker")

class UndefinedName(span: Span, val name: String)
  extends NameError(span, s"undefined name '$name'")

class RedefinedName(span: Span, val name: String)
  extends NameError(span, s"redefined name '$name'")

class TypeError(val span: Span, val message: String)
  extends Diagnostic(span, s"Type Error: $message", DiagnosticSeverity.Error, "flat-checker")

final class NotTerm(span: Span)
  extends TypeError(span, s"not term")

final class NotConstant(span: Span)
  extends TypeError(span, s"not constant")

final class NotAssignable(span: Span)
  extends TypeError(span, s"not assignable")

final class MissingTypeAnnot(span: Span)
  extends TypeError(span, "missing type annotation")

final class MissingReturnValue(span: Span)
  extends TypeError(span, "missing return value")

final class BreakOutOfLoop(span: Span)
  extends TypeError(span, "break statement outside of loop")

final class ContinueOutOfLoop(span: Span)
  extends TypeError(span, "continue statement outside of loop")

final class ArityMismatch(span: Span, expected: Int, actual: Int)
  extends TypeError(span, s"arity mismatch: expected $expected arguments, found $actual")

final class TypeMismatch(span: Span, expected: String, actual: String)
  extends TypeError(span, s"type mismatch: expected $expected, found $actual")

final class PatternTypeMismatch(span: Span, expected: String, actual: String)
  extends TypeError(span, s"pattern type mismatch: expected $expected, found $actual")

final class NoMatchingOverload(span: Span, name: String)
  extends TypeError(span, s"no matching overload for '$name'")