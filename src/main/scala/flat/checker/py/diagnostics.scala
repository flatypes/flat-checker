package flat.checker.py

import flat.checker.DiagnosticSeverity.ERROR
import flat.checker.backend.core.Ident
import flat.checker.{Diagnostic, Location}

class Unsupported(feature: String, loc: Location) extends Diagnostic(loc, ERROR, "Unsupported Feature",
  s"$feature is not supported")

class SyntaxError(detail: String, loc: Location) extends Diagnostic(loc, ERROR, "Syntax Error", detail)

class NameError(detail: String, loc: Location) extends Diagnostic(loc, ERROR, "Name Error", detail)

class Redefined(ident: Ident) extends NameError(s"'${ident.name}' is already defined", ident.loc)

class Undefined(ident: Ident) extends NameError(s"'${ident.name}' is not defined", ident.loc)

class TypeError(detail: String, loc: Location) extends Diagnostic(loc, ERROR, "Type Error", detail)

class TypeMismatch(expected: String, actual: String, loc: Location)
  extends TypeError(s"type mismatch\nexpected: $expected\nactual:   $actual", loc)

class NoAttribute(receiverType: String, attr: String, loc: Location)
  extends TypeError(s"object of type $receiverType has no attribute $attr", loc)
