package flat.checker.py

import flat.DiagnosticSeverity.ERROR
import flat.checker.py.ast.Ident
import flat.{Diagnostic, Location}

class Unsupported(feature: String, loc: Location) extends Diagnostic(loc, "Unsupported Feature", s"$feature is not supported", ERROR)

class SyntaxError(detail: String, loc: Location) extends Diagnostic(loc, "Syntax Error", detail, ERROR)

class NameError(detail: String, loc: Location) extends Diagnostic(loc, "Name Error", detail, ERROR)

class Redefined(ident: Ident) extends NameError(s"'${ident.name}' is already defined", ident.loc)

class Undefined(ident: Ident) extends NameError(s"'${ident.name}' is not defined", ident.loc)

class TypeError(detail: String, loc: Location) extends Diagnostic(loc, "Type Error", detail, ERROR)

class TypeMismatch(expected: String, actual: String, loc: Location)
  extends TypeError(s"type mismatch\nexpected: $expected\nactual:   $actual", loc)

class NoAttribute(receiverType: String, attr: String, loc: Location)
  extends TypeError(s"object of type $receiverType has no attribute $attr", loc)
