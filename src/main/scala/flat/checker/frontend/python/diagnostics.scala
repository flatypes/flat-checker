package flat.checker.frontend.python

import flat.checker.DiagnosticSeverity.ERROR
import flat.checker.{Diagnostic, Location, NameError, TypeError}

class UndefinedName(loc: Location) extends NameError(loc, Seq("name is not defined"))

class RedefinedName(loc: Location) extends NameError(loc, Seq("double definition"))

class ArityMismatch(loc: Location, expected: Int, actual: Int) extends TypeError(loc, Seq(
  if actual < expected then s"missing ${expected - actual} required argument(s)" else "too many arguments",
  s"expected: $expected",
  s"actual:   $actual"
))

class TypeMismatch(loc: Location, expected: String, actual: String) extends TypeError(loc, Seq(
  "type mismatch",
  s"expected: $expected",
  s"actual:   $actual"
))

class AttributeError(loc: Location, attr: String, receiverType: String)
  extends Diagnostic(loc, ERROR, "Attribute Error", Seq(s"object of type $receiverType has no attribute $attr"))

class UnsupportedFeature(loc: Location) extends Diagnostic(loc, ERROR, "Unsupported Feature")