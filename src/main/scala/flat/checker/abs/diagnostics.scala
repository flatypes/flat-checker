package flat.checker.abs

import flat.checker.DiagnosticSeverity.WARN
import flat.checker.{Diagnostic, Location, NameError, TypeError}

class TypeMismatch(loc: Location, expected: String, actual: String) extends TypeError(loc, Seq(
  "type may mismatch",
  s"expected: $expected",
  s"actual:   $actual"
))

class AssertionError(loc: Location) extends TypeError(loc, Seq(
  "assertion may not hold",
))

class IndexOutOfBounds(loc: Location) extends TypeError(loc, Seq("index may out of bound"))

class OverApprox(loc: Location, reason: String) extends Diagnostic(loc, WARN, "Over-approximation",
  Seq(s"cannot infer a precise result because $reason"))