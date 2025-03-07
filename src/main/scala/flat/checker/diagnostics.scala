package flat.checker

import flat.checker.DiagnosticSeverity.{FATAL, INFO, WARN}

class IllSorted(loc: Location, details: Seq[String]) extends Diagnostic(loc, FATAL, "Program is ill-sorted", details)

class Undefined(category: String, name: String, loc: Location) extends IllSorted(loc, Seq(
  s"$category '$name' is not defined"))

class Redefined(category: String, name: String, loc: Location) extends IllSorted(loc, Seq(
  s"$category '$name' has already been defined"))

class ArityMismatch(expected: Int, actual: Int, loc: Location) extends IllSorted(loc, Seq(
  "number of arguments mismatch",
  s"expected: $expected",
  s"actual:   $actual"
))

class SortMismatch(expected: String, actual: String, loc: Location) extends IllSorted(loc, Seq(
  "type mismatch",
  s"expected: $expected",
  s"actual:   $actual"
))

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

class ShowType(loc: Location, inferred: String) extends Diagnostic(loc, INFO, "Show type",
  Seq(inferred))