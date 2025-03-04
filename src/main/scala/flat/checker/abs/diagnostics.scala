package flat.checker.abs

import flat.checker.{Diagnostic, Location, NameError, TypeError}

class TypeMismatch(loc: Location, expected: String, actual: String) extends TypeError(loc, Seq(
  "type mismatch",
  s"expected: $expected",
  s"actual:   $actual"
))


class AssertionError(loc: Location) extends TypeError(loc, Seq(
  "assertion may not hold",
))