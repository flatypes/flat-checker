package flat.checker

import flat.DiagnosticSeverity.{ERROR, INFO, WARN}
import flat.{Diagnostic, Location}

class SortError(detail: String, loc: Location)
  extends Diagnostic(loc, ERROR, "Transpiled program is ill-sorted", detail)

class TypeError(detail: String, loc: Location) extends Diagnostic(loc, ERROR, "Type Error", detail)

class TypeMayMismatch(expected: String, actual: String, loc: Location)
  extends TypeError(s"type may mismatch\nexpected: $expected\nactual:   $actual", loc)

class AssertionMayFail(loc: Location) extends TypeError("assertion may fail", loc)

class InvariantMayViolate(loc: Location) extends TypeError("invariant may violate", loc)

class IndexMayOutOfBounds(loc: Location) extends TypeError("index may be out of bounds", loc)

class OverApprox(reason: String, loc: Location)
  extends Diagnostic(loc, WARN, "Over-approximation", s"cannot infer a precise result because $reason")

class ShowType(inferred: String, loc: Location) extends Diagnostic(loc, INFO, "Show type", inferred)