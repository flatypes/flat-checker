package flat.checker.domain

import flat.checker.domain.RegEx.*

object StrREOps:
  extension (r: StrRE)
    def filterStartWith(s: String): StrRE = REOps.filterStartsWith(r)(s.toList)

    def absStartsWithStr(s: String): BoolSet = REOps.absStartsWith(r)(s.toList)

    def absEndsWithStr(s: String): BoolSet = REOps.absEndsWith(r)(s.toList)

    def filterContainsStr(s: String): StrRE = REOps.filterContains(r)(s.toList)

    def filterNotContainStr(s: String): StrRE = REOps.filterNotContain(r)(s.toList)

    def absContainsStr(s: String): BoolSet = REOps.absContains(r)(s.toList)

    def absIndexOfStr(s: String): IndexSet = REOps.absIndexOf(r)(s.toList)

    def absCountStr(s: String): CountingRE = REOps.absCount(r)(s.toList)

    def absSplitStr(s: String): RegEx[StrRE] = REOps.absSplit(r)(s.toList)

    def absTrimLeft: StrRE = r match
      case Zero() => Zero()
      case One() => One()
      case Lit(a) =>
        val b = a - ' ' - '\n' - '\r' - '\t'
        if b.isEmpty then One() else Lit(b)
      case Plus(r1, r2) => r1.absTrimLeft + r2.absTrimLeft
      case Comp(r1, r2) =>
        val trimmed1 = r1.absTrimLeft
        if trimmed1 == One() then r2.absTrimLeft else trimmed1 * r2
      case Star(r1) =>
        val trimmed = r1.absTrimLeft
        if trimmed == One() then One() else trimmed.star

    def absTrimRight: StrRE = r.reverse.absTrimLeft.reverse

    def absTrim: StrRE = r.absTrimLeft.absTrimRight