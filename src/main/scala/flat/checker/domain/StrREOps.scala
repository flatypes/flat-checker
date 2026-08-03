package flat.checker.domain

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