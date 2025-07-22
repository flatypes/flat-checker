package flat.regex


class LengthTest extends RegexTest:

  import REOps.length

  test("len (a|bb)*"):
    val r = re("(a|bb)*")
    assert(length(r) == NatRange(0))

  test("len a{2,}"):
    val r = re("a{2,}")
    assert(length(r) == NatRange(2))

  test("len (a|bb)c{1,4}"):
    val r = re("(a|bb)c{1,4}")
    assert(length(r) == NatRange(2, 6))

  test("len a{4}|a{2}b{2}|(a|b){3}(c|d)"):
    val r = re("a{4}|a{2}b{2}|(a|b){3}(c|d)")
    assert(length(r) == NatRange.at(4))
