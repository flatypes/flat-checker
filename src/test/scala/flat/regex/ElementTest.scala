package flat.regex

class ElementTest extends RegexTest:

  import REOps.{alphabet, contains, startsWith}

  test("a(bc)*b"):
    val r = re("a(bc)*b")
    assert(alphabet(r) == ch('a', 'b', 'c'))

    assert(contains(r, 'a').contains(true))
    assert(contains(r, 'b').contains(true))
    assert(contains(r, 'c').isEmpty)
    assert(contains(r, 'd').contains(false))

    assert(startsWith(r, "a").contains(true))
    assert(startsWith(r, "ab").contains(true))
    assert(startsWith(r, "abc").isEmpty)
    assert(startsWith(r, "abb").contains(false))

  test("a(b|bc)"):
    val r = re("a(b|bc)")
    assert(alphabet(r) == ch('a', 'b', 'c'))

    assert(contains(r, 'a').contains(true))
    assert(contains(r, 'b').contains(true))
    assert(contains(r, 'c').isEmpty)

    assert(startsWith(r, "a").contains(true))
    assert(startsWith(r, "ab").contains(true))
    assert(startsWith(r, "abc").isEmpty)
    assert(startsWith(r, "abb").contains(false))

  test("(a|b)c+"):
    val r = re("(a|b)c+")
    assert(alphabet(r) == ch('a', 'b', 'c'))

    assert(contains(r, 'a').isEmpty)
    assert(contains(r, 'b').isEmpty)
    assert(contains(r, 'c').contains(true))

    assert(startsWith(r, "a").isEmpty)
    assert(startsWith(r, "b").isEmpty)
    assert(startsWith(r, "ac").isEmpty)
    assert(startsWith(r, "bcc").isEmpty)
    assert(startsWith(r, "c").contains(false))
    assert(startsWith(r, "ab").contains(false))
