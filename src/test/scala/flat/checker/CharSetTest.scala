package flat.checker

import org.scalatest.funsuite.AnyFunSuite

class CharSetTest extends AnyFunSuite {
  test("predefined") {
    assert((CharSet.ALPHA_UPPER | CharSet.ALPHA_LOWER) == CharSet.ALPHA)
    assert((CharSet.ALPHA_LOWER & CharSet.ALPHA_UPPER).isEmpty)
    assert(CharSet.NUM ** CharSet.ALPHA)
    assert(CharSet.ALPHA_LOWER.subsetOf(CharSet.ALPHA))
    assert(CharSet.NUM.subsetOf(CharSet.ALPHA_NUM))
    assert(CharSet.ALPHA.subsetOf(CharSet.ALPHA_NUM))
    assert(CharSet.ALPHA_UPPER.subsetOf(CharSet.ALPHA_NUM))
    assert(CharSet.ALPHA_NUM.prettyString == "[0-9A-Za-z]")
  }

  test("union") {
    val s1 = CharSet.complementOf('a', 'b')
    val s2 = CharSet.complementOf('a', 'A')
    val s3 = s1 | s2
    assert(!s3.contains('a'))
    assert(s3.contains('b'))
    assert(s3.contains('A'))
    assert(s3.contains('c'))

    val s4 = CharSet.of('a', 'b')
    val s5 = s4 | s2
    assert(s5.contains('a'))
    assert(s5.contains('b'))
    assert(!s5.contains('A'))
    assert(s5.contains('c'))
  }

  test("intersect") {
    val s1 = CharSet.complementOf('a', 'b')
    val s2 = CharSet.complementOf('a', 'A')
    val s3 = s1 & s2
    assert(!s3.contains('a'))
    assert(!s3.contains('b'))
    assert(!s3.contains('A'))
    assert(s3.contains('c'))

    val s4 = CharSet.of('a', 'b')
    val s5 = s4 & s2
    assert(s5.isSingleton)
    assert(s5.contains('b'))
  }

  test("disjointness") {
    val s1 = CharSet.of('a')
    val s2 = CharSet.complementOf('a')
    assert(s1 ** s2)
  }

  test("subset") {
    val s1 = CharSet.complementOf('a')
    val s2 = CharSet.complementOf('a', 'A')
    assert(s2.subsetOf(s1))
    assert(!s1.subsetOf(s2))

    val s3 = CharSet.from('b' to 'z')
    assert(s3.subsetOf(s1))
    assert(s3.subsetOf(s2))
    assert(!s2.subsetOf(s3))
  }

  test("compress") {
    val s = CharSet.of('a', 'b', 'c', 'e', 'f', '1', '2', '4', '5', '6', '8', '.')
    assert(s.prettyString == "[1-24-68a-ce-f.]")
  }
}
