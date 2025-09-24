package flat.regex

import org.scalatest.funsuite.AnyFunSuite

class CharSetTest extends AnyFunSuite:
  test("subset"):
    val s1 = CharSet.not('a')
    val s2 = CharSet.not('a', 'A')
    assert(s2.subsetOf(s1))
    assert(!s1.subsetOf(s2))

    val s3 = CharSet.from('b' to 'z')
    assert(s3.subsetOf(s1))
    assert(s3.subsetOf(s2))
    assert(!s2.subsetOf(s3))

    val s4 = CharSet.from(CharSet.allChars.toSet - 'a')
    assert(s1.subsetOf(s4))
    assert(s2.subsetOf(s4))

  test("union"):
    val s1 = CharSet.not('a', 'b')
    val s2 = CharSet.not('a', 'A')
    val s3 = s1 | s2
    assert(!s3.contains('a'))
    assert(s3.contains('b'))
    assert(s3.contains('A'))
    assert(s3.contains('c'))

    val s4 = CharSet('a', 'b')
    val s5 = s4 | s2
    assert(s5.contains('a'))
    assert(s5.contains('b'))
    assert(!s5.contains('A'))
    assert(s5.contains('c'))

  test("intersection"):
    val s1 = CharSet.not('a', 'b')
    val s2 = CharSet.not('a', 'A')
    val s3 = s1 & s2
    assert(!s3.contains('a'))
    assert(!s3.contains('b'))
    assert(!s3.contains('A'))
    assert(s3.contains('c'))

    val s4 = CharSet('a', 'b')
    val s5 = s4 & s2
    assert(s5.isSingleton)
    assert(s5.contains('b'))

  test("disjointness"):
    val s1 = CharSet('a')
    val s2 = CharSet.not('a')
    assert(s1 ** s2)

  test("from ranges"):
    val NUM: CharSet = CharSet.from('0' to '9')
    val ALPHA_LOWER: CharSet = CharSet.from('a' to 'z')
    val ALPHA_UPPER: CharSet = CharSet.from('A' to 'Z')
    val ALPHA: CharSet = ALPHA_LOWER | ALPHA_UPPER
    val ALPHA_NUM: CharSet = ALPHA | NUM

    assert((ALPHA_UPPER | ALPHA_LOWER) == ALPHA)
    assert((ALPHA_LOWER & ALPHA_UPPER).isEmpty)
    assert(NUM ** ALPHA)
    assert(ALPHA_LOWER.subsetOf(ALPHA))
    assert(NUM.subsetOf(ALPHA_NUM))
    assert(ALPHA.subsetOf(ALPHA_NUM))
    assert(ALPHA_UPPER.subsetOf(ALPHA_NUM))

  test("SMT encoding"):
    val cs1 = CharSet.not(';')
    assert(cs1.toSMT == (Seq(';'), false))

    val cs2 = CharSet('A', 'B', 'C', 'F', 'G', 'K')
    assert(cs2.toSMT == (Seq('A' -> 'C', 'F' -> 'G', 'K'), true))

    val cs3 = CharSet.from(('a' to 'f') ++ ('x' to 'z')) | CharSet('1', '2', '3', '4', '9', '}')
    assert(cs3.toSMT == (Seq('1' -> '4', '9', 'a' -> 'f', 'x' -> 'z', '}'), true))
