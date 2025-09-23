package flat.regex

import flat.regex.NarrowOps.*
import org.scalatest.funspec.AnyFunSpec

class NarrowOpsTest extends AnyFunSpec, REAssertions:
  describe("ab*"):
    val r = re("ab*")

    it("narrow by length"):
      assertEqual(r.narrowByLength(Interval(lb = 1)), r)
      assertEqual(r.narrowByLength(Interval(lb = 2)), "ab+")
      assertEqual(r.narrowByLength(Interval(lb = 3)), "ab{2,}")

      assertEmpty(r.narrowByLength(Interval(ub = 0)))
      assertEqual(r.narrowByLength(Interval(ub = 1)), "a")
      assertEqual(r.narrowByLength(Interval(ub = 2)), "ab?")
      assertEqual(r.narrowByLength(Interval(ub = 3)), "ab{,2}")

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), r)
      assertEmpty(r.narrowByFirstIndexOf('b', Interval.at(0)))
      assertEqual(r.narrowByFirstIndexOf('b', Interval(lb = 1)), "ab+")

    it("narrow by contain"):
      assertEqual(r.narrowByContain('a'), r)
      assertEqual(r.narrowByContain('b'), "ab+")
      assertEmpty(r.narrowByContain('c'))

    it("narrow by not contain"):
      assertEmpty(r.narrowByNotContain('a'))
      assertEqual(r.narrowByNotContain('b'), "a")
      assertEqual(r.narrowByNotContain('c'), r)

    it("narrow by char at"):
      assertEqual(r.narrowByChatAt(0, CharSet('a')), r)
      assertEmpty(r.narrowByChatAt(0, CharSet('b')))
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "ab+")
      assertEmpty(r.narrowByChatAt(1, CharSet('a')))

    it("narrow by eq"):
      assertEqual(r.narrowByEq("abb"), "abb")

    it("narrow by not eq"):
      assertEqual(r.narrowByNotEq("ab"), "a|ab{2,}")

  describe("a|b*"):
    val r = re("a|b*")

    it("narrow by length"):
      assertEqual(r.narrowByLength(Interval.at(0)), "")
      assertEqual(r.narrowByLength(Interval.at(1)), "a|b")
      assertEqual(r.narrowByLength(Interval.at(2)), "b{2}")

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), "a")
      assertEmpty(r.narrowByFirstIndexOf('a', Interval(lb = 1)))
      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(0)), "b+")
      assertEmpty(r.narrowByFirstIndexOf('b', Interval(lb = 1)))

    it("narrow by contain"):
      assertEqual(r.narrowByContain('a'), "a")
      assertEqual(r.narrowByContain('b'), "b+")

    it("narrow by not contain"):
      assertEqual(r.narrowByNotContain('a'), "b*")
      assertEqual(r.narrowByNotContain('b'), "a?")

    it("narrow by contain not"):
      assertEqual(r.narrowByContainNot('a'), "b*")
      assertEqual(r.narrowByContainNot('b'), "a")

    it("narrow by char at"):
      assertEqual(r.narrowByChatAt(0, CharSet('a')), "a")
      assertEqual(r.narrowByChatAt(0, CharSet('b')), "b+")
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "b{2,}")
      assertEmpty(r.narrowByChatAt(1, CharSet.not('b')))

    it("narrow by not eq"):
      assertEqual(r.narrowByNotEq("bb"), "a|b?|b{3,}")

  describe("a?b"):
    val r = re("a?b")

    it("narrow by length"):
      assertEmpty(r.narrowByLength(Interval.at(0)))
      assertEqual(r.narrowByLength(Interval.at(1)), "b")
      assertEqual(r.narrowByLength(Interval.at(2)), "ab")

      assertEqual(r.narrowByLength(Interval(ub = 1)), "b")
      assertEqual(r.narrowByLength(Interval(ub = 2)), r)
      assertEmpty(r.narrowByLength(Interval(lb = 3)))

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), "ab")
      assertEmpty(r.narrowByFirstIndexOf('a', Interval(lb = 1)))
      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(0)), "b")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(lb = 1)), "ab")

    it("narrow by contain"):
      assertEqual(r.narrowByContain('a'), "ab")
      assertEqual(r.narrowByContain('b'), r)

    it("narrow by not contain"):
      assertEqual(r.narrowByNotContain('a'), "b")
      assertEmpty(r.narrowByNotContain('b'))

    it("narrow by char at"):
      assertEqual(r.narrowByChatAt(0, CharSet('a')), "ab")
      assertEqual(r.narrowByChatAt(0, CharSet('b')), "b")
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "ab")
      assertEmpty(r.narrowByChatAt(1, CharSet('a')))

    it("narrow by not eq"):
      assertEqual(r.narrowByNotEq("ab"), "b")
      assertEqual(r.narrowByNotEq("b"), "ab")

  describe("a?b?"):
    val r = re("a?b?")

    // NOTE: cannot narrow length

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), "ab?")
      assertEmpty(r.narrowByFirstIndexOf('a', Interval(lb = 1)))

      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(0)), "b")
      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(1)), "ab")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(ub = 1)), "a?b")

    it("narrow by contain"):
      assertEqual(r.narrowByContain('a'), "ab?")
      assertEqual(r.narrowByContain('b'), "a?b")

    it("narrow by not contain"):
      assertEqual(r.narrowByNotContain('a'), "b?")
      assertEqual(r.narrowByNotContain('b'), "a?")

    it("narrow by char at"):
      assertEqual(r.narrowByChatAt(0, CharSet('a')), "ab?")
      assertEqual(r.narrowByChatAt(0, CharSet('b')), "b")
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "ab")
      assertEmpty(r.narrowByChatAt(1, CharSet.not('b')))

  describe("(a|b)+"):
    val r = re("(a|b)+")

    it("narrow by length"):
      assertEqual(r.narrowByLength(Interval.at(1)), "a|b")
      assertEqual(r.narrowByLength(Interval.at(2)), "(a|b){2}")
      assertEqual(r.narrowByLength(Interval(lb = 3)), "(a|b){3,}")

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), "a(a|b)*")
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(1)), "ba(a|b)*")
      assertEqual(r.narrowByFirstIndexOf('a', Interval(lb = 2)), "b{2,}a(a|b)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(0)), "b(a|b)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(1)), "ab(a|b)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(lb = 2)), "a{2,}b(a|b)*")

    it("narrow by contain"):
      assertEquiv(r.narrowByContain('a'), "b*a(a|b)*")
      assertEquiv(r.narrowByContain('b'), "a*b(a|b)*")

    it("narrow by not contain"):
      assertEqual(r.narrowByNotContain('a'), "b+")
      assertEqual(r.narrowByNotContain('b'), "a+")

    it("narrow by char at"):
      assertEqual(r.narrowByChatAt(0, CharSet('a')), "a(a|b)*")
      assertEqual(r.narrowByChatAt(0, CharSet('b')), "b(a|b)*")
      assertEqual(r.narrowByChatAt(1, CharSet('a')), "(a|b)a(a|b)*")
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "(a|b)b(a|b)*")

  describe("(a|ab)*"):
    val r = re("(a|ab)*")

    it("narrow by first index of"):
      assertEqual(r.narrowByFirstIndexOf('a', Interval.at(0)), "a(|b)(a|ab)*")
      assertEmpty(r.narrowByFirstIndexOf('a', Interval(lb = 1)))

      assertEqual(r.narrowByFirstIndexOf('b', Interval.at(1)), "ab(a|ab)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(lb = 2)), "a+ab(a|ab)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(lb = 3)), "a{2,}ab(a|ab)*")
      assertEqual(r.narrowByFirstIndexOf('b', Interval(ub = 3)), "a{,2}ab(a|ab)*")

    it("narrow by contain"):
      assertEquiv(r.narrowByContain('a'), "(a|ab)+")
      assertEqual(r.narrowByContain('b'), "a*ab(a|ab)*")

    it("narrow by not contain"):
      assertEqual(r.narrowByNotContain('a'), "")
      assertEqual(r.narrowByNotContain('b'), "a*")

    it("narrow by char at"):
      assertEquiv(r.narrowByChatAt(0, CharSet('a')), "(a|ab)+")
      assertEquiv(r.narrowByChatAt(1, CharSet('a')), "a(a|ab)+")
      assertEqual(r.narrowByChatAt(1, CharSet('b')), "ab(a|ab)*")
