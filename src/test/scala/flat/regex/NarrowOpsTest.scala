package flat.regex

import flat.regex.NarrowOps.*
import flat.regex.REParser.parse as re
import org.scalatest.funspec.AnyFunSpec

class NarrowOpsTest extends AnyFunSpec:
  describe("ab*"):
    val r = re("ab*")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == r)
      assert(r.narrowByChatAt(0, CharSet('b')).isEmpty)
      assert(r.narrowByChatAt(1, CharSet('b')) == re("abb*"))
      assert(r.narrowByChatAt(1, CharSet('a')).isEmpty)

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == r)
      assert(r.narrowByFirstIndexOf('b', Interval.at(0)).isEmpty)
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 1)) == re("abb*"))

  describe("a|b*"):
    val r = re("a|b*")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == re("a"))
      assert(r.narrowByChatAt(0, CharSet('b')) == re("bb*"))
      assert(r.narrowByChatAt(1, CharSet('b')) == re("bbb*"))
      assert(r.narrowByChatAt(1, CharSet.not('b')).isEmpty)

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == re("a"))
      assert(r.narrowByFirstIndexOf('a', Interval(lb = 1)).isEmpty)
      assert(r.narrowByFirstIndexOf('b', Interval.at(0)) == re("bb*"))
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 1)).isEmpty)

  describe("a?b"):
    val r = re("a?b")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == re("ab"))
      assert(r.narrowByChatAt(0, CharSet('b')) == re("b"))
      assert(r.narrowByChatAt(1, CharSet('b')) == re("ab"))
      assert(r.narrowByChatAt(1, CharSet('a')).isEmpty)

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == re("ab"))
      assert(r.narrowByFirstIndexOf('a', Interval(lb = 1)).isEmpty)
      assert(r.narrowByFirstIndexOf('b', Interval.at(0)) == re("b"))
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 1)) == re("ab"))

  describe("a?b?"):
    val r = re("a?b?")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == re("ab?"))
      assert(r.narrowByChatAt(0, CharSet('b')) == re("b"))
      assert(r.narrowByChatAt(1, CharSet('b')) == re("ab"))
      assert(r.narrowByChatAt(1, CharSet.not('b')).isEmpty)

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == re("ab?"))
      assert(r.narrowByFirstIndexOf('a', Interval(lb = 1)).isEmpty)
      assert(r.narrowByFirstIndexOf('b', Interval.at(0)) == re("b"))
      assert(r.narrowByFirstIndexOf('b', Interval.at(1)) == re("ab"))
      assert(r.narrowByFirstIndexOf('b', Interval(ub = 1)) == re("a?b"))

  describe("(a|b)+"):
    val r = re("(a|b)+")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == re("a(a|b)*"))
      assert(r.narrowByChatAt(0, CharSet('b')) == re("b(a|b)*"))
      assert(r.narrowByChatAt(1, CharSet('a')) == re("(a|b)a(a|b)*"))
      assert(r.narrowByChatAt(1, CharSet('b')) == re("(a|b)b(a|b)*"))

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == re("a(a|b)*"))
      assert(r.narrowByFirstIndexOf('a', Interval.at(1)) == re("ba(a|b)*"))
      assert(r.narrowByFirstIndexOf('a', Interval(lb = 2)) == re("bbb*a(a|b)*"))
      assert(r.narrowByFirstIndexOf('b', Interval.at(0)) == re("b(a|b)*"))
      assert(r.narrowByFirstIndexOf('b', Interval.at(1)) == re("ab(a|b)*"))
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 2)) == re("aaa*b(a|b)*"))

  describe("(a|ab)*"):
    val r = re("(a|ab)*")

    it("narrow by char-at"):
      assert(r.narrowByChatAt(0, CharSet('a')) == re("a(|b)(a|ab)*"))
      assert(r.narrowByChatAt(1, CharSet('a')) == re("aa(|b)(a|ab)*"))
      assert(r.narrowByChatAt(1, CharSet('b')) == re("ab(a|ab)*"))

    it("narrow by index-of"):
      assert(r.narrowByFirstIndexOf('a', Interval.at(0)) == re("a(|b)(a|ab)*"))
      assert(r.narrowByFirstIndexOf('a', Interval(lb = 1)).isEmpty)
      assert(r.narrowByFirstIndexOf('b', Interval.at(1)) == re("ab(a|ab)*"))
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 2)) == re("aa*ab(a|ab)*"))
      assert(r.narrowByFirstIndexOf('b', Interval(lb = 3)) == re("aaa*ab(a|ab)*"))
      assert(r.narrowByFirstIndexOf('b', Interval(ub = 3)) == re("(|a|aa)ab(a|ab)*"))
