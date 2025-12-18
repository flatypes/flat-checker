package flat.regex

import flat.regex.AOps.*
import org.scalatest.funspec.AnyFunSpec

class AOpsTest extends AnyFunSpec, REAssertions:
  describe("abc"):
    val r = re("abc")

    it("drop"):
      assertEqual(r.drop(0), r)
      assertEqual(r.drop(1), "bc")
      assertEqual(r.drop(2), "c")
      assertEqual(r.drop(3), "")
      assertEqual(r.drop(4), "")

    it("take"):
      assertEqual(r.take(0), "")
      assertEqual(r.take(1), "a")
      assertEqual(r.take(2), "ab")
      assertEqual(r.take(3), "abc")
      assertEqual(r.take(4), "abc")

    it("split"):
      assertEqual(r.splitPrefix('a'), "")
      assertEqual(r.splitSuffix('a'), r)
      assertEqual(r.splitPrefix('b'), "a")
      assertEqual(r.splitSuffix('b'), "bc")
      assertEqual(r.splitPrefix('c'), "ab")
      assertEqual(r.splitSuffix('c'), "c")

    it("length"):
      assert(r.length == Interval.at(3))

  describe("ab*"):
    val r = re("ab*")

    it("drop"):
      assertEqual(r.drop(1), "b*")
      assertEqual(r.drop(2), "b*")
      assertEqual(r.drop(3), "b*")

    it("take"):
      assertEqual(r.take(1), "a")
      assertEqual(r.take(2), "ab?")
      assertEquiv(r.take(3), "ab?b?")

    it("split"):
      assertEqual(r.splitPrefix('a'), "")
      assertEqual(r.splitSuffix('a'), r)
      assertEqual(r.splitPrefix('b'), r)
      assertEqual(r.splitSuffix('b'), "bb*")
      assertEmpty(r.splitPrefix('c'))
      assertEmpty(r.splitSuffix('c'))

    it("find"):
      assertEqual(r.findPrefix('b'), "a")
      assertEqual(r.findSuffix('b'), "bb*")

    it("length"):
      assert(r.length == Interval(lb = 1))

  describe("a|b*"):
    val r = re("a|b*")

    it("drop"):
      assertEqual(r.drop(1), "|b*")
      assertEqual(r.drop(2), "|b*")

    it("take"):
      assertEquiv(r.take(1), "a||b")
    // assertEquiv(r.take(2), "a||b|bb")

    it("split"):
      assertEqual(r.splitPrefix('a'), "")
      assertEqual(r.splitSuffix('a'), "a")
      assertEqual(r.splitPrefix('b'), "b*")
      assertEqual(r.splitSuffix('b'), "bb*")

    it("find"):
      assertEqual(r.findPrefix('b'), "")
      assertEqual(r.findSuffix('b'), "bb*")

    it("length"):
      assert(r.length == Interval(lb = 0))

  describe("a?b"):
    val r = re("a?b")

    it("drop"):
      assertEqual(r.drop(1), "b?")
      assertEqual(r.drop(2), "")

    it("take"):
      assertEquiv(r.take(1), "a|b")
      assertEquiv(r.take(2), "(a|b)b?") // NOTE: imprecise

    it("split"):
      assertEqual(r.splitPrefix('a'), "")
      assertEqual(r.splitSuffix('a'), "ab")
      assertEqual(r.splitPrefix('b'), "a?")
      assertEqual(r.splitSuffix('b'), "b")

    it("length"):
      assert(r.length == Interval(1, 2))

  describe("a?b?"):
    val r = re("a?b?")

    it("drop"):
      assertEqual(r.drop(1), "b?")
      assertEqual(r.drop(2), "")

    it("take"):
      assertEquiv(r.take(1), "a|b|")
      assertEquiv(r.take(2), "(a|b|)b?") // NOTE: imprecise

    it("split"):
      assertEqual(r.splitPrefix('a'), "")
      assertEqual(r.splitSuffix('a'), "ab?")
      assertEqual(r.splitPrefix('b'), "a?")
      assertEqual(r.splitSuffix('b'), "b")

    it("length"):
      assert(r.length == Interval(0, 2))

  describe("(a|b)+"):
    val r = re("(a|b)+")

    it("drop"):
      assertEqual(r.drop(1), "(a|b)*")
      assertEqual(r.drop(2), "(a|b)*")

    it("take"):
      assertEquiv(r.take(1), "a|b")
      assertEquiv(r.take(2), "(a|b)(a|b)?")

    it("split"):
      assertEqual(r.splitPrefix('a'), "|(a|b)+")
      assertEqual(r.splitSuffix('a'), "a(a|b)*")
      assertEqual(r.splitPrefix('b'), "|(a|b)+")
      assertEqual(r.splitSuffix('b'), "b(a|b)*")

    it("find"):
      assertEqual(r.findPrefix('a'), "|b+")
      assertEqual(r.findSuffix('a'), "a(a|b)*")
      assertEqual(r.findPrefix('b'), "|a+")
      assertEqual(r.findSuffix('b'), "b(a|b)*")

    it("length"):
      assert(r.length == Interval(lb = 1))

  describe("(a|ab)*"):
    val r = re("(a|ab)*")

    it("drop"):
      assertEqual(r.drop(1), "(|b)(a|ab)*")
      assertEqual(r.drop(2), "(a|ab)*|(|b)(a|ab)*")

    it("take"):
      assertEqual(r.take(1), "a?")
      assertEquiv(r.take(2), "|a|a(a|b)")

    it("split"):
      assertEqual(r.splitPrefix('a'), r)
      assertEqual(r.splitSuffix('a'), "a(a|ab)*|(ab)(a|ab)*")
      assertEqual(r.splitPrefix('b'), "(a|ab)*a")
      assertEqual(r.splitSuffix('b'), "b(a|ab)*")

    it("find"):
      assertEqual(r.findPrefix('a'), "")
      assertEqual(r.findSuffix('a'), "a(a|ab)*|ab(a|ab)*")
      assertEqual(r.findPrefix('b'), "a*a")
      assertEqual(r.findSuffix('b'), "b(a|ab)*")

    it("length"):
      assert(r.length == Interval(lb = 0))