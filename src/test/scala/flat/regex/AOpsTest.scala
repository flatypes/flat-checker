package flat.regex

import flat.regex.AOps.*
import flat.regex.REParser.parse as re
import flat.regex.RegEx.RENull
import org.scalatest.funspec.AnyFunSpec

class AOpsTest extends AnyFunSpec:
  describe("abc"):
    val r = re("abc")

    it("drop"):
      assert(r.drop(0) == r)
      assert(r.drop(1) == re("bc"))
      assert(r.drop(2) == re("c"))
      assert(r.drop(3) == RENull)
      assert(r.drop(4) == RENull)

    it("take"):
      assert(r.take(0) == RENull)
      assert(r.take(1) == re("a"))
      assert(r.take(2) == re("ab"))
      assert(r.take(3) == re("abc"))
      assert(r.take(4) == re("abc"))

    it("split"):
      assert(r.splitPrefix('a') == RENull)
      assert(r.splitSuffix('a') == r)
      assert(r.splitPrefix('b') == re("a"))
      assert(r.splitSuffix('b') == re("bc"))
      assert(r.splitPrefix('c') == re("ab"))
      assert(r.splitSuffix('c') == re("c"))

    it("length"):
      assert(r.length == Interval.at(3))

  describe("ab*"):
    val r = re("ab*")

    it("drop"):
      assert(r.drop(1) == re("b*"))
      assert(r.drop(2) == re("b*"))
      assert(r.drop(3) == re("b*"))

    it("take"):
      assert(r.take(1) == re("a"))
      assert(r.take(2) == re("ab?"))
      assert(r.take(3) == re("ab?b?"))

    it("split"):
      assert(r.splitPrefix('a') == RENull)
      assert(r.splitSuffix('a') == r)
      assert(r.splitPrefix('b') == r)
      assert(r.splitSuffix('b') == re("bb*"))
      assert(r.splitPrefix('c').isEmpty)
      assert(r.splitSuffix('c').isEmpty)

    it("find"):
      assert(r.findPrefix('b') == re("a"))
      assert(r.findSuffix('b') == re("bb*"))

    it("length"):
      assert(r.length == Interval(lb = 1))

  describe("a|b*"):
    val r = re("a|b*")

    it("drop"):
      assert(r.drop(1) == re("|b*"))
      assert(r.drop(2) == re("|b*"))

    it("take"):
      assert(r.take(1) == re("a|b?"))
      assert(r.take(2) == re("(a|b?)b?"))

    it("split"):
      assert(r.splitPrefix('a') == RENull)
      assert(r.splitSuffix('a') == re("a"))
      assert(r.splitPrefix('b') == re("b*"))
      assert(r.splitSuffix('b') == re("bb*"))

    it("find"):
      assert(r.findPrefix('b') == RENull)
      assert(r.findSuffix('b') == re("bb*"))

    it("length"):
      assert(r.length == Interval())

  describe("a?b"):
    val r = re("a?b")

    it("drop"):
      assert(r.drop(1) == re("b?"))
      assert(r.drop(2) == RENull)

    it("take"):
      assert(r.take(1) == re("a|b"))
      assert(r.take(2) == re("(a|b)b?")) // NOTE: imprecise

    it("split"):
      assert(r.splitPrefix('a') == RENull)
      assert(r.splitSuffix('a') == re("ab"))
      assert(r.splitPrefix('b') == re("a?"))
      assert(r.splitSuffix('b') == re("b"))

    it("length"):
      assert(r.length == Interval(1, 2))

  describe("a?b?"):
    val r = re("a?b?")

    it("drop"):
      assert(r.drop(1) == re("b?"))
      assert(r.drop(2) == RENull)

    it("take"):
      assert(r.take(1) == re("a|b|"))
      assert(r.take(2) == re("(a|b|)b?")) // NOTE: imprecise

    it("split"):
      assert(r.splitPrefix('a') == RENull)
      assert(r.splitSuffix('a') == re("ab?"))
      assert(r.splitPrefix('b') == re("a?"))
      assert(r.splitSuffix('b') == re("b"))

    it("length"):
      assert(r.length == Interval(0, 2))

  describe("(a|b)+"):
    val r = re("(a|b)+")

    it("drop"):
      assert(r.drop(1) == re("(a|b)*"))
      assert(r.drop(2) == re("(a|b)*"))

    it("take"):
      assert(r.take(1) == re("a|b"))
      assert(r.take(2) == re("(a|b)(a|b)?"))

    it("split"):
      assert(r.splitPrefix('a') == re("|(a|b)+"))
      assert(r.splitSuffix('a') == re("a(a|b)*"))
      assert(r.splitPrefix('b') == re("|(a|b)+"))
      assert(r.splitSuffix('b') == re("b(a|b)*"))

    it("find"):
      assert(r.findPrefix('a') == re("|b+"))
      assert(r.findSuffix('a') == re("a(a|b)*"))
      assert(r.findPrefix('b') == re("|a+"))
      assert(r.findSuffix('b') == re("b(a|b)*"))

    it("length"):
      assert(r.length == Interval(lb = 1))

  describe("(a|ab)*"):
    val r = re("(a|ab)*")

    it("drop"):
      assert(r.drop(1) == re("(|b)(a|ab)*"))
      assert(r.drop(2) == re("(a|ab)*|(|b)(a|ab)*"))

    it("take"):
      assert(r.take(1) == re("a?"))
      assert(r.take(2) == re("a?(b|a?)"))

    it("split"):
      assert(r.splitPrefix('a') == r)
      assert(r.splitSuffix('a') == re("a(a|ab)*|(ab)(a|ab)*"))
      assert(r.splitPrefix('b') == re("(a|ab)*a"))
      assert(r.splitSuffix('b') == re("b(a|ab)*"))

    it("find"):
      assert(r.findPrefix('a') == RENull)
      assert(r.findSuffix('a') == re("a(a|ab)*|ab(a|ab)*"))
      assert(r.findPrefix('b') == re("a*a"))
      assert(r.findSuffix('b') == re("b(a|ab)*"))

    it("length"):
      assert(r.length == Interval())