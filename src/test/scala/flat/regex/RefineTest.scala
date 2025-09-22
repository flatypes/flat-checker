package flat.regex

import flat.Ops.CmpOp.*
import flat.regex.NarrowOps.*
import flat.regex.REParser.parse as re
import org.scalatest.funspec.AnyFunSpec

class RefineTest extends AnyFunSpec:
  describe("ab*"):
    val r = re("ab*")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == r)
      assert(refineByCharAt(r, 0, (EQ, 'b')).isEmpty)
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("abb*"))
      assert(refineByCharAt(r, 1, (EQ, 'a')).isEmpty)

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == r)
      assert(refineByIndexOf(r, 'b', (EQ, 0)).isEmpty)
      assert(refineByIndexOf(r, 'b', (GE, 1)) == re("abb*"))

  describe("a|b*"):
    val r = re("a|b*")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == re("a"))
      assert(refineByCharAt(r, 0, (EQ, 'b')) == re("bb*"))
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("bbb*"))
      assert(refineByCharAt(r, 1, (NE, 'b')).isEmpty)

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a"))
      assert(refineByIndexOf(r, 'a', (GT, 0)).isEmpty)
      assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("bb*"))
      assert(refineByIndexOf(r, 'b', (GT, 0)).isEmpty)

  describe("a?b"):
    val r = re("a?b")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == re("ab"))
      assert(refineByCharAt(r, 0, (EQ, 'b')) == re("b"))
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab"))
      assert(refineByCharAt(r, 1, (EQ, 'a')).isEmpty)

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("ab"))
      assert(refineByIndexOf(r, 'a', (GE, 1)).isEmpty)
      assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("b"))
      assert(refineByIndexOf(r, 'b', (NE, 0)) == re("ab"))

  describe("a?b?"):
    val r = re("a?b?")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == re("ab?"))
      assert(refineByCharAt(r, 0, (EQ, 'b')) == re("b"))
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab"))
      assert(refineByCharAt(r, 1, (NE, 'b')).isEmpty)

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("ab?"))
      assert(refineByIndexOf(r, 'a', (GT, 0)).isEmpty)
      assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("b"))
      assert(refineByIndexOf(r, 'b', (EQ, 1)) == re("ab"))
      assert(refineByIndexOf(r, 'b', (LE, 1)) == re("a?b"))

  describe("(a|b)+"):
    val r = re("(a|b)+")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == re("a(a|b)*"))
      assert(refineByCharAt(r, 0, (EQ, 'b')) == re("b(a|b)*"))
      assert(refineByCharAt(r, 1, (EQ, 'a')) == re("(a|b)a(a|b)*"))
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("(a|b)b(a|b)*"))

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a(a|b)*"))
      assert(refineByIndexOf(r, 'a', (EQ, 1)) == re("ba(a|b)*"))
      assert(refineByIndexOf(r, 'a', (GE, 2)) == re("bbb*a(a|b)*"))
      assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("b(a|b)*"))
      assert(refineByIndexOf(r, 'b', (EQ, 1)) == re("ab(a|b)*"))
      assert(refineByIndexOf(r, 'b', (GE, 2)) == re("aaa*b(a|b)*"))

  describe("(a|ab)*"):
    val r = re("(a|ab)*")

    it("refine by char-at"):
      assert(refineByCharAt(r, 0, (EQ, 'a')) == re("a(|b)(a|ab)*"))
      assert(refineByCharAt(r, 1, (EQ, 'a')) == re("aa(|b)(a|ab)*"))
      assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab(a|ab)*"))

    it("refine by index-of"):
      assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a(|b)(a|ab)*"))
      assert(refineByIndexOf(r, 'a', (GE, 1)).isEmpty)
      assert(refineByIndexOf(r, 'b', (EQ, 1)) == re("ab(a|ab)*"))
      assert(refineByIndexOf(r, 'b', (NE, 1)) == re("aa*ab(a|ab)*"))
      assert(refineByIndexOf(r, 'b', (GT, 2)) == re("aaa*ab(a|ab)*"))
      assert(refineByIndexOf(r, 'b', (LT, 4)) == re("(|a|aa)ab(a|ab)*"))
