package flat.checker.domain

import flat.checker.domain.REOps.{*, given}
import org.scalatest.funspec.AnyFunSpec

class FindTest extends AnyFunSpec, REAssertions:
  describe("ab+c"):
    val r = re("ab+c")

    it("indexOf a"):
      val (mustContain, r1) = r.absIndexOf("a")
      assert(mustContain)
      assert(r1.isFinite)
      assertResult(Set(0))(r1.toFinSet())

    it("indexOf b"):
      val (mustContain, r1) = r.absIndexOf("b")
      assert(mustContain)
      assert(r1.isFinite)
      assertResult(Set(1))(r1.toFinSet())

    it("indexOf c"):
      val (mustContain, r1) = r.absIndexOf("c")
      assert(mustContain)
      assert(!r1.isFinite)
      assertResult(2)(r1.toFinSet().min)

    it("may contain bb"):
      assertResult(BoolSet.Full)(r.absContains("bb"))

    it("never contain ac"):
      assertResult(BoolSet.False)(r.absContains("ac"))

  describe("ab*c"):
    val r = re("ab*c")

    it("indexOf a"):
      val (mustContain, r1) = r.absIndexOf("a")
      assert(mustContain)
      assert(r1.isFinite)
      assertResult(Set(0))(r1.toFinSet())

    it("indexOf b"):
      val (mustContain, r1) = r.absIndexOf("b")
      assert(!mustContain)
      assert(r1.isFinite)
      assertResult(Set(1))(r1.toFinSet())

    it("indexOf c"):
      val (mustContain, r1) = r.absIndexOf("c")
      assert(mustContain)
      assert(!r1.isFinite)
      assertResult(1)(r1.toFinSet().min)

    it("may contain ac"):
      assertResult(BoolSet.Full)(r.absContains("ac"))

  describe("(a|b)*"):
    val r = re("(a|b)*")

    it("filter contains ab"):
      assertCharRE("(a|b)*ab(a|b)*")(r.filterContains("ab"))

    it("filter not contain ab"):
      assertCharRE("b*a*")(r.filterNotContain("ab"))

  describe("a(,a)+"):
    val r = re("a(,a)+")

    it("must contain ,a"):
      assertResult(BoolSet.True)(r.absContains(",a"))

    it("must contain a,a"):
      assertResult(BoolSet.True)(r.absContains("a,a"))

    it("count a"):
      val r1 = r.absCount("a")
      assert(!r1.isFinite)
      assertResult(Set(2))(r1.toFinSet())

    describe("split ,"):
      val r1 = r.absSplit(",")

      it("length"):
        val r2 = r1.absLength
        assert(!r2.isFinite)
        assertResult(Set(2))(r2.toFinSet())

      it("any element is a"):
        assertCharRE("a")(r1.alphabet)

  describe("a(b,c)*d,e"):
    val r = re("a(b,c)*d,e")

    it("count d"):
      val r1 = r.absCount("d")
      assert(r1.isFinite)
      assertResult(Set(1))(r1.toFinSet())

    it("count cb"):
      val r1 = r.absCount("cb")
      assert(!r1.isFinite)
      assertResult(0)(r1.toFinSet().min)

    describe("split ,"):
      val r1 = r.absSplit(",")

      it("length"):
        val r2 = r1.absLength
        assert(!r2.isFinite)
        assertResult(2)(r2.toFinSet().min)

      it("first element is ab|ad"):
        assertCharRE("ab|ad")(r1.absAt(0))

      it("second element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absAt(1))

      it("third element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absAt(3))

      it("last element is e"):
        assertCharRE("e")(r1.absAtRight(0))
