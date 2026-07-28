package flat.checker.domain

import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import org.scalatest.funspec.AnyFunSpec

class FindTest extends AnyFunSpec, REAssertions:
  describe("ab+c"):
    val r = re("ab+c")

    it("indexOf a"):
      val indexSet = r.absIndexOfStr("a")
      assert(indexSet.neg.isEmpty)
      assert(indexSet.pos.isFinite)
      assertResult(Set(0))(indexSet.pos.toFinSet)

    it("indexOf b"):
      val indexSet = r.absIndexOfStr("b")
      assert(indexSet.neg.isEmpty)
      assert(indexSet.pos.isFinite)
      assertResult(Set(1))(indexSet.pos.toFinSet)

    it("indexOf c"):
      val indexSet = r.absIndexOfStr("c")
      assert(indexSet.neg.isEmpty)
      assert(!indexSet.pos.isFinite)
      assertResult(2)(indexSet.pos.min)

    it("may contain bb"):
      assertResult(BoolSet.Full)(r.absContainsStr("bb"))

    it("never contain ac"):
      assertResult(BoolSet.False)(r.absContainsStr("ac"))

  describe("ab*c"):
    val r = re("ab*c")

    it("indexOf a"):
      val indexSet = r.absIndexOfStr("a")
      assert(indexSet.neg.isEmpty)
      assert(indexSet.pos.isFinite)
      assertResult(Set(0))(indexSet.pos.toFinSet)

    it("indexOf b"):
      val indexSet = r.absIndexOfStr("b")
      assert(indexSet.neg.nonEmpty)
      assert(indexSet.pos.isFinite)
      assertResult(Set(1))(indexSet.pos.toFinSet)

    it("indexOf c"):
      val indexSet = r.absIndexOfStr("c")
      assert(indexSet.neg.isEmpty)
      assert(!indexSet.pos.isFinite)
      assertResult(1)(indexSet.pos.min)

    it("may contain ac"):
      assertResult(BoolSet.Full)(r.absContainsStr("ac"))

  describe("(a|b)*"):
    val r = re("(a|b)*")

    it("filter contains ab"):
      assertCharRE("(a|b)*ab(a|b)*")(r.filterContainsStr("ab"))

    it("filter not contain ab"):
      assertCharRE("b*a*")(r.filterNotContainStr("ab"))

  describe("a(,a)+"):
    val r = re("a(,a)+")

    it("must contain ,a"):
      assertResult(BoolSet.True)(r.absContainsStr(",a"))

    it("must contain a,a"):
      assertResult(BoolSet.True)(r.absContainsStr("a,a"))

    it("count a"):
      val r1 = r.absCountStr("a")
      assert(!r1.isFinite)
      assertResult(2)(r1.min)

    describe("split ,"):
      val r1 = r.absSplitStr(",")

      it("length"):
        val r2 = r1.absLength
        assert(!r2.isFinite)
        assertResult(2)(r2.min)

      it("any element is a"):
        assertCharRE("a")(r1.alphabet)

  describe("a(b,c)*d,e"):
    val r = re("a(b,c)*d,e")

    it("count d"):
      val r1 = r.absCountStr("d")
      assert(r1.isFinite)
      assertResult(Set(1))(r1.toFinSet)

    it("count cb"):
      val r1 = r.absCountStr("cb")
      assert(!r1.isFinite)
      assertResult(0)(r1.min)

    describe("split ,"):
      val r1 = r.absSplitStr(",")

      it("length"):
        val r2 = r1.absLength
        assert(!r2.isFinite)
        assertResult(2)(r2.min)

      it("first element is ab|ad"):
        assertCharRE("ab|ad")(r1.absAt(0))

      it("second element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absAt(1))

      it("third element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absAt(3))

      it("last element is e"):
        assertCharRE("e")(r1.absAtRight(0))
