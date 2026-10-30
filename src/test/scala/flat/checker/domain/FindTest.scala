package flat.checker.domain

import flat.checker.domain.REOps.*
import org.scalatest.funspec.AnyFunSpec

class FindTest extends AnyFunSpec, REAssertions:
  describe("ab+c"):
    val r = re("ab+c")

    it("indexOf a"):
      val indexSet = r.absFind("a")
      assert(!indexSet.negative)
      assertResult(NatSet(0))(indexSet.left)

    it("indexOf b"):
      val indexSet = r.absFind("b")
      assert(!indexSet.negative)
      assertResult(NatSet(1))(indexSet.left)

    it("indexOf c"):
      val indexSet = r.absFind("c")
      assert(!indexSet.negative)
      assertResult(NatSet(Set(2), true))(indexSet.left)

    it("may contain bb"):
      assertResult(BoolSet.Full)(r.absContains("bb"))

    it("never contain ac"):
      assertResult(BoolSet.False)(r.absContains("ac"))

  describe("ab*c"):
    val r = re("ab*c")

    it("indexOf a"):
      val indexSet = r.absFind("a")
      assert(!indexSet.negative)
      assertResult(NatSet(0))(indexSet.left)

    it("indexOf b"):
      val indexSet = r.absFind("b")
      assert(indexSet.negative)
      assertResult(NatSet(1))(indexSet.left)

    it("indexOf c"):
      val indexSet = r.absFind("c")
      assert(!indexSet.negative)
      assertResult(NatSet(Set(1), true))(indexSet.left)

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
      val set = r.absCount("a")
      assertResult(NatSet(Set(2), true))(set)

    describe("split ,"):
      val r1 = r.absSplit(",")

      it("length"):
        val set = r1.absLength
        assertResult(NatSet(Set(2), true))(set)

      it("any element is a"):
        assertCharRE("a")(r1.alphabet)

  describe("a(b,c)*d,e"):
    val r = re("a(b,c)*d,e")

    it("count d"):
      val r1 = r.absCount("d")
      assertResult(NatSet(1))(r1)

    it("count cb"):
      val set = r.absCount("cb")
      assertResult(NatSet(Set(0), true))(set)

    describe("split ,"):
      val r1 = r.absSplit(",")

      it("length"):
        val set = r1.absLength
        assertResult(NatSet(Set(2), true))(set)

      it("first element is ab|ad"):
        assertCharRE("ab|ad")(r1.absElemAt(0))

      it("second element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absElemAt(1))

      it("third element is e|cd|cb"):
        assertCharRE("e|cd|cb")(r1.absElemAt(3))

      it("last element is e"):
        assertCharRE("e")(r1.absElemAt(IRight(1)))

  describe("//[a-z]+(/[a-z]*)*"):
    val r = re("//[a-z]+(/[a-z]*)*")

    it("find '/' from 2"):
      val indexSet = r.absFind("/", ILeft(2))
      assert(indexSet.negative)
      assertResult(NatSet(Set(3), true))(indexSet.left)

    it("filter s.find('/', 2) >= 0"):
      val r1 = r.filterFindCmp("/", ILeft(2), CmpOp.GE, 0)
      assertCharRE("//[a-z]+(/[a-z]*)+")(r1)

    it("filter '/' in s[2:]"):
      val r1 = r.absDrop(2).filterContains("/")
      assertCharRE("(/[a-z]*)+")(r1)
