package flat.checker.domain

import flat.checker.domain.Prettifier.pp
import flat.checker.domain.REOps.{*, given}
import org.scalatest.funspec.AnyFunSpec

class FindTest extends AnyFunSpec, REAssertions:
  describe("(a|b)*"):
    val r = re("(a|b)*")

    it("may contain a"):
      assertResult(BoolSet.Full)(r.absContains("a"))

    it("may contain b"):
      assertResult(BoolSet.Full)(r.absContains("b"))

    it("not contain c"):
      assertResult(BoolSet.False)(r.absContains("c"))

    it("filter contains ab"):
      assertCharRE("(a|b)*ab(a|b)*")(r.filterContains("ab"))

    it("filter not contain ab"):
      assertCharRE("b*a*")(r.filterNotContain("ab"))

    it("may contain bba"):
      assertResult(BoolSet.Full)(r.absContains("bba"))

    describe("count a"):
      val r1 = r.absCount("a")

      it("is infinite"):
        assert(!r1.isFinite)

      it("min = 0"):
        assertResult(0)(r1.toSet().min)

    describe("count ab"):
      val r1 = r.absCount("ab")

      it("is infinite"):
        assert(!r1.isFinite)

      it("min = 0"):
        assertResult(0)(r1.toSet().min)

  describe("a(b,b)*"):
    val r = re("a(b,b)*")

    it("must contain a"):
      assertResult(BoolSet.True)(r.absContains("a"))

    it("may contain b"):
      assertResult(BoolSet.Full)(r.absContains("b"))

    it("may contain comma"):
      assertResult(BoolSet.Full)(r.absContains(","))

    it("filter contains comma"):
      info(r.filterContains(",").pp)
