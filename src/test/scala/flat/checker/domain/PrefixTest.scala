package flat.checker.domain

import flat.checker.domain.REOps.{*, given}
import org.scalatest.funspec.AnyFunSpec

class PrefixTest extends AnyFunSpec, REAssertions:
  describe("ab+c"):
    val r = re("ab+c")

    it("must start with ab"):
      assertResult(BoolSet.True)(r.absStartsWith("ab"))

    it("must end with bc"):
      assertResult(BoolSet.True)(r.absEndsWith("bc"))

    it("may start with abb"):
      assertResult(BoolSet.Full)(r.absStartsWith("abb"))

    it("may end with bbc"):
      assertResult(BoolSet.Full)(r.absEndsWith("bbc"))

    it("may start with abc"):
      assertResult(BoolSet.Full)(r.absStartsWith("abc"))

    it("may end with abc"):
      assertResult(BoolSet.Full)(r.absEndsWith("abc"))

  describe("ab*c"):
    val r = re("ab*c")

    it("may start with ab"):
      assertResult(BoolSet.Full)(r.absStartsWith("ab"))

    it("may end with bc"):
      assertResult(BoolSet.Full)(r.absEndsWith("bc"))

    it("may start with ac"):
      assertResult(BoolSet.Full)(r.absStartsWith("ac"))

    it("may end with ac"):
      assertResult(BoolSet.Full)(r.absEndsWith("ac"))
