package flat.regex

import flat.regex.REOps.*
import org.scalatest.funspec.AnyFunSpec

class InfixTest extends AnyFunSpec, REAssertions:
  describe("abc"):
    val r = re("abc")

    it("filter not contain ab"):
      assert(r.filterNotContain("ab").isEmpty)

    it("filter not contain bc"):
      assert(r.filterNotContain("bc").isEmpty)

    it("filter not contain ac"):
      assertEqual(r.filterNotContain("ac"), r)

  describe("a(b|c)d"):
    val r = re("a(b|c)d")

    it("filter contain ab"):
      assertEqual(r.filterContain("ab"), re("abd"))

    it("filter not contain ab"):
      assertEqual(r.filterNotContain("ab"), re("acd"))

  describe("(a|b)*"):
    val r = re("(a|b)*")

    it("filter contain ab"):
      assertEqual(r.filterContain("ab"), re("(a|b)*ab(a|b)*"))

    it("filter not contain ab"):
      assertEquiv(r.filterNotContain("ab"), re("b*a*"))

  describe(".*"):
    val r = re(".*")

    it("filter contain ab"):
      assertEqual(r.filterContain("ab"), re(".*ab.*"))

    it("filter not contain ab"):
      assertEquiv(r.filterNotContain("ab"), re("([^a]|a+[^ab])*a*"))
