package flat.regex

import org.scalatest.funspec.AnyFunSpec

class SubLangTest extends AnyFunSpec, REAssertions:
  describe("equiv"):
    it("union symmetric"):
      assertEquiv("a|b", "b|a")
      assertEquiv("a|b|c+", "b|c+|a")
      assertEquiv("[a-z]", "[a-ff-z]")

    it("concat associative"):
      assertEquiv("a(bc)", "(ab)c")
      assertEquiv("(ab){3}", "a(ba){2}b")
      assertEquiv("(ab){2,}", "a(ba)+b")

    it("cpncat-union distributive"):
      assertEquiv("(bc|bd)a", "b(c|d)a")
      assertEquiv("ab?", "a|ab")
      assertEquiv("ab{2,4}", "ab{2}|ab{3}|ab{4}")

  def assertSub(r1: String, r2: String): Unit =
    if !(re(r1) subsetOf re(r2)) then
      fail(s"$r1 ⊈ $r2")

  def assertNotSub(r1: String, r2: String): Unit =
    if re(r1) subsetOf re(r2) then
      fail(s"$r1 ⊆ $r2")

  describe("subset"):
    it("charset subset"):
      assertSub("[a-f]", "[a-z]")
      assertSub("[^()]", "[^(]")

    it("union cases subset"):
      assertSub("a", "a|b")
      assertSub("c|b", "a|b|c")

      assertSub("a{2,4}", "a{2,5}")
      assertSub("a{2,4}", "a{1,4}")
      assertSub("a{2,4}", "a{1,5}")

    it("star"):
      assertSub("", "a*")
      assertSub("a", "a*")
      assertSub("a+", "a*")
      assertSub("a?", "a*")

      assertSub("a{1,5}", "a*")
      assertSub("a{5,}", "a*")
      assertSub("(ab){2,}", "a(ba)*b")

    it("negative"):
      assertNotSub("ab*", "ac*")
      assertNotSub("ab+c", "ab+")
      assertNotSub("ab+", "ab+c")

      assertNotSub("(ab)*", "a(ba)*b")
      assertNotSub("ab{2,4}", "ab{2,3}")
      assertNotSub("ab{2,4}", "ab{1,3}")

