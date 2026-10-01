package flat.checker.domain

import flat.checker.domain.Prettifier.pp
import org.scalatest.funspec.AnyFunSpec

class RESubTest extends AnyFunSpec, REAssertions:
  private def assertSub(regex1: String, regex2: String): Unit =
    val r1 = re(regex1)
    val r2 = re(regex2)
    if !RESub.check(r1, r2) then
      fail(s"${r1.pp} ⊈ ${r2.pp}")

  private def assertNotSub(regex1: String, regex2: String): Unit =
    val r1 = re(regex1)
    val r2 = re(regex2)
    if RESub.check(r1, r2) then
      fail(s"${r1.pp} ⊆ ${r2.pp}")

  private def assertEquiv(regex1: String, regex2: String): Unit =
    val r1 = re(regex1)
    val r2 = re(regex2)
    if !RESub.check(r1, r2) || !RESub.check(r2, r1) then
      fail(s"${r1.pp} != ${r2.pp}")

  describe("abc"):
    it("is equivalent to a(bc)"):
      assertEquiv("abc", "a(bc)")

    it("is not a subset of ab"):
      assertNotSub("abc", "ab")

    it("is not a subset of abcd"):
      assertNotSub("abc", "abcd")

    it("is a subset of (a|b)bc"):
      assertSub("abc", "(a|b)bc")

    it("is a subset of (a|b)(b|c)c"):
      assertSub("abc", "(a|b)(b|c)c")

  describe("a|b|c"):
    it("is equivalent to a|(b|c)"):
      assertEquiv("a|b|c", "a|(b|c)")

    it("is equivalent to c|a|b"):
      assertEquiv("a|b|c", "c|a|b")

    it("is equivalent to [a-c]"):
      assertEquiv("a|b|c", "[a-c]")

    it("is a subset of [a-f]"):
      assertSub("a|b|c", "[a-f]")

    it("is not a subset of [b-f]"):
      assertNotSub("a|b|c", "[b-f]")

  describe("a"):
    it("is subset of a?"):
      assertSub("a", "a?")

    it("is subset of a*"):
      assertSub("a", "a*")

    it("is subset of a+"):
      assertSub("a", "a+")

    it("is not a subset of a+a"):
      assertNotSub("a", "a+a")

  describe("a*a*"):
    it("is equivalent to a*"):
      assertEquiv("a*a*", "a*")

    it("is equivalent to a*a*a*"):
      assertEquiv("a*a*", "a*a*a*")

  describe("a(b|c)?d"):
    it("is equivalent to abd|acd|ad"):
      assertEquiv("a(b|c)?d", "abd|acd|ad")

    it("is equivalent to a(bd|cd|d)"):
      assertEquiv("a(b|c)?d", "a(bd|cd|d)")

    it("is equivalent to a((b|c)d|d)"):
      assertEquiv("a(b|c)?d", "a((b|c)d|d)")

    it("is a subset of a(b|c)*d"):
      assertSub("a(b|c)?d", "a(b|c)*d")

    it("is not a subset of a(b|c)+d"):
      assertNotSub("a(b|c)?d", "a(b|c)+d")

  describe("(ab)*"):
    it("is equivalent to a(ba)*b|ε"):
      assertEquiv("(ab)*", "a(ba)*b|")

    it("is not a subset of a(ba)*b"):
      assertNotSub("(ab)*", "a(ba)*b")

  describe("(abab)+"):
    it("is a subset of (a|b)+"):
      assertSub("(abab)+", "(a|b)+")

    it("is a subset of (ab)+"):
      assertSub("(abab)+", "(ab)+")

    it("is a subset of a(ba)+b"):
      assertSub("(abab)+", "a(ba)+b")

    it("is equivalent to aba(baba)*b"):
      assertEquiv("(abab)+", "aba(baba)*b")

    it("is equivalent to a(baba)*bab"):
      assertEquiv("(abab)+", "a(baba)*bab")