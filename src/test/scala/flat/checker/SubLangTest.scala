package flat.checker

import org.scalatest.funsuite.AnyFunSuite

class SubLangTest extends AnyFunSuite:
  private def positive(left: String, right: String): Unit =
    val r1 = ReLang.fromPython(left)
    val r2 = ReLang.fromPython(right)
    assert(ReLangSub.check(r1, r2))

  private def negative(left: String, right: String): Unit =
    val r1 = ReLang.fromPython(left)
    val r2 = ReLang.fromPython(right)
    assert(!ReLangSub.check(r1, r2))

  test("equivalent cases"):
    positive("", "")
    positive("[0-9]", "[0-9]")
    positive("..*", ".+")
    positive("a|b", "b|a")
    positive("ab?", "a|ab")
    positive("a(bc|bd)", "ab(c|d)")
    positive("ab{2,4}", "abb|abbb|abbbb")
    positive("(ab){3}", "a(ba){2}b")
    positive("(ab){2,}", "a(ba)+b")

  test("positive cases"):
    positive("[a-f]", "[a-z]")
    positive("a+", "a*")
    positive("a?", "a*")
    positive("[ab]a*", "[ab]a*|[bc]c*")
    positive("ab{2,4}", "ab{2,5}")
    positive("ab{2,4}", "ab{1,4}")
    positive("(ab){2,}", "a(ba)*b")

  test("negative cases"):
    negative("(ab)*", "a(ba)*b")
    negative("[a-f]", "[b-g]")
    negative("ab{2,4}", "ab{2,3}")
    negative("ab{2,4}", "ab{1,3}")
    negative("ab*", "ac*")
    negative("ab+c", "ab+")
    negative("ab+", "ab+c")
