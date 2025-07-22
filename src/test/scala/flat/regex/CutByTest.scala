package flat.regex

import org.scalatest.funsuite.AnyFunSuite

class CutByTest extends RegexTest:

  import REOps.cutBy
  import RegExpr.*

  test("cut abc"):
    val r = re("abc")
    assert(cutBy(r, 'a') == (List((RENull, re("bc"))), RENone))
    assert(cutBy(r, 'b') == (List((re("a"), re("c"))), RENone))
    assert(cutBy(r, 'c') == (List((re("ab"), RENull)), RENone))
    assert(cutBy(r, 'd') == (Nil, r))

  test("cut ab*"):
    val r = re("ab*")
    assert(cutBy(r, 'a') == (List((RENull, re("b*"))), RENone))
    assert(cutBy(r, 'b') == (List((re("a"), re("b*"))), re("a")))

  test("cut a?b"):
    val r = re("a?b")
    assert(cutBy(r, 'a') == (List((RENull, re("b"))), re("b")))
    assert(cutBy(r, 'b') == (List((re("a?"), RENull)), RENone))

  test("cut a?b?"):
    val r = re("a?b?")
    assert(cutBy(r, 'a') == (List((RENull, re("b?"))), re("b?")))
    assert(cutBy(r, 'b') == (List((re("a?"), RENull)), re("a?")))

  test("cut a|b*"):
    val r = re("a|b*")
    assert(cutBy(r, 'a') == (List((RENull, RENull)), re("b*")))
    assert(cutBy(r, 'b') == (List((RENull, re("b*"))), re("a|")))

  test("cut a|bc"):
    val r = re("a|bc")
    assert(cutBy(r, 'a') == (List((RENull, RENull)), re("bc")))
    assert(cutBy(r, 'b') == (List((RENull, re("c"))), re("a")))
    assert(cutBy(r, 'c') == (List((re("b"), RENull)), re("a")))

  test("cut (a|ab)*"):
    val r = re("(a|ab)*")
    assert(cutBy(r, 'a') == (List((RENull, re("(a|ab)*")), (RENull, re("b(a|ab)*"))), RENull))
    assert(cutBy(r, 'b') == (List((re("a*a"), re("(a|ab)*"))), re("a*")))

  test("cut (a|b)+"):
    val r = re("(a|b)+")
    assert(cutBy(r, 'a') == (List((RENull, re("(a|b)*")), (re("b+"), re("(a|b)*"))), re("b+")))

  test("cut (a|b){0,2}"):
    val r = re("(a|b){0,2}")
    assert(cutBy(r, 'a') == (List((RENull, re("(a|b)?")), (re("b"), RENull)), re("b{0,2}")))

  test("cut (a|b){1,4}"):
    val r = re("(a|b){1,4}")
    assert(cutBy(r, 'a') == (List(
      (RENull, re("(a|b){0,3}")), (re("b"), re("(a|b){0,2}")), (re("b{2}"), re("(a|b)?")), (re("b{3}"), RENull)
    ), re("b{1,4}")))
