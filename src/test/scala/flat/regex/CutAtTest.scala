//package flat.regex
//
//import org.scalatest.funsuite.AnyFunSuite
//
//class CutAtTest extends RegexTest:
//
//  import AOps.*
//  import RegEx.RENull
//
//  test("cut abc"):
//    val r = re("abc")
//
//    assert(take(r, 0) == RENull)
//    assert(take(r, 1) == re("a"))
//    assert(take(r, 2) == re("ab"))
//    assert(take(r, 3) == re("abc"))
//    assert(take(r, 4) == re("abc"))
//
//    assert(drop(r, 0) == r)
//    assert(drop(r, 1) == re("bc"))
//    assert(drop(r, 2) == re("c"))
//    assert(drop(r, 3) == RENull)
//    assert(drop(r, 4) == RENull)
//
//    assert(takeRight(r, 0) == RENull)
//    assert(takeRight(r, 1) == re("c"))
//    assert(takeRight(r, 2) == re("bc"))
//    assert(takeRight(r, 3) == re("abc"))
//    assert(takeRight(r, 4) == re("abc"))
//
//    assert(dropRight(r, 0) == r)
//    assert(dropRight(r, 1) == re("ab"))
//    assert(dropRight(r, 2) == re("a"))
//    assert(dropRight(r, 3) == RENull)
//    assert(dropRight(r, 4) == RENull)
//
//  test("cut ab*"):
//    val r = re("ab*")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), re("b*"))))
//    assert(cutAt(r, 1) == List((re("a"), ch('b'), re("b*"))))
//    assert(cutAt(r, 2) == List((re("ab"), ch('b'), re("b*"))))
//    assert(cutAt(r, 3) == List((re("abb"), ch('b'), re("b*"))))
//    assert(cutAt(r, 4) == List((re("abbb"), ch('b'), re("b*"))))
//    assert(cutAt(r, 5) == List((re("abbbb"), ch('b'), re("b*"))))
//
//  test("cut a?b"):
//    val r = re("a?b")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), re("b")), (RENull, ch('b'), RENull)))
//    assert(cutAt(r, 1) == List((re("a"), ch('b'), RENull)))
//    assert(cutAt(r, 2) == Nil)
//
//  test("cut a?b?"):
//    val r = re("a?b?")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), re("b?")), (RENull, ch('b'), RENull)))
//    assert(cutAt(r, 1) == List((re("a"), ch('b'), RENull)))
//    assert(cutAt(r, 2) == Nil)
//
//  test("cut a|b*"):
//    val r = re("a|b*")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), RENull), (RENull, ch('b'), re("b*"))))
//    assert(cutAt(r, 1) == List((re("b"), ch('b'), re("b*"))))
//
//  test("cut a|bc"):
//    val r = re("a|bc")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), RENull), (RENull, ch('b'), re("c"))))
//    assert(cutAt(r, 1) == List((re("b"), ch('c'), RENull)))
//    assert(cutAt(r, 2) == Nil)
//
//  test("cut (a|ab)*"):
//    val r = re("(a|ab)*")
//    assert(cutAt(r, 0) == List((RENull, ch('a'), re("(a|ab)*")), (RENull, ch('a'), re("b(a|ab)*"))))
//    assert(cutAt(r, 1) == List(
//      (re("a"), ch('a'), re("(a|ab)*")), (re("a"), ch('a'), re("b(a|ab)*")),
//      (re("a"), ch('b'), re("(a|ab)*"))))
//    assert(cutAt(r, 2) == List(
//      (re("aa"), ch('a'), re("(a|ab)*")), (re("aa"), ch('a'), re("b(a|ab)*")),
//      (re("aa"), ch('b'), re("(a|ab)*")),
//      (re("ab"), ch('a'), re("(a|ab)*")), (re("ab"), ch('a'), re("b(a|ab)*"))))
//
//    assert(drop(r, 1) == re("(|b)(a|ab)*"))
//    assert(drop(r, 2) == re("(|b)(a|ab)*"))
//    assert(drop(r, 3) == re("(|b)(a|ab)*"))
//    assert(drop(r, 4) == re("(|b)(a|ab)*"))
