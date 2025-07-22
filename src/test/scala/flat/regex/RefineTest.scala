package flat.regex

import org.scalatest.funsuite.AnyFunSuite

class RefineTest extends RegexTest:

  import RERefiner.*
  import RegExpr.*
  import flat.Ops.CmpOp.*

  test("refine ab*"):
    val r = re("ab*")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == r)
    assert(refineByCharAt(r, 1, (EQ, 'b')) == re("abb*"))

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == r)
    assert(refineByIndexOf(r, 'b', (EQ, 0)) == RENone)
    assert(refineByIndexOf(r, 'b', (GE, 1)) == re("abb*"))
    assert(refineByNotFound(r, 'a') == RENone)
    assert(refineByNotFound(r, 'b') == re("a"))

  test("refine a?b"):
    val r = re("a?b")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == re("ab"))
    assert(refineByCharAt(r, 0, (EQ, 'b')) == re("b"))
    assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab"))
    assert(refineByCharAt(r, 1, (EQ, 'a')) == RENone)

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("ab"))
    assert(refineByIndexOf(r, 'a', (GE, 1)) == RENone)
    assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("b"))
    assert(refineByIndexOf(r, 'b', (NE, 0)) == re("ab"))
    assert(refineByNotFound(r, 'a') == re("b"))
    assert(refineByNotFound(r, 'b') == RENone)

  test("refine a?b?"):
    val r = re("a?b?")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == re("ab?"))
    assert(refineByCharAt(r, 0, (EQ, 'b')) == re("b"))
    assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab"))
    assert(refineByCharAt(r, 1, (NE, 'b')) == RENone)

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("ab?"))
    assert(refineByIndexOf(r, 'a', (GT, 0)) == RENone)
    assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("b"))
    assert(refineByIndexOf(r, 'b', (EQ, 1)) == re("ab"))
    assert(refineByIndexOf(r, 'b', (LE, 1)) == re("a?b"))
    assert(refineByNotFound(r, 'a') == re("b?"))
    assert(refineByNotFound(r, 'b') == re("a?"))

  test("refine a|b*"):
    val r = re("a|b*")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == re("a"))
    assert(refineByCharAt(r, 0, (EQ, 'b')) == re("bb*"))
    assert(refineByCharAt(r, 1, (EQ, 'b')) == re("bbb*"))
    assert(refineByCharAt(r, 1, (NE, 'b')) == RENone)

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a"))
    assert(refineByIndexOf(r, 'a', (GT, 0)) == RENone)
    assert(refineByIndexOf(r, 'b', (EQ, 0)) == re("bb*"))
    assert(refineByIndexOf(r, 'b', (GT, 0)) == RENone)
    assert(refineByNotFound(r, 'a') == re("b*"))
    assert(refineByNotFound(r, 'b') == re("a|"))

  test("refine a|bc"):
    val r = re("a|bc")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == re("a"))
    assert(refineByCharAt(r, 0, (NE, 'a')) == re("bc"))
    assert(refineByCharAt(r, 1, (EQ, 'c')) == re("bc"))
    assert(refineByCharAt(r, 1, (NE, 'c')) == RENone)

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a"))
    assert(refineByIndexOf(r, 'a', (GE, 1)) == RENone)
    assert(refineByIndexOf(r, 'c', (EQ, 1)) == re("bc"))
    assert(refineByIndexOf(r, 'c', (GE, 1)) == re("bc"))
    assert(refineByIndexOf(r, 'c', (LE, 1)) == re("bc"))
    assert(refineByNotFound(r, 'a') == re("bc"))
    assert(refineByNotFound(r, 'b') == re("a"))

  test("refine (a|ab)*"):
    val r = re("(a|ab)*")
    assert(refineByCharAt(r, 0, (EQ, 'a')) == re("(a|ab)(a|ab)*"))
    assert(refineByCharAt(r, 1, (EQ, 'a')) == re("a(a|ab)(a|ab)*"))
    assert(refineByCharAt(r, 1, (EQ, 'b')) == re("ab(a|ab)*"))

    assert(refineByIndexOf(r, 'a', (EQ, 0)) == re("a(|b)(a|ab)*"))
    assert(refineByIndexOf(r, 'a', (GE, 1)) == RENone)
    assert(refineByIndexOf(r, 'b', (EQ, 1)) == re("ab(a|ab)*"))
    assert(refineByIndexOf(r, 'b', (NE, 1)) == re("a+ab(a|ab)*"))
    assert(refineByIndexOf(r, 'b', (GT, 2)) == re("a{2,}ab(a|ab)*"))
    assert(refineByIndexOf(r, 'b', (LT, 4)) == re("a{,2}ab(a|ab)*"))
    assert(refineByNotFound(r, 'a') == re(""))
    assert(refineByNotFound(r, 'b') == re("a*"))