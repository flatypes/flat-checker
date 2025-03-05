package flat.checker.abs

import flat.checker.abs.ABool.*
import flat.checker.abs.ReLang.*
import flat.checker.abs.RegexImplicits.*
import org.scalatest.funsuite.AnyFunSuite

class ElementTest extends AnyFunSuite:
  test("contain character 1"):
    val r = mkConcat('a', mkConcat('b', 'c').*, 'b')
    assert(r.contains('a') == True)
    assert(r.contains('b') == True)
    assert(r.contains('c') == Top)

  test("contain character 2"):
    val r = mkConcat(mkUnion('+', '-'), CharSet.NUM.+)
    assert(r.contains('+') == Top)
    assert(r.contains('-') == Top)
    assert(r.contains('0') == Top)
    assert(r.neverContain('*'))

  test("only contain characters"):
    val r = mkConcat('-'.?, CharSet.from('1' to '9').+)
    assert(r.alphabet.subsetOf(CharSet.NUM | CharSet.of('-')))
    assert(!r.alphabet.subsetOf(CharSet.NUM))

  test("must start with union"):
    val r = mkConcat('a', mkUnion('b', "bc"))
    assert(ReLangOps.startsWith(r, "a") == True)
    assert(ReLangOps.startsWith(r, "ab") == True)

  test("must start with star"):
    val r = mkConcat('a', mkConcat('b', 'c').*, 'b')
    assert(ReLangOps.startsWith(r, "a") == True)
    assert(ReLangOps.startsWith(r, "ab") == True)

  test("may start with"):
    val r = mkConcat(mkUnion('+', '-'), CharSet.NUM.+)
    assert(ReLangOps.startsWith(r, "+") == Top)
    assert(ReLangOps.startsWith(r, "-") == Top)
    assert(ReLangOps.startsWith(r, "-1") == Top)
    assert(ReLangOps.startsWith(r, "+12") == Top)

  test("never start with"):
    val r = mkConcat(mkUnion('+', '-'), CharSet.NUM.+)
    assert(ReLangOps.startsWith(r, "0") == False)
    assert(ReLangOps.startsWith(r, "-+") == False)

