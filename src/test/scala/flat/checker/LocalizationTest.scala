package flat.checker

import flat.checker
import flat.checker.Bound.*
import flat.checker.ReLang.*
import flat.checker.RegexImplicits.*
import flat.checker.backend.Index
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class LocalizationTest extends AnyFunSuite:
  // [0-9] "." [0-9]{2}
  private val version1 = mkConcat(CharSet.NUM, '.', CharSet.NUM ^ 2)

  test("char at a relative position"):
    val cnf = version1.toCNF
    // dot = s.find('.')
    val dot = CNFOps.indexOf(cnf, '.').toOption.get
    // s[dot]
    val cs = CNFOps.charAt(cnf, dot)
    assert(cs.isSingleton && cs.contains('.'))

  test("char at an absolute position"):
    // s[2]
    val cs = ReLangOps.charAt(version1, 2).get
    assert(cs == CharSet.NUM)

  test("char at range"):
    // s[1:4]
    val cs = ReLangOps.charAt(version1, 1, 4)
    assert(cs == CharSet.from('.', '0' to '9'))

  test("char at out-of-bound index"):
    // s[4]
    val result = ReLangOps.charAt(version1, 4)
    assert(result.isEmpty)

  test("char at manyAB"):
    val r = ReLang.fromPython("a*b*")
    val cs = CharSet.of('a', 'b')
    assert(ReLangOps.charAt(r, 10).get == cs)
    assert(ReLangOps.charAt(r, 0, 2) == cs)
    assert(ReLangOps.charAt(r, 0, -1) == cs)
    assert(ReLangOps.charAt(r, 2, -2) == cs)

  test("slice constant ranges"):
    val cnf = version1.toCNF
    // s[0:1]
    val r1 = CNFOps.substring(cnf,
      CNFOps.convertToRelative(cnf, 0).toOption.get,
      CNFOps.convertToRelative(cnf, 1).toOption.get)
    assert(r1.isNumber)
    // s[2:4]
    val r2 = CNFOps.substring(cnf,
      CNFOps.convertToRelative(cnf, 2).toOption.get,
      CNFOps.convertToRelative(cnf, 4).toOption.get)
    assert(r2.isNumber)

  test("slice from a constant index"):
    val cnf = version1.toCNF
    // s[2:]
    val r = CNFOps.substring(cnf, CNFOps.convertToRelative(cnf, 2).toOption.get)
    assert(r.isNumber)

  // [0-9]+ "." [0-9]+ "." [0-9]+
  private val version2 = mkConcat(CharSet.NUM.+, '.', CharSet.NUM.+, '.', CharSet.NUM.+)

  test("failed to convert a constant index to relative position"):
    // s[2:]
    val result = CNFOps.convertToRelative(version2.toCNF, 2)
    assert(result.isLeft)

  test("slice a range of relative positions"):
    val cnf = version2.toCNF
    // firstDot = s.find('.')
    val firstDot = CNFOps.indexOf(cnf, '.').toOption.get
    // afterFirstDot = firstDot + 1
    val afterFirstDot = CNFOps.shiftIndex(Index(cnf, firstDot), 1).toOption.get.pos
    // secondDot = s.find('.', afterFirstDot)
    val secondDot = CNFOps.indexOf(cnf, '.', afterFirstDot).toOption.get
    // s[afterFirstDot:secondDot]
    val r = CNFOps.substring(cnf, afterFirstDot, secondDot)
    assert(r.isNumber)


  test("split finite"):
    val sr1 = CNFOps.split(version1.toCNF, '.').toOption.get
    assert(sr1.length.isInt && sr1.length.asInt == 2)
    assert(sr1.get(0).get == digit)
    assert(sr1.get(1).get == (digit ^ 2))

    val sr2 = CNFOps.split(version2.toCNF, '.').toOption.get
    assert(sr2.length.isInt && sr2.length.asInt == 3)
    assert(sr2.get(0).get == number)
    assert(sr2.get(1).get == number)
    assert(sr2.get(2).get == number)

  test("split infinite"):
    // number (',' number)*
    val r1 = mkConcat(number, ReConcat(',', number).*)
    val sr1 = CNFOps.split(r1.toCNF, ',').toOption.get
    assert(sr1.length.ub == PosInf)
    assert(sr1.forall(_ == number))

    // (number ',')* number
    val r2 = mkConcat(ReConcat(number, ',').*, number)
    val sr2 = CNFOps.split(r2.toCNF, ',').toOption.get
    assert(sr2.length.ub == PosInf)
    assert(sr2.forall(_ == number))

    // number ',' number (',' number)* ',' (number ',')* number
    val r3 = mkConcat(number, ',', number, ReConcat(',', number).*, ',',
      ReConcat(number, ',').*, number)
    val sr3 = CNFOps.split(r3.toCNF, ',').toOption.get
    assert(sr3.length == checker.Interval(3, PosInf))
    assert(sr3.forall(_ == number))

    assert(sr3.get(0).get == number)
    assert(sr3.get(1).get == number)
    assert(sr3.get(2).isEmpty)
    assert(sr3.get(-1).get == number)

