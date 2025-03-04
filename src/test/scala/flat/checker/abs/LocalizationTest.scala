package flat.checker.abs

import flat.checker.Bound.*
import flat.checker.abs.ReLang.*
import flat.checker.abs.RegexImplicits.*
import flat.checker.abs.analysis.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class LocalizationTest extends AnyFunSuite {
  // [0-9] "." [0-9]{2}
  private val version1 = mkConcat(CharSet.NUM, '.', CharSet.NUM ^ 2)

  test("char at a relative position") {
    // dot = s.find('.')
    val dot = find(version1.toCNF, '.', 0).toOption.get
    // s[dot]
    val cs = charAt(version1, dot).toOption.get
    assert(cs.isSingleton && cs.contains('.'))
  }

  test("char at an absolute position") {
    // s[2]
    val cs = charAt(version1, AbsPos(2)).toOption.get
    assert(cs == CharSet.NUM)
  }

  test("char at out-of-bound index") {
    // s[4]
    val result = charAt(version1, AbsPos(4))
    assert(result.isLeft)
  }

  test("slice constant ranges") {
    // s[0:1]
    val r1 = slice(version1.toCNF, AbsPos(0), AbsPos(1)).toOption.get
    assert(canCastToNat(r1))
    // s[2:4]
    val r2 = slice(version1.toCNF, AbsPos(2), AbsPos(4)).toOption.get
    assert(canCastToNat(r2))
  }

  test("slice from a constant index") {
    // s[2:]
    val r = slice(version1.toCNF, AbsPos(2)).toOption.get
    assert(canCastToNat(r))
  }

  // [0-9]+ "." [0-9]+ "." [0-9]+
  private val version2 = mkConcat(CharSet.NUM.+, '.', CharSet.NUM.+, '.', CharSet.NUM.+)

  test("failed to slice from a constant index") {
    // s[2:]
    val result = slice(version2.toCNF, AbsPos(2))
    assert(result.isLeft)
  }

  test("slice a range of relative positions") {
    val versionCNF = version2.toCNF
    // firstDot = s.find('.')
    val firstDot = find(versionCNF, '.', 0).toOption.get
    // afterFirstDot = firstDot + 1
    val afterFirstDot = rightShift(firstDot, 1).toOption.get
    // secondDot = s.find('.', afterFirstDot)
    val secondDot = find(versionCNF, '.', afterFirstDot).toOption.get
    // s[afterFirstDot:secondDot]
    val r = slice(versionCNF, afterFirstDot, secondDot).toOption.get
    assert(canCastToNat(r))
  }

  test("split finite") {
    val sr1 = split(version1, '.').toOption.get
    assert(sr1.lengthRange == Range.fromInt(2))
    assert(sr1.get(0).get == digit)
    assert(sr1.get(1).get == (digit ^ 2))

    val sr2 = split(version2, '.').toOption.get
    assert(sr2.lengthRange == Range.fromInt(3))
    assert(sr2.each == number)
  }

  test("split infinite") {
    // number (',' number)*
    val r1 = mkConcat(number, ReConcat(',', number).*)
    val sr1 = split(r1, ',').toOption.get
    assert(sr1.lengthRange.ub == PosInf)
    assert(sr1.each == number)

    // (number ',')* number
    val r2 = mkConcat(ReConcat(number, ',').*, number)
    val sr2 = split(r2, ',').toOption.get
    assert(sr2.lengthRange.ub == PosInf)
    assert(sr2.each == number)

    // number ',' number (',' number)* ',' (number ',')* number
    val r3 = mkConcat(number, ',', number, ReConcat(',', number).*, ',',
      ReConcat(number, ',').*, number)
    val sr3 = split(r3, ',').toOption.get
    assert(sr3.lengthRange == Range(3, PosInf))
    assert(sr3.each == number)

    assert(sr3.get(0).get == number)
    assert(sr3.get(1).get == number)
    assert(sr3.get(2).isEmpty)
    assert(sr3.get(-1).get == number)
  }
}
