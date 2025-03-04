package flat.checker.abs

import flat.checker.Bound.*
import flat.checker.abs.ReLang.*
import flat.checker.abs.RegexImplicits.*
import flat.checker.abs.analysis.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class LengthTest extends AnyFunSuite {
  test("fixed length") {
    val len = measureLength(mkConcat(mkUnion('+', '-'), CharSet.NUM ^ 4))
    assert(len == Range.fromInt(5))
  }

  test("variant length") {
    val len = measureLength(mkConcat(mkUnion('a', "ab"), mkUnion("cd", 'c')))
    assert(len == Range.fromScalaRange(2 to 4))
  }

  test("infinite length") {
    val len = measureLength(mkConcat(mkUnion('+', '-'), CharSet.NUM.+))
    assert(len == Range(2, PosInf))
  }

  test("rel pos to abs") {
    // s: [0-9]{2} "." [0-9]+
    val version1 = mkConcat(CharSet.NUM ^ 2, '.', CharSet.NUM.+)
    // i = s.find('.')
    val dot = find(version1.toCNF, '.', 0).toOption.get
    // int(i)
    val indexOfDot = measureLength(slice(version1.toCNF, RelPos(version1.toCNF, 0), dot).toOption.get)
    assert(indexOfDot == Range.fromInt(2))
  }
}
