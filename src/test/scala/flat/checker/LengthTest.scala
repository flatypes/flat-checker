package flat.checker

import flat.checker.Bound.*
import flat.checker.ReLang.*
import flat.checker.RegexImplicits.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class LengthTest extends AnyFunSuite:
  test("fixed length"):
    val len = mkConcat(mkUnion('+', '-'), CharSet.NUM ^ 4).length
    assert(len == Interval.fromInt(5))

  test("variant length"):
    val len = mkConcat(mkUnion('a', "ab"), mkUnion("cd", 'c')).length
    assert(len == Interval.fromRange(2 to 4))

  test("infinite length"):
    val len = mkConcat(mkUnion('+', '-'), CharSet.NUM.+).length
    assert(len == Interval(2, PosInf))

  test("rel pos to abs"):
    // s: [0-9]{2} "." [0-9]+
    val version1 = mkConcat(CharSet.NUM ^ 2, '.', CharSet.NUM.+).toCNF
    // i = s.indexOf('.')
    val pos = CNFOps.indexOf(version1, '.').toOption.get
    // int(i)
    val i = Index(version1, pos).toType.interval.asInt
    assert(i == 2)
