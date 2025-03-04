package flat.checker.abs

import flat.checker.abs.ReLang.*
import flat.checker.abs.RegexImplicits.*
import flat.checker.abs.analysis.*
import org.scalatest.funsuite.AnyFunSuite

class ElementTest extends AnyFunSuite {
  test("must start with union") {
    val r = mkConcat('a', mkUnion('b', "bc"))
    assert(mustStartWith(r, "a"))
    assert(mustStartWith(r, "ab"))
  }

  test("must start with star") {
    val r = mkConcat('a', mkConcat('b', 'c').*, 'b')
    assert(mustStartWith(r, "a"))
    assert(mustStartWith(r, "ab"))
  }

  test("may start with") {
    val r = mkConcat(mkUnion('+', '-'), CharSet.NUM.+)
    assert(mayStartWith(r, "+"))
    assert(mayStartWith(r, "-"))
    assert(mayStartWith(r, "-1"))
    assert(mayStartWith(r, "+12"))
    assert(!mayStartWith(r, "0"))
    assert(!mayStartWith(r, "-+"))
  }

  test("must contain character") {
    val r = mkConcat('a', mkConcat('b', 'c').*, 'b')
    assert(mustContain(r, 'a'))
    assert(mustContain(r, 'b'))
    assert(!mustContain(r, 'c'))
  }

  test("may contain character") {
    val r = mkConcat(mkUnion('+', '-'), CharSet.NUM.+)
    assert(mayContain(r, '+'))
    assert(mayContain(r, '-'))
    assert(mayContain(r, '0'))
    assert(!mayContain(r, '*'))
  }

  test("only contain characters") {
    val r = mkConcat('-'.?, CharSet.from('1' to '9').+)
    assert(onlyContain(r, CharSet.NUM | CharSet.of('-')))
    assert(!onlyContain(r, CharSet.NUM))
  }
}
