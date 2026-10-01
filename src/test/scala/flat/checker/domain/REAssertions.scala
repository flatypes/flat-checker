package flat.checker.domain

import flat.checker.domain.Prettifier.pp
import org.scalatest.Assertions
import org.scalatest.compatible.Assertion

trait REAssertions extends Assertions:
  export REParser.parse as re

  def assertCharRE(expected: StrRE)(actual: StrRE): Assertion =
    if expected == actual || (RESub.check(expected, actual) && RESub.check(actual, expected)) then
      succeed
    else
      fail(s"Expected ${expected.pp}, but got ${actual.pp}")

  def assertCharRE(expected: String)(actual: StrRE): Assertion =
    assertCharRE(re(expected))(actual)