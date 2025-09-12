package flat.regex

import flat.regex.REParser.parse as re
import flat.regex.simpl.*
import org.scalatest.funsuite.AnyFunSuite

class SimplTest extends AnyFunSuite:
  test("extract common factor"):
    assert(re("abc|abd").extractCommonFactor == re("ab(c|d)"))
    assert(re("aa(a|b)*|ba(a|b)*").extractCommonFactor == re("(a|b)a(a|b)*"))
