package flat.regex

import flat.regex.simpl.*
import org.scalatest.funsuite.AnyFunSuite

class SimplTest extends AnyFunSuite, REAssertions:
  test("extract common factor"):
    assertEqual(re("abc|abd").extractCommonFactor, "ab(c|d)")
    assertEqual(re("aa(a|b)*|ba(a|b)*").extractCommonFactor, "(a|b)a(a|b)*")
