package flat.regex

import org.scalatest.funsuite.AnyFunSuite

trait RegexTest extends AnyFunSuite:
  export REParser.parse as re
  export CharSet.of as ch
