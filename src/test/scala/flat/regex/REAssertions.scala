package flat.regex

import flat.regex.simpl.equalsRE
import org.scalatest.Assertions

trait REAssertions extends Assertions:
  export flat.regex.REParser.parse as re

  def assertEqual(r1: RegEx, r2: RegEx): Unit =
    if !(r1 equalsRE r2) then
      fail(s"$r1 != $r2")

  def assertEqual(r1: RegEx, r2: String): Unit =
    assertEqual(r1, re(r2))

  def assertEquiv(r1: RegEx, r2: RegEx): Unit =
    if !(r1 equiv r2) then
      fail(s"$r1 != $r2")

  def assertEquiv(r1: RegEx, r2: String): Unit =
    assertEquiv(r1, re(r2))

  def assertEquiv(r1: String, r2: String): Unit =
    assertEquiv(re(r1), r2)

  def assertEmpty(r: RegEx): Unit =
    if !r.isEmpty then
      fail(s"$r is nonempty")