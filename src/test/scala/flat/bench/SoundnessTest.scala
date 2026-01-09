package flat.bench

import flat.regex.AOps.*
import flat.regex.Interval
import org.apache.commons.text.StringEscapeUtils
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import java.util.regex.Pattern

// @Ignore
class SoundnessTest extends AnyFunSuite:
  test("length"):
    for (id, r) <- Resources.testRegExprs do
      val interval = r.length
      if interval != Interval(lb = 0) then // nontrivial
        for s <- Resources.testWords(id) do
          if !interval.contains(s.length) then
            fail(s"UNSOUND: #$id |$s| not in $interval")

  test("reverse"):
    for (id, r) <- Resources.testRegExprs do
      val r1 = r.reverse
      println(s"#$id: validation start")
      val pat = compileRegex(r1.toJavaRegex)
      for s <- Resources.testWords(id) do
        val s1 = s.reverse
        if !pat.matcher(s1).matches() then
          fail(s"UNSOUND: #$id: ${prettyString(s1)} ∉ $r1 ($pat)")
      println(s"#$id: validation OK")

  private def testTake(k: Int): Unit =
    for (id, r) <- Resources.testRegExprs do
      val r1 = r.take(k)
      for s <- Resources.testWords(id) do
        val s1 = s.take(k)
        if !r1.contains(s1) then
          fail(s"UNSOUND: #$id: ${prettyString(s1)} ∉ $r1")

  test("take 1"):
    testTake(1)

  test("take 2"):
    testTake(2)

  test("take 3"):
    testTake(3)

  private def testDrop(k: Int): Unit =
    for (id, r) <- Resources.testRegExprs do
      val r1 = r.drop(k)
      println(s"#$id: validation start")
      val pat = compileRegex(r1.toJavaRegex)
      for s <- Resources.testWords(id) do
        val s1 = s.drop(k)
        if !pat.matcher(s1).matches() then
          fail(s"UNSOUND: #$id: '$s1' ∉ $r1")
      println(s"#$id: validation OK")

  test("drop 1"):
    testDrop(1)

  private def compileRegex(regex: String): Pattern = Pattern.compile(regex, Pattern.DOTALL)

  private def prettyString(s: String): String =
    "\"" + s.flatMap(c => StringEscapeUtils.escapeJava(c.toString)) + "\""
