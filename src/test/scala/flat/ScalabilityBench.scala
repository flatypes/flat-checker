package flat

import com.github.curiousoddman.rgxgen.RgxGen
import com.github.curiousoddman.rgxgen.config.{RgxGenOption, RgxGenProperties}
import com.github.curiousoddman.rgxgen.model.{RgxGenCharsDefinition, WhitespaceChar}
import com.github.curiousoddman.rgxgen.parsing.dflt.ConstantsProvider.UNICODE_SYMBOL_RANGE
import flat.regex.*
import flat.regex.AOps.*
import flat.util.Stopwatch
import org.apache.commons.text.StringEscapeUtils.escapeJava
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, Ignore}

import java.util
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

@Ignore
class ScalabilityBench extends AnyFunSuite, BeforeAndAfterAll:
  private val examples: List[(Int, String, RegEx)] =
    val source = Source.fromURL(getClass.getResource("/regex.txt"))
    val lines = source.getLines()
    List.from(for (regex, i) <- lines.zipWithIndex yield (i, regex, REParser.parse(regex)))

  private val data = ArrayBuffer.empty[ujson.Obj]

  private val fuzzerConfig =
    val properties = new RgxGenProperties
    RgxGenOption.INFINITE_PATTERN_REPETITION.setInProperties(properties, 10)
    RgxGenOption.WHITESPACE_DEFINITION.setInProperties(properties,
      util.Arrays.asList(WhitespaceChar.SPACE, WhitespaceChar.TAB, WhitespaceChar.CARRIAGE_RETURN,
        WhitespaceChar.LINE_FEED, WhitespaceChar.VERTICAL_TAB, WhitespaceChar.FORM_FEED))
    RgxGenOption.DOT_MATCHES_ONLY.setInProperties(properties, RgxGenCharsDefinition.of(UNICODE_SYMBOL_RANGE))
    properties

  override def beforeAll(): Unit =
    for (i, regex, _) <- examples do
      data += ujson.Obj("id" -> ujson.Num(i + 1), "regex" -> ujson.Str(regex), "length" -> ujson.Num(regex.length))

  test("reverse"):
    for (i, _, re) <- examples do
      val times = ArrayBuffer.empty[Double]
      for _ <- 1 to 10 do
        val (_, time) = Stopwatch.time { re.reverse }
        times += time
      data(i)("reverse time") = times

  test("length"):
    for (i, _, re) <- examples do
      val times = ArrayBuffer.empty[Double]
      for _ <- 1 to 10 do
        val (_, time) = Stopwatch.time { re.length }
        times += time
      data(i)("length time") = times

  test("take N"):
    for (i, _, re) <- examples do
      val ns = 1 to 10
      data(i)("take arg") = ns
      val times = ArrayBuffer.empty[Double]
      for n <- ns do
        val (_, time) = Stopwatch.time { re.take(n) }
        times += time
      data(i)("take time") = times

  test("take right N"):
    for (i, _, re) <- examples do
      val ns = 1 to 10
      data(i)("takeRight arg") = ns
      val times = ArrayBuffer.empty[Double]
      for n <- ns do
        val (_, time) = Stopwatch.time { re.take(IndexR(n)) }
        times += time
      data(i)("takeRight time") = times

  test("take index of char"):
    for (i, _, re) <- examples do
      val cs = randomSep(re.alphabet)
      data(i)("takeIndex arg") = cs.map(_.toString).map(escapeJava)
      val times = ArrayBuffer.empty[Double]
      for c <- cs do
        val (_, time) = Stopwatch.time { re.take(IndexAt(c.toString)) }
        times += time
      data(i)("takeIndex time") = times

  test("drop N"):
    for (i, _, re) <- examples do
      val ns = 1 to 10
      data(i)("drop arg") = ns
      val times = ArrayBuffer.empty[Double]
      for n <- ns do
        val (_, time) = Stopwatch.time { re.drop(n) }
        times += time
      data(i)("drop time") = times

  test("drop right N"):
    for (i, _, re) <- examples do
      val ns = 1 to 10
      data(i)("dropRight arg") = ns
      val times = ArrayBuffer.empty[Double]
      for n <- ns do
        val (_, time) = Stopwatch.time { re.drop(IndexR(n)) }
        times += time
      data(i)("dropRight time") = times

  test("drop index of char"):
    for (i, _, re) <- examples do
      val cs = randomSep(re.alphabet)
      data(i)("dropIndex arg") = cs.map(_.toString).map(escapeJava)
      val times = ArrayBuffer.empty[Double]
      for c <- cs do
        val (_, time) = Stopwatch.time { re.drop(IndexAt(c.toString)) }
        times += time
      data(i)("dropIndex time") = times

  test("starts with prefix"):
    for (i, regex, re) <- examples do
      val prefixes = randomPrefix(regex)
      data(i)("prefix arg") = prefixes.map(escapeJava)
      val times = ArrayBuffer.empty[Double]
      for prefix <- prefixes do
        val (_, time) = Stopwatch.time { re.startsWith(prefix) }
        times += time
      data(i)("prefix time") = times

  test("contains infix"):
    for (i, regex, re) <- examples do
      val infixes = randomInfix(regex)
      data(i)("infix arg") = infixes.map(escapeJava)
      val times = ArrayBuffer.empty[Double]
      for infix <- infixes do
        val (_, time) = Stopwatch.time { re.containsInfix(infix) }
        times += time
      data(i)("infix time") = times

  val asciiPunctuation: Set[Char] = Set(
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<',
    '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~'
  )

  private def randomSep(cs: CharSet, count: Int = 10): List[Char] =
    val set = cs.toSet
    val cs1 = Random.shuffle((set & asciiPunctuation).toList)
    if cs1.length >= count then
      return cs1.take(count)

    val cs2 = Random.shuffle((set -- asciiPunctuation).toList)
    if cs1.length + cs2.length >= count then
      return cs1 ++ cs2.take(count - cs1.length)

    val cs3 = Random.shuffle((asciiPunctuation -- set).toList)
    assert(cs1.length + cs2.length + cs3.length >= count)
    cs1 ++ cs2 ++ cs3.take(count - cs1.length - cs2.length)

  private def randomPrefix(regex: String, count: Int = 10): List[String] =
    val gen = RgxGen.parse(fuzzerConfig, regex)
    List.from:
      for _ <- 1 to count yield
        val s = gen.generate()
        if s.isEmpty then "" else
          val n = Random.between(1, (s.length min 20) + 1)
          s.take(n)

  private def randomInfix(regex: String, count: Int = 10): List[String] =
    val gen = RgxGen.parse(fuzzerConfig, regex)
    List.from:
      for _ <- 1 to count yield
        val s = gen.generate()
        if s.isEmpty then ""
        else
          val start = Random.nextInt(s.length)
          val n = Random.between(1, ((s.length - start) min 20) + 1)
          s.substring(start, start + n)

  override def afterAll(): Unit =
    val content = ujson.write(data, indent = 2)
    os.write.over(os.pwd / "scalability_bench.json", content)