package flat.bench

import com.github.curiousoddman.rgxgen.RgxGen
import com.github.curiousoddman.rgxgen.config.{RgxGenOption, RgxGenProperties}
import com.github.curiousoddman.rgxgen.model.{RgxGenCharsDefinition, WhitespaceChar}
import com.github.curiousoddman.rgxgen.parsing.dflt.ConstantsProvider.UNICODE_SYMBOL_RANGE
import flat.regex.RegEx.*
import flat.regex.{CharSet, REParser, RegEx}
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import java.util
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.jdk.OptionConverters.*
import scala.util.Random

object Resources:
  lazy val testRegexes: List[(Int, String)] =
    val source = Source.fromURL(getClass.getResource("/regex.txt"))
    val lines = source.getLines()
    List.from(for (regex, i) <- lines.zipWithIndex if !regex.startsWith("// ") yield i + 1 -> regex)

  lazy val testRegExprs: List[(Int, RegEx)] =
    for (i, regex) <- testRegexes yield i -> REParser.parse(regex)

  lazy val testWords: Map[Int, List[String]] =
    Map.from(for (i, regex) <- testRegexes yield i -> genWords(regex))

def genWords(regex: String, n: Int = 100): List[String] =
  val properties = new RgxGenProperties
  RgxGenOption.INFINITE_PATTERN_REPETITION.setInProperties(properties, 10)
  RgxGenOption.WHITESPACE_DEFINITION.setInProperties(properties,
    util.Arrays.asList(WhitespaceChar.SPACE, WhitespaceChar.TAB, WhitespaceChar.CARRIAGE_RETURN,
      WhitespaceChar.LINE_FEED, WhitespaceChar.VERTICAL_TAB, WhitespaceChar.FORM_FEED))
  RgxGenOption.DOT_MATCHES_ONLY.setInProperties(properties, RgxGenCharsDefinition.of(UNICODE_SYMBOL_RANGE))

  val gen = RgxGen.parse(properties, regex)
  val buf = ListBuffer.empty[String]
  gen.getUniqueEstimation.toScala match
    case Some(m) if m.doubleValue() < 100 =>
      val it = gen.iterateUnique()
      while buf.size < n && it.hasNext do
        buf += it.next()
    case _ =>
      while buf.size < n do
        buf += gen.generate()
  buf.toList

@Ignore
class GenTest extends AnyFunSuite:
  test("\\d"):
    val words = genWords("\\d")
    assert(words.forall(w => w.length == 1 && CharSet.asciiDigit.contains(w.head)))
    assert(CharSet.asciiDigit.toSet.map(_.toString).forall(words.contains))

  test("\\w"):
    val words = genWords("\\w")
    assert(words.forall(w => w.length == 1 && CharSet.asciiWord.contains(w.head)))
    assert(CharSet.asciiWord.toSet.map(_.toString).forall(words.contains))

  test("\\s"):
    val words = genWords("\\s")
    assert(words.forall(w => w.length == 1 && CharSet.asciiSpace.contains(w.head)))
    assert(CharSet.asciiSpace.toSet.map(_.toString).forall(words.contains))

  test("[ab]{2,1024}"):
    val words = genWords("a{2,1024}")
    assert(words.forall(w => w.forall("ab".contains) && 2 <= w.length && w.length <= 1024))

  test("(\\d{3})?"):
    val words = genWords("(\\d{3})?")
    assert(words.forall(w => w.isEmpty || (w.length == 3 && w.forall(CharSet.asciiDigit.contains))))

  test("[^\\x00-\\xFF]"):
    val words = genWords("[^\\x00-\\xFF]")
    assert(words.forall(w => w.length == 1 && w.head > 0xFF))

  test("[^\\W]"):
    val words = genWords("[^\\W]")
    assert(words.forall(w => w.length == 1 && CharSet.asciiWord.contains(w.head)))

def genWordsSlow(regex: String, n: Int = 1000): List[String] =
  val r = REParser.parse(regex)
  List.from(for _ <- 0 until n yield genWordSlow(r))

def genWordSlow(re: RegEx): String = re match
  case RENone => throw IllegalArgumentException("empty language")
  case RENull => ""
  case RELit(cs) => genCharSlow(cs).toString
  case REConcat(r1, r2) => genWordSlow(r1) + genWordSlow(r2)
  case REUnion(r1, r2) => if Random.nextBoolean() then genWordSlow(r1) else genWordSlow(r2)
  case REStar(r) =>
    val k = Random.between(0, 5)
    (for _ <- 0 until k yield genWordSlow(r)).mkString

def genCharSlow(cs: CharSet): Char =
  val chars = cs.toSet
  val idx = Random.nextInt(chars.size)
  chars.toList(idx)
