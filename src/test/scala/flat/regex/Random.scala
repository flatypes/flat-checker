package flat.regex

import flat.regex.RegEx.*

class Random extends scala.util.Random:
  def charFrom(cs: CharSet): Char =
    val i = between(0, cs.size)
    cs.toSet.toList(i)

  def stringIn(re: RegEx): String = re match
    case RENone => throw IllegalArgumentException("empty language")
    case RENull => ""
    case RELit(cs) => charFrom(cs).toString
    case REConcat(r1, r2) => stringIn(r1) + stringIn(r2)
    case REUnion(r1, r2) => if nextBoolean() then stringIn(r1) else stringIn(r2)
    case REStar(r) =>
      val n = between(0, 10)
      (0 until n).map(_ => stringIn(r)).mkString("")
