package flat.checker.abs

import flat.checker.abs.ReLang.ReChars

import scala.language.implicitConversions

object RegexImplicits:
  implicit def charToReChar(c: Char): ReLang = ReChars(CharSet.of(c))

  implicit def charSetToReChar(cs: CharSet): ReLang = ReChars(cs)

  implicit def stringToRegex(s: String): ReLang = ReLang.fromString(s)
