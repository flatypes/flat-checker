package flat.checker.util

import scala.collection.mutable.ListBuffer

object CharSetUtil:
  private def compressRanges(chars: List[Char]): List[Char | (Char, Char)] =
    if chars.size <= 2 then chars
    else
      val ranges = ListBuffer.empty[Char | (Char, Char)]
      var start = chars.head
      var prev = start
      for c <- chars.tail do
        if c == prev + 1 then
          prev = c
        else
          ranges += (if start == prev then start else (start, prev))
          start = c
          prev = c
      ranges += (if start == prev then start else (start, prev))
      ranges.toList

  extension (s: Set[Char])
    def compress: List[Char | (Char, Char)] =
      val special = compressRanges(s.filter(_.isLetterOrDigit).toList.sorted)
      val others = s.filter(!_.isLetterOrDigit).toList.sorted
      special ++ others