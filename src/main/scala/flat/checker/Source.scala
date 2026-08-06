package flat.checker

class Source(val uri: String, val text: String):
  def getLine(line: Int): String =
    val lines = text.linesIterator.toSeq
    if 0 <= line && line < lines.length then
      lines(line)
    else
      throw new IndexOutOfBoundsException(s"Line $line is out of bounds: $uri has ${lines.length} lines.")

object Source:
  def fromPath(path: os.Path): Source =
    val uri = path.toNIO.toUri.toString
    val text = os.read(path)
    Source(uri, text)