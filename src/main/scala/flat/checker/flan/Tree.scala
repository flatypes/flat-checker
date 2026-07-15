package flat.checker.flan

type Literal = Null | Boolean | Int | Char | String

def escapeChar(c: Char): String =
  c match
    case '\'' => "\\'"
    case '\\' => "\\\\"
    case c =>
      if 0x20 <= c && c <= 0x7E then c.toString else escapeNonPrintable(c)

private def escapeNonPrintable(c: Char): String = c match
  case '\n' => "\\n"
  case '\r' => "\\r"
  case '\t' => "\\t"
  case _ => String.format("\\u%04x", c.toInt)

def escapeString(s: String): String =
  s.flatMap:
    case '"' => "\\\""
    case '\\' => "\\\\"
    case c =>
      if 0x20 <= c && c <= 0x7E then c.toString else escapeNonPrintable(c)

def escapeSMTString(s: String): String =
  s.flatMap: c =>
    if 0x20 <= c && c <= 0x7E then c.toString else String.format("\\u{%x}", c.toInt)