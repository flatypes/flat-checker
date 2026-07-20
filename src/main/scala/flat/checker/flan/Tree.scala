package flat.checker.flan

type Literal = Null | Boolean | Int | Char | String
def parseInt(text: String): Int =
  val numberPart = if text.startsWith("-") then text.drop(1) else text
  val n =
    if numberPart.startsWith("0x") then Integer.parseInt(numberPart.drop(2), 16)
    else if numberPart.startsWith("0b") then Integer.parseInt(numberPart.drop(2), 2)
    else Integer.parseInt(numberPart)
  if text.startsWith("-") then -n else n
def unescape(text: String): String =
  var i = 0
  val sb = StringBuilder()
  while i < text.length do
    text.charAt(i) match
      case '\\' =>
        text.charAt(i + 1) match
          case '\\' | '\'' | '"' => sb += text.charAt(i); i += 2
          case 'a' => sb += '\u0007'; i += 2
          case 'b' => sb += '\b'; i += 2
          case 'f' => sb += '\f'; i += 2
          case 'n' => sb += '\n'; i += 2
          case 'r' => sb += '\r'; i += 2
          case 't' => sb += '\t'; i += 2
          case 'v' => sb += '\u000B'; i += 2
          case c if '0' <= c && c <= '7' => Integer.parseInt(text.substring(i + 1, i + 4), 8).toChar; i += 4
          case 'x' => sb += Integer.parseInt(text.substring(i + 2, i + 4), 16).toChar; i += 4
          case 'u' => sb += Integer.parseInt(text.substring(i + 2, i + 6), 16).toChar; i += 6
          case _ => throw IllegalArgumentException("Unknown escape sequence")
      case c => sb += c; i += 1
  sb.toString

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