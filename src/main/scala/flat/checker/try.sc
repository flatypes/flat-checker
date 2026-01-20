import flat.regex.*
import flat.regex.AOps.*
import flat.regex.NarrowOps.*
import flat.regex.RegEx.*

val oct = REParser.parse("[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5]")
oct.toNumber()

val ipv4 = REParser.tryParse("""{DecOctet}\.{DecOctet}\.{DecOctet}\.{DecOctet}""",
  Map("DecOctet" -> oct)).toOption.get
ipv4.splitBy('.')

val h16 = REParser.parse("[0-9A-Fa-f]{1,4}")
h16.toNumber(16)

val ls32 = REParser.tryParse("""{H16}:{H16}|{IPv4Address}""",
  Map("H16" -> h16, "IPv4Address" -> ipv4)).toOption.get
val ipv6 = REParser.tryParse(
  """({H16}:){6}{LS32}|::({H16}:){5}{LS32}|{H16}?::({H16}:){4}{LS32}|""" +
    """(({H16}:){0,1}{H16})?::({H16}:){3}{LS32}|(({H16}:){0,2}{H16})?::({H16}:){2}{LS32}|""" +
    """(({H16}:){0,3}{H16})?::{H16}:{LS32}|(({H16}:){0,4}{H16})?::{LS32}|""" +
    """(({H16}:){0,5}{H16})?::{H16}|(({H16}:){0,6}{H16})?::""",
  Map("H16" -> h16, "LS32" -> ls32, "IPv4Address" -> ipv4)
).toOption.get

val ipv6With4 = ipv6.reverse.narrowPartBy(0, _.narrowByContain('.'), ':').reverse
ipv6With4.reverse.splitPart(':', 0).reverse.alphabet
