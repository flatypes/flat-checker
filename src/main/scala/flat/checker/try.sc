import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ArithOp.{ADD, SUB}
import flat.checker.{SMTSolver, VarCtx}
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.NarrowOps.*
import flat.regex.RegEx.*

// split
val simple = REParser.parse("a:(b:|c:d)")
simple.splitWith(':')

// simplified ipv6
val sipv6 = REParser.parse(
  """(A:){4}(A:A|4)|::(A:){3}(A:A|4)|A?::(A:){2}(A:A|4)|""" +
    """((A:){0,1}A)?::(A:)(A:A|4)|((A:){0,2}A)?::(A:A|4)|((A:){0,3}A)?::A|((A:){0,4}A)?::"""
)

val sParts = sipv6.splitWith(':')
val sPartsMid = sParts.drop(1).dropRight(1)
sPartsMid.count("")
val sPartsNotNull = sParts.narrowEach(1, 1, _.narrowByNotEq(""))
sPartsNotNull.length

val sPartsV4 = sParts.narrowRight(1, _.narrowByEq("4"))
sPartsV4.getRight(1)
val sPartsV4Updated = sPartsV4.dropRight(1).append(fromChar('A')).append(fromChar('A'))
sPartsV4Updated.drop(1).dropRight(1).count("")
sPartsV4Updated.getEach(1, 1)

// ipv4
val oct = REParser.parse("[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5]")
oct.toNumber()

val ipv4 = REParser.tryParse("""{DecOctet}\.{DecOctet}\.{DecOctet}\.{DecOctet}""",
  Map("DecOctet" -> oct)).toOption.get
val ipv4Parts = ipv4.splitWith('.')
ipv4Parts.length
ipv4Parts.getAny

// ipv6
val h16 = REParser.parse("[0-9A-Fa-f]{1,4}")
h16.toNumber(16)

val ls32 = REParser.tryParse("{H16}:{H16}|{IPv4Address}",
  Map("H16" -> h16, "IPv4Address" -> ipv4)).toOption.get
val ipv6 = REParser.tryParse(
  """({H16}:){6}{LS32}|::({H16}:){5}{LS32}|{H16}?::({H16}:){4}{LS32}|""" +
    """(({H16}:){0,1}{H16})?::({H16}:){3}{LS32}|(({H16}:){0,2}{H16})?::({H16}:){2}{LS32}|""" +
    """(({H16}:){0,3}{H16})?::{H16}:{LS32}|(({H16}:){0,4}{H16})?::{LS32}|""" +
    """(({H16}:){0,5}{H16})?::{H16}|(({H16}:){0,6}{H16})?::""",
  Map("H16" -> h16, "LS32" -> ls32, "IPv4Address" -> ipv4)
).toOption.get

ipv6.toJavaRegex.length

val parts = ipv6.splitWith(':')
parts.length
val partsMid = parts.drop(1).dropRight(1)
partsMid.count("")

parts.narrowSomeFromIndexOfUntil(("", 1, 1), 1, 1, _.narrowByEq(""))

val partsWithV4 = parts.narrowRight(1, _.narrowByContain('.'))
partsWithV4.getRight(1).alphabet

val parts1 = parts.narrow(0, _.narrowByEq(""))
parts1.drop(1).dropRight(1).indexOf("")

val parts2 = parts.narrowRight(1, _.narrowByEq(""))
parts2.drop(1).dropRight(1).rightIndexOf("")

val partsWithV4Updated = partsWithV4.dropRight(1).append(fromString("%x")).append(fromString("%x"))
val parts3 = partsWithV4Updated.narrowEach(1, 1, _.narrowByNotEq(""))
parts3.length

partsWithV4Updated.narrow(0, _.narrowByNotEq("")).takeIndexOf("", 1, 1).getAny

val partsWithoutV4 = parts.narrowRight(1, _.narrowByNotContain('.'))
partsWithoutV4.narrowEach(1, 1, _.narrowByNotEq("")).length
