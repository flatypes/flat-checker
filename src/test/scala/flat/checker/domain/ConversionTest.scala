package flat.checker.domain

import flat.checker.domain.StrREOps.*
import org.scalatest.funspec.AnyFunSpec

class ConversionTest extends AnyFunSpec, REAssertions:
  describe("from integer"):
    it("[1, 12]"):
      val fmt = NumStrFormat()
      assertCharRE("[1-9]|1[0-2]")(absFromInt(1, 12, fmt))

    it("[1, 12] with zero padding"):
      val fmt = NumStrFormat(zeroPadded = true, width = 2)
      assertCharRE("0[1-9]|1[0-2]")(absFromInt(1, 12, fmt))

    it("[1, 31]"):
      val fmt = NumStrFormat()
      assertCharRE("[1-9]|[12][0-9]|3[01]")(absFromInt(1, 31, fmt))

    it("[1, 31] with zero padding"):
      val fmt = NumStrFormat(zeroPadded = true, width = 2)
      assertCharRE("0[1-9]|[12][0-9]|3[01]")(absFromInt(1, 31, fmt))

    it("[129, 131]"):
      val fmt = NumStrFormat()
      assertCharRE("129|13[01]")(absFromInt(129, 131, fmt))

    it("[1, 321654]"):
      val fmt = NumStrFormat()
      assertCharRE("[1-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9][0-9][0-9]|[1-9][0-9][0-9][0-9][0-9]|" +
        "[12][0-9][0-9][0-9][0-9][0-9]|3[01][0-9][0-9][0-9][0-9]|320[0-9][0-9][0-9]|321[0-5][0-9][0-9]|" +
        "3216[0-4][0-9]|32165[0-4]")(absFromInt(1, 321654, fmt))

    it("[0, 75]"):
      val fmt = NumStrFormat()
      assertCharRE("[0-9]|[1-6][0-9]|7[0-5]")(absFromInt(0, 75, fmt))

    it("[0, 75] with zero padding"):
      val fmt = NumStrFormat(zeroPadded = true, width = 2)
      assertCharRE("[0-6][0-9]|7[0-5]")(absFromInt(0, 75, fmt))

    it("[0x0, 0xFF] with zero padding, uppercase"):
      val fmt = NumStrFormat(zeroPadded = true, width = 2, conv = 'X')
      assertCharRE("[0-9A-F][0-9A-F]")(absFromInt(0x0, 0xFF, fmt))

    it("[0x0, 0xff] with zero padding, lowercase"):
      val fmt = NumStrFormat(zeroPadded = true, width = 2, conv = 'x')
      assertCharRE("[0-9a-f][0-9a-f]")(absFromInt(0x0, 0xFF, fmt))