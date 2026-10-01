package flat.checker.domain

import flat.checker.domain.REOps.*
import org.scalatest.funspec.AnyFunSpec

class LengthTest extends AnyFunSpec, REAssertions:
  describe("a(b|c)?"):
    val r = re("a(b|c)?")

    it("group by length"):
      val m = r.groupByLength(2)
      info(m.toString)

  describe("a(b|c)*"):
    val r = re("a(b|c)*")

    it("group by length"):
      val m = r.groupByLength(3)
      info(m.toString)