package flat.checker.domain

import org.scalatest.funspec.AnyFunSpec

class CharSetTest extends AnyFunSpec:
  describe("[0-7]"):
    val a = CharSet.from('0' to '7')
    val b = CharSet.from('0' to '9')

    it("is nonempty"):
      assert(a.nonEmpty)

    it("contains '0'"):
      assert(a.contains('0'))

    it("not contain '8'"):
      assert(!a.contains('8'))

    it("is subset of [0-9]"):
      assert(a.subsetOf(b))

    it("union with [89] is [0-9]"):
      assert((a | CharSet('8', '9')).equiv(b))

    it("intersection with [89] is empty"):
      assert((a & CharSet('8', '9')).isEmpty)