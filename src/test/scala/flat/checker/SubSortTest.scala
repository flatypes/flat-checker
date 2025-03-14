package flat.checker

import org.scalatest.funsuite.AnyFunSuite

class SubSortTest extends AnyFunSuite:
  import Sort.*

  test("reflexivity"):
    assert(Int :<: Int)
    assert(Array(String) :<: Array(String))
    assert(Fun(Seq(Int), Bool) :<: Fun(Seq(Int), Bool))

  test("top and bot"):
    assert(Int :<: Top)
    assert(Bot :<: Int)

  test("co-/contra-variance"):
    assert(Fun(Seq(Top), String) :<: Fun(Seq(String), Top))