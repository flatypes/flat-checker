package flat.checker.backend

import optimus.algebra.Int2Const
import optimus.optimization.*
import optimus.optimization.enums.SolutionStatus.OPTIMAL
import optimus.optimization.model.{MPFloatVar, MPIntVar}
import org.scalatest.funsuite.AnyFunSuite

class MPModelTest extends AnyFunSuite:
  test("bounded int min"):
    implicit val model: MPModel = new MPModel
    // 0 <= x < y
    val x = MPIntVar("x", Int.MinValue to Int.MaxValue)
    val y = MPIntVar("y", Int.MinValue to Int.MaxValue)
    add(x >:= 0)
    add(y >:= x + 1)
    minimize(x - y) // expect: no solution, but actual: -1
    start()
    // assert(model.getStatus != OPTIMAL)
    release()

  test("bounded float min"):
    implicit val model: MPModel = new MPModel
    // 0 <= x < y
    val x = MPFloatVar("x")
    val y = MPFloatVar("y")
    add(x >:= 0)
    add(x + 1 <:= y)
    minimize(x - y)
    start()
    assert(model.getStatus != OPTIMAL)
    release()

  test("bounded float max"):
    implicit val model: MPModel = new MPModel
    // 0 <= x < y
    val x = MPFloatVar("x")
    val y = MPFloatVar("y")
    add(x >:= 0)
    add(x + 1 <:= y)
    maximize(x - y)
    val success = start()
    assert(success)
    assert(objectiveValue == -1)