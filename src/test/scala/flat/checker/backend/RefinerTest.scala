package flat.checker.backend

import flat.checker.Bound.PosInf
import flat.checker.ReLang.ReEmpty
import flat.checker.{Interval, ReLang}
import org.scalatest.funsuite.AnyFunSuite

class RefinerTest extends AnyFunSuite:
  given Types = Types.from(Nil)

  test("refine by length interval"):
    val r1 = ReLang.fromPython("a?")
    assert(Refiner.refine(r1, LenIn(0)) equiv ReEmpty)
    assert(Refiner.refine(r1, LenIn(Interval(1, PosInf))) equiv ReLang.fromPython("a"))
    val r2 = ReLang.fromPython("(ab)*")
    assert(Refiner.refine(r2, LenIn(0)) equiv ReEmpty)
    assert(Refiner.refine(r2, LenIn(Interval(1, PosInf))) equiv ReLang.fromPython("(ab)+"))
    assert(Refiner.refine(r2, LenIn(Interval(3, 11))) equiv ReLang.fromPython("(ab){2,5}"))

  test("refine by must-contained char"):
    val r1 = ReLang.fromPython("a*b*")
    assert(Refiner.refine(r1, Contain('b')) equiv ReLang.fromPython("a*b+"))
    assert(Refiner.refine(r1, Contain('a')) equiv ReLang.fromPython("a+b*"))
    val r2 = ReLang.fromPython("([^b][^b]*b|b.).*|[^b]*")
    assert(Refiner.refine(r2, Contain('b')) equiv ReLang.fromPython("([^b][^b]*b|b.).*"))

  test("refine by never-contained chars"):
    val r1 = ReLang.fromPython("a*b*")
    assert(Refiner.refine(r1, NotContain('b')) equiv ReLang.fromPython("a*"))
    assert(Refiner.refine(r1, NotContain('a')) equiv ReLang.fromPython("b*"))
    val r2 = ReLang.fromPython("b.|a*")
    assert(Refiner.refine(r2, NotContain('b')) equiv ReLang.fromPython("a*"))
    assert(Refiner.refine(r2, NotContain('a')) equiv ReLang.fromPython("b[^a]|"))

  test("refine by char at"):
    val r = ReLang.fromPython("(0|01)+")
    assert(Refiner.refine(r, CharAt(1, true, '1')) equiv ReLang.fromPython("01(0|01)*"))
    assert(Refiner.refine(r, CharAt(1, false, '1')) equiv ReLang.fromPython("0(0|01)+"))
    val r1 = ReLang.fromPython("1?(0|01)*")
    assert(Refiner.refine(r1, CharAt(-2, true, '1')) equiv ReLang.fromPython("1?(0|01)*010"))
