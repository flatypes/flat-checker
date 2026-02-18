package flat.checker

import flat.Ops.CmpOp.*
import flat.checker.ast.*
import org.scalatest.funspec.AnyFunSpec

class LPSolverTest extends AnyFunSpec:
  describe("Linear system 1"):
    val i = Var("i")
    val s = Var("s")
    val solver = LPSolver(List(
      LT(i, Sub(StringLength(s), Const(1))),
      GE(i, Const(0)),
      LE(i, Sub(StringLength(s), Const(1))),
      GE(StringLength(s), Const(0))
    ))

    it("solve |s|"):
      assert(solver.solve(StringLength(s)) == (Some(2), None)) // |s| >= 2

    it("solve i"):
      assert(solver.solve(i) == (Some(0), None)) // i >= 0

    it("solve |s| - i"):
      assert(solver.solve(Sub(StringLength(s), i)) == (Some(2), None)) // i <= |s| - 2