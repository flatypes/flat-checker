package flat.checker

import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import org.scalatest.funspec.AnyFunSpec

class LPSolverTest extends AnyFunSpec:
  describe("Linear system 1"):
    val i = Var("i")
    val s = Var("s")
    val solver = LPSolver(List(
      LT(i, SUB(Length(s), 1)),
      GE(i, 0),
      LE(i, SUB(Length(s), 1)),
      GE(Length(s), 0)
    ))

    it("solve |s|"):
      assert(solver.solve(Length(s)) == (Some(2), None)) // |s| >= 2

    it("solve i"):
      assert(solver.solve(i) == (Some(0), None)) // i >= 0

    it("solve |s| - i"):
      assert(solver.solve(SUB(Length(s), i)) == (Some(2), None)) // i <= |s| - 2