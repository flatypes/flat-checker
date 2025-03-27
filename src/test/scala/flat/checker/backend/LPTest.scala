package flat.checker.backend

import flat.checker.Bound.PosInf
import flat.checker.backend.core.*
import flat.checker.backend.core.ArithOp.SUB
import flat.checker.backend.core.CmpOp.*
import org.scalatest.funsuite.AnyFunSuite

class LPTest extends AnyFunSuite:
  test("1"):
    // simplify ¬ (|s| > 0) as |s| = 0
    val sLen = StrLen(Var("|s|"))
    val cond = Not(GT(sLen, 0))
    val (lb, ub) = LPSolver.solve(sLen, List(cond))
    assert(lb.asInt == 0)
    assert(ub.asInt == 0)

  test("2"):
    // simplify i < |s| - 1, 0 ≤ i ≤ |s| - 1 as |s| - i > 1
    val i = Var("i")
    val sLen = StrLen(Var("s"))
    val conds = List(LT(i, SUB(sLen, 1)), LE(0, i), LE(i, SUB(sLen, 1)))
    val lb = LPSolver.solveLower(SUB(sLen, i), conds)
    assert(lb.asInt == 2)

  test("3"):
    // simplify 1 <= |s|, i < |s| - 1, 0 ≤ i as |s| ∈ [2,∞]
    val sLen = StrLen(Var("s"))
    val i = Var("i")
    val conds = List(LE(1, sLen), LT(i, SUB(sLen, 1)), LE(0, i))
    val (lb, ub) = LPSolver.solve(sLen, conds)
    assert(lb.asInt == 2)
    assert(ub == PosInf)

  test("4"):
    // simplify ¬ (i < |s| - 1), i ≤ |s| - 1 as |s| - i = 1
    val i = Var("i")
    val sLen = StrLen(Var("s"))
    val conds = List(Not(LT(i, SUB(sLen, 1))), LE(i, SUB(sLen, 1)))
    val (lb, ub) = LPSolver.solve(SUB(sLen, i), conds)
    assert(lb.asInt == 1)
    assert(ub.asInt == 1)