package flat.checker.backend

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Bound
import flat.checker.Bound.*
import flat.checker.backend.core.ArithOp.SUB
import flat.checker.backend.core.CmpOp.{GE, LE}
import flat.checker.backend.core.{Cmp, CmpOp, Expr}

object RangeSolver extends LazyLogging:
  def solve(expr: Expr, premises: List[Expr])(using types: Types): (Bound, Bound) =
    val slv = new SMTSolver.Interactive
    slv.addPremises(premises)
    // guess lower bound
    var cont = true
    var k = -4
    while cont && k <= 4 do
      if slv.canProve(GE(expr, k)) then k += 1 else cont = false
    val lb = if k == -4 then NegInf else if cont then PosInf else Fin(k - 1)
    // guess upper bound
    cont = true
    k = 4
    while cont && k >= -4 do
      if slv.canProve(LE(expr, k)) then k -= 1 else cont = false
    val ub = if k == 4 then PosInf else if cont then NegInf else Fin(k + 1)
    (lb, ub)

  def canProve(compare: Cmp, premises: List[Expr])(using types: Types): Boolean =
    val (lb, ub) = solve(SUB(compare.left, compare.right), premises) // e1 - e2
    compare.op match
      case CmpOp.EQ => lb == Fin(0) && ub == Fin(0) // e1 == e2 <-> e1 - e2 == 0
      case CmpOp.NE => lb != Fin(0) || ub != Fin(0)
      case CmpOp.LE => ub == Fin(0) // e1 <= e2 <-> e1 - e2 <= 0
      case CmpOp.LT => ub == Fin(-1) // e1 < e2 <-> e1 - e2 < 0 (<= -1)
      case CmpOp.GE => lb == Fin(0) // e1 >= e2 <-> e1 - e2 >= 0
      case CmpOp.GT => lb == Fin(1) // e1 > e2 <-> e1 - e2 > 0 (>= 1)