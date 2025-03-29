package flat.checker.backend

import flat.checker.Bound
import flat.checker.Bound.*
import flat.checker.backend.core.*
import optimus.algebra.{Int2Const, Expression as MPExpr}
import optimus.optimization.*
import optimus.optimization.model.{MPBinaryVar, MPFloatVar, MPVar}

import scala.collection.mutable

object LPSolver:
  def solve(expr: Expr, premises: List[Expr]): (Bound, Bound) =
    val lb = solveLower(expr, premises)
    val ub = solveUpper(expr, premises)
    (lb, ub)

  def solveLower(expr: Expr, premises: List[Expr]): Bound =
    val slv = new Model
    for c <- premises do slv.addCond(c)
    slv.solve(expr, minimize = true)

  def solveUpper(expr: Expr, premises: List[Expr]): Bound =
    val slv = new Model
    for c <- premises do slv.addCond(c)
    slv.solve(expr, minimize = false)

  private class Model extends MPModel():
    def addCond(cond: Expr): Unit =
      cond match
        case Cmp(op, e1, e2) => addCompare(op, e1, e2)
        case Not(Cmp(op, e1, e2)) => addCompare(negate(op), e1, e2)
        case _ => // ignore other conditions

    private def negate(op: CmpOp): CmpOp =
      op match
        case CmpOp.EQ => CmpOp.NE
        case CmpOp.NE => CmpOp.EQ
        case CmpOp.LE => CmpOp.GT
        case CmpOp.LT => CmpOp.GE
        case CmpOp.GE => CmpOp.LT
        case CmpOp.GT => CmpOp.LE

    private def addCompare(op: CmpOp, left: Expr, right: Expr): Unit =
      val x = encodeExpr(left)
      val y = encodeExpr(right)
      op match
        case CmpOp.EQ => add(x := y)
        case CmpOp.LE => add(x <:= y)
        case CmpOp.LT => add(x <:= y - 1) // x < y iff x <= y - 1
        case CmpOp.GE => add(x >:= y)
        case CmpOp.GT => add(x >:= y + 1) // x > y iff x >= y + 1
        case CmpOp.NE =>
          /* x != y is encoded as
                x - y + ma >= 1
                x - y + ma <= m - 1
             where a is either 0 or 1, and m is a large value

             Ref: https://math.stackexchange.com/questions/37075/how-can-not-equals-be-expressed-as-an-inequality-for-a-linear-programming-model
           */
          val a = MPBinaryVar()(this)
          val m = 1000000
          add(x - y + m * a >:= 1)
          add(x - y + m * a <:= m - 1)

    def solve(objective: Expr, minimize: Boolean): Bound =
      val o = encodeExpr(objective)
      optimize(o, minimize)
      val result: Bound =
        if start() then objectiveValue.toInt
        else if minimize then NegInf else PosInf
      release()
      result

    private val abstraction = mutable.Map.empty[Expr, MPVar]

    private def encodeExpr(expr: Expr): MPExpr =
      // TODO: only consider int-sorted expressions
      abstraction.get(expr) match
        case Some(x) => x
        case None =>
          expr match
            case Const(i: Int) => i
            case Arith(op, e1, e2) =>
              val me1 = encodeExpr(e1)
              val me2 = encodeExpr(e2)
              op match
                case ArithOp.ADD => me1 + me2
                case ArithOp.SUB => me1 - me2
            case StrLen(_) =>
              val x = MPFloatVar.positive()(this)
              abstraction(expr) = x
              x
            case _ =>
              val x = MPFloatVar()(this)
              abstraction(expr) = x
              x