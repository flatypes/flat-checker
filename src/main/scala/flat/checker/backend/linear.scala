package flat.checker.backend

import flat.checker.backend.core.ArithOp.{ADD, SUB}
import flat.checker.backend.core.{Arith, ArithOp, Const, Expr}

import scala.collection.mutable.ListBuffer

object linear:
  def flattenArith(expr: Expr): List[(ArithOp, Expr)] =
    expr match
      case Arith(ADD, e1, e2) => flattenArith(e1) ++ flattenArith(e2)
      case Arith(SUB, e1, e2) => flattenArith(e1) ++ flattenArith(e2).map { case op -> e => !op -> e }
      case _ => List(ADD -> expr)

  def simplifyArith(expr: Expr): Expr =
    var sum = 0
    val terms = ListBuffer.empty[(ArithOp, Expr)]
    flattenArith(expr).foreach {
      case ADD -> Const(k: Int) => sum += k
      case SUB -> Const(k: Int) => sum -= k
      case op -> e => terms += (op -> e)
    }
    terms.toList match
      case Nil => Const(sum)
      case (ADD, e0) :: ts =>
        val e = ts.foldLeft(e0) { case (acc, (op, e)) => Arith(op, acc, e) }
        sum match
          case 0 => e
          case _ if sum > 0 => ADD(e, sum)
          case _ => SUB(e, -sum)
      case ts =>
        val e0: Expr = Const(sum)
        ts.foldLeft(e0) { case (acc, (op, e)) => Arith(op, acc, e) }
