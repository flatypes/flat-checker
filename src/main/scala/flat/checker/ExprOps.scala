package flat.checker

import flat.checker.core.*
import flat.checker.core.ArithOp.*

object ExprOps:
  extension (expr: Expr)
    def negated: Expr = expr match
      case Negate(e) => e
      case Const(n: Int) => Const(-n)
      case e => Negate(e)

    def summands: List[Expr] = expr match
      case Arith(ADD, e1, e2) => e1.summands ++ e2.summands
      case Arith(SUB, e1, e2) => e1.summands ++ e2.summands.map(_.negated)
      case e => List(e)

    /** Returns the difference `expr - base` if it is a nonnegative constant. */
    def diffNonneg(base: Expr): Option[Int] =
      var unexpected = false
      var baseCount = 0
      var sum = 0
      expr.summands.foreach:
        case Const(n: Int) => sum += n
        case e => if e == base then baseCount += 1 else unexpected = true
      if !unexpected && baseCount == 1 && sum >= 0 then Some(sum) else None