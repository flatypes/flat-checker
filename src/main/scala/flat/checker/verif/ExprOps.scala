package flat.checker.verif

import flat.checker.flan.tpd.*

object ExprOps:
  extension (expr: Expr)
    def conjuncts: List[Expr] = expr match
      case BoolLit(true) => Nil
      case And(e1, e2) => e1.conjuncts ++ e2.conjuncts
      case _ => List(expr)