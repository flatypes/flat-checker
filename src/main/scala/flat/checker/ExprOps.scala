package flat.checker

import flat.checker.core.*
import flat.checker.core.ArithOp.*

object ExprOps:
  extension (expr: Expr)
  // Boolean
    /** Simplifies this condition. */
    def simpl: Expr = expr match
      case And(b1, b2) => And(b1.simpl, b2.simpl)
      case Or(b1, b2) => Or(b1.simpl, b2.simpl)
      case Not(And(b1, b2)) => Or(Not(b1).simpl, Not(b2).simpl)
      case Not(Or(b1, b2)) => And(Not(b1).simpl, Not(b2).simpl)
      case Not(Not(b)) => b.simpl
      case Not(Cmp(op, e1, e2)) => Cmp(Analyzer.negateCmpOp(op), e1, e2)
      case Ite(b, e1, e2) => Ite(b.simpl, e1, e2)
      case _ => expr

    /** Returns all conjuncts. */
    def conjuncts: List[Expr] =
      expr match
        case And(e1, e2) => e1.conjuncts ++ e2.conjuncts
        case _ => List(expr)

    /** Returns all disjuncts. */
    def disjuncts: List[Expr] =
      expr match
        case Or(e1, e2) => e1.disjuncts ++ e2.disjuncts
        case _ => List(expr)

    // Arithmetic

    /** Returns the negation `-expr`. */
    private def negation: Expr = expr match
      case Negate(e) => e
      case Const(n: Int) => Const(-n)
      case e => Negate(e)

    /** Returns all summands. */
    def summands: List[Expr] = expr match
      case Arith(ADD, e1, e2) => e1.summands ++ e2.summands
      case Arith(SUB, e1, e2) => e1.summands ++ e2.summands.map(_.negation)
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