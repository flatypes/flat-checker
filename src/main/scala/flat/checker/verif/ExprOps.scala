package flat.checker.verif

import flat.checker.domain.CmpOp
import flat.checker.flan.tpd.*

object ExprOps:
  extension (expr: Expr)
    def conjuncts: List[Expr] = expr match
      case BoolConst(true) => Nil
      case And(e1, e2) => e1.conjuncts ++ e2.conjuncts
      case _ => List(expr)

type Inequality = (Expr, CmpOp, Expr)

object InequalitySolver:
  def flatten(expr: Expr, multiplier: Int): List[(Int, Expr)] = expr match
    case IntConst(i) => List((multiplier * i.toInt, IntConst(1)))
    case Negate(e) => flatten(e, -multiplier)
    case Add(e1, e2) => flatten(e1, multiplier) ++ flatten(e2, multiplier)
    case Sub(e1, e2) => flatten(e1, multiplier) ++ flatten(e2, -multiplier)
    case _ => List((multiplier, expr))

  def unflatten(terms: Map[Expr, Int]): Expr =
    val variables = terms.filter(_._1 != IntConst(1)(noRange)).toList
    val constant = terms.getOrElse(IntConst(1), 0)
    variables match
      case Nil => IntConst(BigInt(constant))
      case (e, coef) :: rest =>
        val first: Expr = multiply(e, coef)
        val sum = rest.foldLeft(first) { case (l, (r, coef)) =>
          if coef > 0 then Add(l, multiply(r, coef)) else Sub(l, multiply(r, -coef))
        }
        if constant > 0 then Add(sum, IntConst(BigInt(constant)))
        else if constant < 0 then Sub(sum, IntConst(BigInt(-constant)))
        else sum

  def multiply(expr: Expr, factor: Int): Expr =
    if factor == 1 then expr
    else if factor > 1 then Mul(IntConst(BigInt(factor)), expr)
    else if factor == -1 then Negate(expr)
    else if factor < -1 then Mul(IntConst(BigInt(factor)), expr)
    else IntConst(BigInt(0))

  def simplifyArith(expr: Expr): Expr = expr match
    case _: (Negate | Add | Sub) =>
      val terms = flatten(expr, 1)
      val grouped = terms.groupBy(_._2).map { (x, list) => (x, list.map(_._1).sum) }.filter(_._2 != 0)
      unflatten(grouped)
    case _ => expr

  def isolate(target: Expr, ineq: Inequality): Option[(CmpOp, Expr)] =
    val (left, op, right) = ineq
    // Move all terms to the left side
    val terms = flatten(left, 1) ++ flatten(right, -1)
    // Group terms by variables/constants
    val grouped = terms.groupBy(_._2).map { (x, list) => (x, list.map(_._1).sum) }.filter(_._2 != 0)
    // Make sure the coefficients of every variable are either 1 or -1
    val variables = grouped.filter(_._1 != IntConst(1)(noRange))
    if variables.exists { (_, coef) => coef != 1 && coef != -1 } then
      return None

    // Check if target is present
    val targetCoef = variables.getOrElse(target, 0)
    if targetCoef == 0 then
      return None

    // Build the result
    val resultOp = if targetCoef > 0 then op else op.flip
    val result = unflatten(grouped.filter(_._1 != target).map { (x, coef) => (x, -coef / targetCoef) })
    Some((resultOp, result))