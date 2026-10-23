package flat.checker.verif

import flat.checker.flan.tpd.*

object ExprOps:
  extension (expr: Expr)
    def conjuncts: List[Expr] = expr match
      case BoolConst(true) => Nil
      case And(e1, e2) => e1.conjuncts ++ e2.conjuncts
      case _ => List(expr)

enum CmpOp:
  case EQ, NE, LE, LT, GE, GT

  def flip: CmpOp = this match
    case EQ => EQ
    case NE => NE
    case LE => GE
    case LT => GT
    case GE => LE
    case GT => LT

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
    require(variables.forall { (_, coef) => coef == 1 || coef == -1 }, "Coefficients must be 1 or -1")
    val constant = terms.getOrElse(IntConst(1), 0)
    variables match
      case Nil => IntConst(BigInt(constant))
      case (e, coef) :: rest =>
        val first: Expr = if coef > 0 then e else Negate(e)
        val sum = rest.foldLeft(first) { case (l, (r, coef)) => if coef > 0 then Add(l, r) else Sub(l, r) }
        if constant > 0 then Add(sum, IntConst(BigInt(constant)))
        else if constant < 0 then Sub(sum, IntConst(BigInt(-constant)))
        else sum

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