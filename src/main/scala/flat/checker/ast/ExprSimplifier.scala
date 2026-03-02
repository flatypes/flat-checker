package flat.checker.ast

import flat.Ops.CmpOp.*

object ExprSimplifier:
  // Simplifier assuming all arguments are simplified.
  private def and(conjuncts: List[Expr]): Expr =
    val bs = conjuncts.flatMap:
      case And(es) => es
      case e => List(e)
    if bs.contains(Const(false)) then Const(false) // false ∧ _ = false
    else bs.filter(_ != Const(true)) match
      case Nil => Const(true)
      case List(b) => b
      case bs => And(bs)

  private def or(disjuncts: List[Expr]): Expr =
    val bs = disjuncts.flatMap:
      case Or(es) => es
      case e => List(e)
    if bs.contains(Const(true)) then Const(true) // true ∨ _ = true
    else bs.filter(_ != Const(false)) match
      case Nil => Const(false)
      case List(b) => b
      case bs => Or(bs)

  private def not(cond: Expr): Expr = cond match
    case And(bs) => Or(bs.map(not)) // ¬(b1 ∧ b2) = ¬b1 ∨ ¬b2
    case Or(bs) => And(bs.map(not)) // ¬(b1 ∨ b2) = ¬b1 ∧ ¬b2
    case Not(b) => b // ¬(¬b) = b
    case Implies(b1, b2) => And(b1, not(b2)) // ¬(b1 ⇒ b2) = b1 ∧ ¬b2
    case Ite(b, b1, b2) => Ite(b, not(b1), not(b2)) // ¬(if b then b1 else b2) = if b then ¬b1 else ¬b2
    case RelExpr(op, e1, e2) => RelExpr(op.negation, e1, e2) // ¬(e1 op e2) = e1 ¬op e2
    case _ => Not(cond)

  private def ite(cond: Expr, thenValue: Expr, elseValue: Expr): Expr = (cond, thenValue, elseValue) match
    case (Const(true), e1, _) => e1 // if true then e1 else e2 = e1
    case (Const(false), _, e2) => e2 // if false then e1 else e2 = e2
    case _ => Ite(cond, thenValue, elseValue)

  private def implies(left: Expr, right: Expr): Expr = (left, right) match
    case (Const(true), b) => b // true ⇒ b = b
    case (Const(false), _) => Const(true) // false ⇒ b = true
    case (_, Const(true)) => Const(true) // b ⇒ true = true
    case (b, Const(false)) => Not(b) // b ⇒ false = ¬b
    case _ => Implies(left, right)

  private def rel(op: CmpOp, left: Expr, right: Expr): Expr = (left, right) match
    case (Const(n1: Int), Const(n2: Int)) => // constant propagation
      val result = op match
        case EQ => n1 == n2
        case NE => n1 != n2
        case LT => n1 < n2
        case LE => n1 <= n2
        case GT => n1 > n2
        case GE => n1 >= n2
      Const(result)
    case _ => RelExpr(op, left, right)

  private def negate(value: Expr): Expr = value match
    case Const(0) => Const(0) // -0 = 0
    case Const(n: Int) => Const(-n) // constant propagation
    case Negate(e) => e // -(-e) = e
    case _ => Negate(value)

  private def add(left: Expr, right: Expr): Expr = (left, right) match
    case (Const(n1: Int), Const(n2: Int)) => Const(n1 + n2) // constant propagation
    case (Const(0), e) => e // 0 + e = e
    case (e, Const(0)) => e // e + 0 = e
    case (e1, Negate(e2)) => sub(e1, e2) // e1 + (-e2) = e1 - e2
    case (Negate(e1), e2) => sub(e2, e1) // (-e1) + e2 = e2 - e1
    case _ => Add(left, right)

  private def sub(left: Expr, right: Expr): Expr = (left, right) match
    case (Const(n1: Int), Const(n2: Int)) => Const(n1 - n2) // constant propagation
    case (e, Const(0)) => e // e - 0 = e
    case (Const(0), e) => negate(e) // 0 - e = -e
    case (e1, Negate(e2)) => add(e1, e2) // e1 - (-e2) = e1 + e2
    case _ => Sub(left, right)

  extension (expr: Expr)
    def simplify: Expr =
      expr.transform:
        case And(bs) => and(bs.map(_.simplify))
        case Or(bs) => or(bs.map(_.simplify))
        case Not(b) => not(b.simplify)
        case Implies(b1, b2) => implies(b1.simplify, b2.simplify)
        case Ite(b, e1, e2) => ite(b.simplify, e1.simplify, e2.simplify)
        case RelExpr(op, e1, e2) => rel(op, e1.simplify, e2.simplify)
        case Negate(e) => negate(e.simplify)
        case Add(e1, e2) => add(e1.simplify, e2.simplify)
        case Sub(e1, e2) => sub(e1.simplify, e2.simplify)
