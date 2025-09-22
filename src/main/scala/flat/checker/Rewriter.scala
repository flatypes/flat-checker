package flat.checker

import flat.checker.core.*

object Rewriter:
  def simplifyCond(expr: Expr): Expr =
    expr match
      case And(e1, e2) => And(simplifyCond(e1), simplifyCond(e2))
      case Or(e1, e2) => Or(simplifyCond(e1), simplifyCond(e2))
      case Not(And(e1, e2)) => Or(simplifyCond(Not(e1)), simplifyCond(Not(e2)))
      case Not(Or(e1, e2)) => And(simplifyCond(Not(e1)), simplifyCond(Not(e2)))
      case Not(Not(e)) => simplifyCond(e)
      case Not(Cmp(op, e1, e2)) => Cmp(Analyzer.negateCmpOp(op), e1, e2)
      case Ite(e, e1, e2) => Ite(simplifyCond(e), e1, e2)
      case _ => expr

  def destructAnd(expr: Expr): List[Expr] =
    expr match
      case And(e1, e2) => destructAnd(e1) ++ destructAnd(e2)
      case e => List(e)

  def destructOr(expr: Expr): List[Expr] =
    expr match
      case Or(e1, e2) => destructOr(e1) ++ destructOr(e2)
      case e => List(e)

  import CmpOp.*

  def reverse(op: CmpOp): CmpOp = op match
    case EQ => EQ
    case NE => NE
    case LE => GE
    case LT => GT
    case GE => LE
    case GT => LT

  import ArithOp.*

  def negate(expr: Expr): Expr = expr match
    case Negate(e) => e
    case _ => Negate(expr)

  def add(exprs: List[Expr]): Expr = exprs match
    case Nil => 0
    case e :: Nil => e
    case e :: es => es.foldLeft(e) {
      case (acc, Negate(e)) => SUB(acc, e)
      case (acc, e) => ADD(acc, e)
    }

  def toANF(expr: Expr): List[Expr] =
    expr match
      case Arith(ADD, e1, e2) => toANF(e1) ++ toANF(e2)
      case Arith(SUB, e1, e2) => toANF(e1) ++ toANF(e2).map(negate)
      case _ => List(expr)

  def simplifyArith(expr: Expr): Expr =
    val (consts, vars) = toANF(expr).partition {
      case Const(_: Int) => true
      case Negate(Const(_: Int)) => true
      case _ => false
    }
    val k = consts.map {
      case Const(n: Int) => n
      case Negate(Const(n: Int)) => -n
      case _ => assert(false)
    }.sum
    val const = if k == 0 then Nil else if k > 0 then List(Const(k)) else List(Negate(Const(-k)))
    add(vars ++ const)
