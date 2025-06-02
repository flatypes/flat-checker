package flat.checker

import flat.checker.core.*

object Rewriter:
  def strSlice(str: Expr, fromIndex: Expr): StrSlice = StrSlice(str, fromIndex, StrLen(str))

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

  def destructOrClassical(expr: Expr): List[Expr] =
    val es = destructOr(expr)
    List.from(for i <- es.indices yield mkAnd(es.take(i).map(Not.apply) :+ es(i)))

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

  def push(variable: Expr, cond: Cmp): Option[(CmpOp, Expr)] =
    val allTerms = toANF(cond.left) ++ toANF(cond.right).map(negate)
    val (varTerms, otherTerms) = allTerms.partition {
      case e if e == variable => true
      case Negate(e) if e == variable => true
      case _ => false
    }
    varTerms match
      case Negate(_) :: Nil => Some(reverse(cond.op) -> add(otherTerms))
      case _ :: Nil => Some(cond.op -> add(otherTerms.map(negate)))
      case _ => None

  def forallIntConst(exprs: List[Expr]): Option[List[Int]] = exprs match
    case Nil => Some(Nil)
    case Const(n: Int) :: es => forallIntConst(es).map(n :: _)
    case _ => None

  def tryGetConstDiff(expr: Expr, baseExpr: Expr): Option[Int] =
    val (baseTerms, otherTerms) = toANF(expr).partition(_ == baseExpr)
    for
      _ <- Some(())
      if baseTerms.length == 1
      ns <- forallIntConst(otherTerms)
    yield ns.sum

  def destructIte(expr: Expr, cond: Expr): Option[(Expr, Expr)] =
    val e1 = expr.transform {
      case Ite(e, e1, _) if e == cond => e1
    }
    val e2 = expr.transform {
      case Ite(e, _, e2) if e == cond => e2
    }
    if e1 != expr then Some((e1, e2)) else None
