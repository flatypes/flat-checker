package flat.checker

import flat.checker.ExprOps.summands
import flat.checker.ast.*

import scala.Function.unlift
import scala.collection.mutable.ListBuffer

object Analyzer:

  import ArithOp.*
  import CmpOp.*

  def getModifiedVars(stmt: Stmt): Set[String] =
    stmt match
      case Assign(x, _) => Set(x)
      case IfStmt(_, b1, b2) => b1.flatMap(getModifiedVars).toSet | b2.flatMap(getModifiedVars).toSet
      case While(_, b, _) => b.flatMap(getModifiedVars).toSet
      case _ => Set.empty

  def extractCmpExprs(cond: Expr): List[Cmp] =
    cond match
      case And(e1, e2) => extractCmpExprs(e1) ++ extractCmpExprs(e2)
      case Not(e) => for cmp <- extractCmpExprs(e) yield cmp.copy(op = negateCmpOp(cmp.op))
      case cmp: Cmp => List(cmp)
      case _ => Nil

  def negateCmpOp(op: CmpOp): CmpOp =
    op match
      case EQ => NE
      case NE => EQ
      case LE => GT
      case LT => GE
      case GE => LT
      case GT => LE

  def reverseCmpOp(op: CmpOp): CmpOp =
    op match
      case EQ => EQ
      case NE => NE
      case LE => GE
      case LT => GT
      case GE => LE
      case GT => LT

  def guessLoopInv(whileStmt: While, block: List[Stmt]): List[Expr] =
    val cmpExprs = extractCmpExprs(whileStmt.cond)
    val invBuf = ListBuffer.empty[Expr]
    for
      i <- getModifiedVars(whileStmt)
      ds = whileStmt.body.map(accumulateDelta(i, _))
      if ds.forall(_.isDefined)
      delta = ds.map(_.get).sum
      if delta != 0
      expectedOp = if delta > 0 then LE else GE
      c1 <- cmpExprs.collectFirst(unlift { cmp =>
        solveInequality(cmp, i) match
          case Some(op, r) if op == expectedOp => Some(r)
          case _ => None
      })
      c2 <- getMostRecentValue(i, block, whileStmt)
    do
      invBuf += And(Cmp(reverseCmpOp(expectedOp), Var(i), c2),
        Cmp(expectedOp, Var(i), simplifyArith(ADD(c1, delta))))
    invBuf.toList

  private def simplifyArith(expr: Expr): Expr =
    val (consts, vars) = expr.summands.partition {
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

  private def add(exprs: List[Expr]): Expr = exprs match
    case Nil => 0
    case e :: Nil => e
    case e :: es => es.foldLeft(e) {
      case (acc, Negate(e)) => SUB(acc, e)
      case (acc, e) => ADD(acc, e)
    }

  private def accumulateDelta(i: String, stmt: Stmt): Option[Int] =
    stmt match
      case Assign(x, e) if x == i =>
        e match
          case Arith(op, Var(y), Const(c: Int)) if y == i => // i = i +- c
            Some(if op == ADD then c else -c)
          case _ => None
      case _ =>
        if getModifiedVars(stmt).contains(i) then None else Some(0)

  private def getMostRecentValue(x: String, block: List[Stmt], untilStmt: Stmt): Option[Expr] =
    val i = block.indexOf(untilStmt)
    if i == -1 then return None
    val j = block.take(i).lastIndexWhere {
      case Assign(y, _) if y == x => true
      case _ => false
    }
    if j == -1 then return None
    val candidate = block(j).asInstanceOf[Assign].value
    val xs = candidate.collectVars
    val ys = block.slice(j, i).flatMap(getModifiedVars).toSet
    if (xs & ys).isEmpty then Some(candidate) else None

  private type ArithTerm = (Boolean, Expr)

  extension (t: ArithTerm)
    inline def unary_! : ArithTerm = (!t._1, t._2)

  /** Split e into e1 + e2 + ... */
  private def flattenAdd(expr: Expr): List[ArithTerm] =
    expr match
      case Arith(ADD, e1, e2) => flattenAdd(e1) ++ flattenAdd(e2)
      case Arith(SUB, e1, e2) => flattenAdd(e1) ++ flattenAdd(e2).map(!_)
      case _ => List((true, expr))

  private def solveInequality(cmp: Cmp, x: String): Option[(LE.type | GE.type, Expr)] =
    if cmp.op == EQ || cmp.op == NE then return None
    val (leftXs, leftOthers) = flattenAdd(cmp.left).partition(_._2 == Var(x))
    val (rightXs, rightOthers) = flattenAdd(cmp.right).partition(_._2 == Var(x))
    if leftXs.length + rightXs.length != 1 then return None
    // push `x` to the left
    val xTerm = if leftXs.length == 1 then leftXs.head else !rightXs.head
    // push anything other than `x` on the left to the right
    var rightTerms = rightOthers ++ leftOthers.map(!_)
    var op = cmp.op
    // if `x` is negative, both sides *(-1)
    if !xTerm._1 then
      op = negateCmpOp(op)
      rightTerms = rightTerms.map(!_)
    val right = rightTerms.foldLeft(0: Expr) { case (v, (b, e)) => Arith(if b then ADD else SUB, v, e) }
    op match
      case LE => Some(LE, right)
      case GE => Some(GE, right)
      case LT => Some(LE, Arith(SUB, right, 1))
      case GT => Some(GE, Arith(ADD, right, 1))
      case _ => assert(false)