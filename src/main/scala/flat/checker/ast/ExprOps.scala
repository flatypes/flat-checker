package flat.checker.ast

import flat.checker.ast.*

import scala.collection.mutable.ListBuffer

object ExprOps:
  extension (expr: Expr)
    /** Returns all conjuncts. */
    def getConjuncts: List[Expr] =
      expr match
        case Const(true) => Nil
        case And(es) => es.flatMap(_.getConjuncts)
        case _ => List(expr)

    /** Returns all disjuncts. */
    def getDisjuncts: List[Expr] =
      expr match
        case Const(false) => Nil
        case Or(es) => es.flatMap(_.getDisjuncts)
        case _ => List(expr)

    // Arithmetic

    /** Returns the negation `-expr`. */
    def negation: Expr = expr match
      case Const(n: Int) => Const(-n)
      case Negate(e) => e
      case _ => Negate(expr)

    /** Returns all summands. */
    def summands: List[Expr] = getSummands match
      case Nil => List(Const(0))
      case es => es

    private def getSummands: List[Expr] = expr match
      case Const(0) => Nil
      case Add(e1, e2) => e1.summands ++ e2.summands
      case Sub(e1, e2) => e1.summands ++ e2.summands.map(_.negation)
      case _ => List(expr)

    /** Returns the difference `expr - base` if it is a nonnegative constant. */
    def diffNonneg(base: Expr): Option[Int] =
      var unexpected = false
      var baseCount = 0
      var sum = 0
      expr.summands.foreach:
        case Const(n: Int) => sum += n
        case e => if e == base then baseCount += 1 else unexpected = true
      if !unexpected && baseCount == 1 && sum >= 0 then Some(sum) else None

  def mkSum(summands: List[Expr]): Expr =
    val pos = ListBuffer.empty[Expr]
    val neg = ListBuffer.empty[Expr]
    var k = 0
    summands.foreach:
      case Const(n: Int) => k += n
      case Negate(e) => neg += e
      case e => pos += e
    val common = pos.toSet & neg.toSet
    for e <- common do
      pos -= e
      neg -= e
    if pos.nonEmpty then
      val e = pos.reduce(Add(_, _))
      val e1 = neg.foldLeft(e)(Sub(_, _))
      if k == 0 then e1 else if k > 0 then Add(e1, Const(k)) else Sub(e1, Const(-k))
    else if neg.nonEmpty then
      val e: Expr = Negate(neg.head)
      val e1 = neg.tail.foldLeft(e)(Sub(_, _))
      if k == 0 then e1 else if k > 0 then Add(e1, Const(k)) else Sub(e1, Const(-k))
    else
      Const(k)
