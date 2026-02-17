package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*
import flat.checker.ast.Printer.*

object Analyzer extends LazyLogging:
  /** An execution ''trace'' is a list of statements. */
  private type Trace = List[Stmt]

  /** Collects traces from executing the given `stmts`. */
  private def collectTraces(stmts: List[Stmt]): List[Trace] = stmts match
    case Nil => List(Nil)
    case IfStmt(b, thenBody, elseBody) :: ss =>
      collectTraces(thenBody ++ ss).map(Assume(b) :: _) ++ collectTraces(elseBody ++ ss).map(Assume(Not(b)) :: _)
    case s@(Break() | Continue() | Return()) :: _ => List(s)
    case s :: ss => collectTraces(ss).map(s :: _)

  /** Symbolic Value */
  private enum Value:
    case Abs(value: Expr)
    case Rel(delta: Int)
    case Unknown

    def +(k: Int): Value = this match
      case Abs(e) => Abs(mkAdd(e, k))
      case Rel(d) => Rel(d + k)
      case Unknown => Unknown

    def |(that: Value): Value = if this == that then this else Unknown

  import Value.*

  private def computeValue(name: String, trace: Trace): Value =
    var value = Rel(0)
    trace.foreach:
      case Assign(x, e) if x == name =>
        e match
          case Arith(op, Var(y), Const(k: Int)) if y == x =>
            value += (if op == ADD then k else -k)
          case _ =>
            value = if e.collectVars.contains(x) then Unknown else Abs(e)
      case While(_, body) if collectModifiedVars(body).contains(name) =>
        value = Unknown
      case _ =>
    value

  /** Collects all variables that are modified (as lhs of `Assign`) inside the given `stmt`. */
  def collectModifiedVars(body: List[Stmt]): Set[String] = body match
    case Nil => Set.empty
    case Assign(x, _) :: rest => Set(x) | collectModifiedVars(rest)
    case IfStmt(_, s1, s2) :: rest => collectModifiedVars(s1) | collectModifiedVars(s2) | collectModifiedVars(rest)
    case While(_, body) :: rest => collectModifiedVars(body) | collectModifiedVars(rest)
    case _ :: rest => collectModifiedVars(rest)

  /** Guesses naive loop invariants where user invariants are not provided.
   *
   * Applies to loops `while x op e do ...` where the loop variable `x` has a constant delta `k ≠ 0` in each iteration.
   * Let `e0` be the initial value of `x` before entering the loop.
   * The guessed invariant is:
   *  - `e0 ≤ x < e + k` if `k > 0` and `op` is `<` (similar for `≤`);
   *  - `e + k < x ≤ e0` if `k < 0` and `op` is `>` (similar for `≥`).
   *
   * @param body the body of the program to analyze
   */
  def guessInvariants(body: List[Stmt]): Unit =
    for stmt <- body do
      stmt.traverse:
        case loop@While(Cmp(op, Var(x), e), s) if loop.invariants.isEmpty && op != EQ && op != NE &&
          (e.collectVars & collectModifiedVars(s)).isEmpty =>
          val initValues = for
            trace <- collectTraces(s)
            k = trace.indexOf(loop)
            if k >= 0
          yield computeValue(x, trace.take(k))
          initValues.reduce(_ | _) match
            case Abs(e0) =>
              val finalValues = collectTraces(s).map(computeValue(x, _))
              (finalValues.reduce(_ | _), op) match
                case (Rel(k), LT | LE) if k > 0 =>
                  val inv = And(LE(e0, Var(x)(IntSort)), op(Var(x)(IntSort), mkAdd(e, k)))
                  logger.debug("Guessed invariant: {}", ppExpr(inv))
                  loop.invariants += inv.setLocation(loop.cond.loc)
                case (Rel(k), GT | GE) if k < 0 =>
                  val inv = And(op.reverse(mkAdd(e, k), Var(x)(IntSort)), LE(Var(x)(IntSort), e0))
                  logger.debug("Guessed invariant: {}", ppExpr(inv))
                  loop.invariants += inv.setLocation(loop.cond.loc)
                case _ =>
            case _ =>
        case _ =>

  /** Collects the `break`-conditions in the given loop `body`. */
  def collectBreakConds(body: List[Stmt]): List[Expr] =
    for
      trace <- collectTraces(body)
      k = trace.indexOf(Break())
      if k >= 0
    yield collectExitCond(trace.take(k))

  private def collectExitCond(trace: Trace): Expr =
    mkAnd:
      trace.reverse
        .takeWhile:
          case Assume(_) | Assert(_) => true
          case _ => false
        .map:
          case Assume(b) => b
          case Assert(b) => b
          case _ => assert(false)
