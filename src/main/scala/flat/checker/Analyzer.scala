package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.Printer.{ppExpr, ppStmt}
import flat.checker.ast.*
import flat.checker.ast.ArithOp.*

object Analyzer extends LazyLogging:
  /** Symbolic Value */
  enum Value:
    case Abs(value: Expr)
    case Rel(delta: Int)
    case Unknown

    def +(k: Int): Value = this match
      case Abs(e) => Abs(mkAdd(e, k))
      case Rel(d) => Rel(d + k)
      case Unknown => Unknown

    def |(that: Value): Value = if this == that then this else Unknown

  import Value.*

  /** A ''state'' is a map from variables to symbolic values. */
  type State = Map[String, Value]

  /** Executes (symbolically) the given `stmt` under the initial `state`.
   *
   * Restrictions:
   *  - Only integer variables are supported.
   *  - While-loops are handled by killing all modified variables.
   */
  def execute(stmt: Stmt, state: State)(using trace: (Stmt, State) => Unit): State =
    trace(stmt, state)
    stmt match
      case SeqStmt(s1, s2) =>
        val state1 = execute(s1, state)
        execute(s2, state1)
      case Assign(x, e) if state.contains(x) =>
        e match
          case Arith(op, Var(y), Const(k: Int)) if y == x =>
            val v = state(x) + (if op == ADD then k else -k)
            state + (x -> v)
          case _ =>
            val v = if e.collectVars.contains(x) then Unknown else Abs(e)
            state + (x -> v)
      case IfStmt(b, s1, s2) =>
        val state1 = execute(s1, state)
        val state2 = execute(s2, state)
        Map.from(for x <- state1.keys yield x -> (state1(x) | state2(x)))
      case While(_, s) =>
        val modified = collectModifiedVars(s) & state.keySet
        state ++ (for x <- modified yield x -> Unknown)
      case _ => state

  /** Collects all variables that are modified (as lhs of `Assign`) inside the given `stmt`. */
  def collectModifiedVars(stmt: Stmt): Set[String] = stmt match
    case SeqStmt(s1, s2) => collectModifiedVars(s1) | collectModifiedVars(s2)
    case Assign(x, _) => Set(x)
    case IfStmt(_, s1, s2) => collectModifiedVars(s1) | collectModifiedVars(s2)
    case While(_, s) => collectModifiedVars(s)
    case _ => Set.empty

  /** Guesses naive loop invariants where user invariants are not provided.
   *
   * Applies to loops `while x op e do ...` where the loop variable `x` has a constant delta `k ≠ 0` in each iteration.
   * Let `e0` be the initial value of `x` before entering the loop.
   * The guessed invariant is:
   *  - `e0 ≤ x < e + k` if `k > 0` and `op` is `<` (similar for `≤`);
   *  - `e + k < x ≤ e0` if `k < 0` and `op` is `>` (similar for `≥`).
   *
   * @param body  the body of the program to analyze
   * @param state the initial state
   */
  def guessInvariants(body: Stmt, state: State): Unit = execute(body, state)(using guess)

  private def guess(stmt: Stmt, state: State): Unit = stmt match
    case loop@While(Cmp(op, Var(x), e), s) if loop.invariants.isEmpty && op != EQ && op != NE &&
      (e.collectVars & collectModifiedVars(s)).isEmpty =>
      state(x) match
        case Abs(e0) =>
          val finalState = execute(s, Map(x -> Rel(0)))(using (_, _) => {})
          (finalState(x), op) match
            case (Rel(k), LT | LE) if k > 0 =>
              val inv = And(LE(e0, Var(x)), op(Var(x), mkAdd(e, k)))
              logger.debug("Guessed invariant: {}", ppExpr(inv))
              loop.invariants += inv.setLocation(loop.cond.loc)
            case (Rel(k), GT | GE) if k < 0 =>
              val inv = And(op.reverse(mkAdd(e, k), Var(x)), LE(Var(x), e0))
              logger.debug("Guessed invariant: {}", ppExpr(inv))
              loop.invariants += inv.setLocation(loop.cond.loc)
            case _ =>
        case _ =>
    case _ =>

  /** Collects the `break`-conditions in the given loop `body`. */
  def collectBreakConds(body: Stmt): List[Expr] =
    body.toBlock.flatMap:
      case IfStmt(b, Break(), Skip()) => List(b)
      case IfStmt(_, s1, s2) => collectBreakAssert(s1.toBlock) ++ collectBreakAssert(s2.toBlock)
      case Break() => throw IllegalStateException("loop body contains non-conditional break: " + ppStmt(body))
      case _ => Nil

  /** Collects `assert`-conditions right before `break`-statements in the given `block`. */
  private def collectBreakAssert(block: List[Stmt]): List[Expr] =
    block.flatMap:
      case IfStmt(_, s1, s2) => collectBreakAssert(s1.toBlock) ++ collectBreakAssert(s2.toBlock)
      case brk@Break() =>
        val i = block.indexOf(brk)
        if i - 1 >= 0 && block(i - 1).isInstanceOf[Assert] then
          val b = block(i - 1).asInstanceOf[Assert].cond
          List(b)
        else
          logger.warn("cannot collect break-conditions for statement {} of the block {}", i, ppStmt(mkStmtList(block)))
          List(true)
      case _ => Nil