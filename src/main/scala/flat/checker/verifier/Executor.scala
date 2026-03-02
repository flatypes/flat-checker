package flat.checker.verifier

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.checker.ast.ExprSimplifier.simplify
import flat.checker.ast.Printer.{FreshInfo, ppExpr}
import flat.checker.{Analyzer, VarCtx, Verifier}
import flat.regex.CharSet
import flat.{Config, Issuer}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

enum AssertKind:
  case AssertIndex
  case AssertPre
  case AssertPost
  case AssertInvOnEntry
  case AssertInvOnExit
  case AssertUser

import flat.checker.verifier.AssertKind.*

final case class State(funDef: FunDef,
                       values: Map[String, Expr] = Map.empty,
                       freshSorts: Map[String, Sort] = Map.empty,
                       assumptions: List[Expr] = Nil,
                       // debugging info
                       freshInfo: FreshInfo = Map.empty)

class Executor(using config: Config, issuer: Issuer) extends LazyLogging:
  private val goalCounter = new AtomicInteger

  import Owner.*

  private val verifier = new Verifier

  def exec(funDef: FunDef): Unit =
    Analyzer.guessInvariants(funDef.body)
    goalCounter.set(0)
    val state = State(funDef)
    if checkSide(funDef.requires, state) then
      run(List(CodeItem(funDef.body, FUN)), execAssume(funDef.requires, state))

  private def run(stack: List[CodeItem], state: State): Unit = stack match
    case Nil => // final state
    case item :: items =>
      item.current match
        case Some(Assign(x, e)) =>
          if checkSide(e, state) then
            run(item.next :: items, execAssign(x, e, state))
        case Some(Havoc(xs)) =>
          run(item.next :: items, execHavoc(xs, state))
        case Some(Assume(e)) =>
          if checkSide(e, state) then
            run(item.next :: items, execAssume(e, state))
        case Some(Assert(e)) =>
          if checkSide(e, state) then
            execAssert(e, AssertUser, state)
            run(item.next :: items, state)
        case Some(IfStmt(e, b1, b2)) =>
          if checkSide(e, state) then
            run(CodeItem(b1, IF) :: stack, execAssume(e, state))
            run(CodeItem(b2, IF) :: stack, execAssume(Not(e), state))
        case Some(stmt@While(e, b)) =>
          val inv = mkAnd(stmt.invariants.toList)
          if checkSide(inv, state) then
            execAssert(inv, AssertInvOnEntry, state)
            val havocState = execHavoc(Analyzer.collectModifiedVars(b).toList, state)
            if checkSide(e, havocState) && checkSide(inv, havocState) then
              val invState = execAssume(inv, havocState)
              run(CodeItem(b, WHILE) :: stack, execAssume(e, invState))
              run(item.next :: items, execAssume(Not(e), invState))
        case Some(Break()) =>
          val k = stack.indexWhere(_.owner == WHILE)
          assert(k >= 0, "break outside of a while loop")
          stack.drop(k + 1) match
            case Nil => throw IllegalStateException()
            case item :: items => run(item.next :: items, state)
        case Some(Continue()) =>
          val k = stack.indexWhere(_.owner == WHILE)
          assert(k >= 0, "continue outside of a while loop")
          val inv = mkAnd(stack.drop(k + 1).head.current.get.asInstanceOf[While].invariants.toList)
          execAssert(inv, AssertInvOnExit, state)
        case Some(Return()) =>
          execAssert(state.funDef.ensures, AssertPost, state)
        case Some(s) => throw IllegalStateException(s"unexpected statement ${s.getClass}")
        case None => // end of body
          item.owner match
            case FUN =>
              execAssert(state.funDef.ensures, AssertPost, state)
            case IF => // continue after the if-statement
              stack.tail match
                case Nil => throw IllegalStateException()
                case item :: items => run(item.next :: items, state)
            case WHILE =>
              val inv = mkAnd(stack.tail.head.current.get.asInstanceOf[While].invariants.toList)
              execAssert(inv, AssertInvOnExit, state)

  private def execAssign(x: String, e: Expr, state: State): State =
    val v = evalAndSimplify(e, state)
    state.copy(values = state.values + (x -> v))

  private def execHavoc(xs: List[String], state: State): State =
    val freshVars = for _ <- xs yield FreshVar.create()
    val versions = for x <- xs yield state.freshInfo.values.filter(_._1 == x).map(_._2).maxOption.getOrElse(0)
    state.copy(values = state.values ++ (for (x, v) <- xs zip freshVars yield x -> v),
      freshSorts = state.freshSorts ++ (for (x, Var(y)) <- xs zip freshVars yield y -> state.funDef.assignable(x)),
      freshInfo = state.freshInfo ++ (for ((x, Var(y)), k) <- xs zip freshVars zip versions yield y -> (x, k + 1)))

  private def execAssume(e: Expr, state: State): State =
    evalAndSimplify(e, state) match
      case And(vs) => state.copy(assumptions = state.assumptions ++ vs)
      case v => state.copy(assumptions = state.assumptions :+ v)

  private def execAssert(e: Expr, kind: AssertKind, state: State): Boolean =
    val conclusion = evalAndSimplify(e, state)
    if conclusion == Const(true) then
      return true

    val id = goalCounter.incrementAndGet()
    printGoal(e, kind, state)
    val sortingCtx = Map.from(for Decl(x, s) <- state.funDef.params yield x -> s) ++ state.freshSorts
    val prover = verifier.Prover(using VarCtx(sortingCtx), SortingContext(sortingCtx))
    for e <- state.assumptions do
      prover.assume(e)
    prover.prove(conclusion) match
      case Left(err) => throw RuntimeException("verification failed: " + err)
      case Right(_) => true

  private def checkSide(expr: Expr, state: State): Boolean = expr match
    case And(es) =>
      var ok = checkSide(es.head, state)
      var state1 = state
      var i = 1
      while ok && i < es.length do
        state1 = execAssume(es(i - 1), state1)
        ok = checkSide(es(i), state1)
        i += 1
      ok
    case Implies(e1, e2) =>
      checkSide(e1, state) && checkSide(e2, execAssume(e1, state))
    case Ite(e, e1, e2) =>
      checkSide(e, state) && checkSide(e1, execAssume(e, state)) && checkSide(e2, execAssume(Not(e), state))

    case CharFromCode(e) => // 0 <= e <= 0xFFFF
      checkSide(e, state) &&
        execAssert(And(LE(Const(0), e), LE(e, Const(0xFFFF))), AssertPre, state)
    case CharAt(e, ei) => // 0 <= ei < |e|
      checkSide(e, state) && checkSide(ei, state) &&
        execAssert(And(LE(Const(0), ei), LT(ei, StringLength(e))), AssertIndex, state)
    case Substring(es, ei, ej) => // 0 <= ei and 0 <= ej
      checkSide(es, state) && checkSide(ei, state) && checkSide(ej, state) &&
        execAssert(And(LE(Const(0), ei), LE(Const(0), ej)), AssertIndex, state)
    case StringToInt(e, base) => // e is a string representing an integer in the given base
      checkSide(e, state) &&
        execAssert(StringForall(e, Lambda(List(Decl("c", CharSort)), RefinedBy(Var("c"), CharSet.intDigit(base)))),
          AssertPre, state)

    case SeqGet(e, ei) => // 0 <= ei < |e|
      checkSide(e, state) && checkSide(ei, state) &&
        execAssert(And(LE(Const(0), ei), LT(ei, SeqLength(e))), AssertIndex, state)
    case SeqSlice(es, ei, ej) => // 0 <= ei and 0 <= ej
      checkSide(es, state) && checkSide(ei, state) && checkSide(ej, state) &&
        execAssert(And(LE(Const(0), ei), LE(Const(0), ej)), AssertIndex, state)

    case MapGet(e, ek) => // e contains key ek
      checkSide(e, state) && checkSide(ek, state) && execAssert(MapContains(e, ek), AssertPre, state)
    case MapRemove(e, ek) => // e contains key ek
      checkSide(e, state) && checkSide(ek, state) && execAssert(MapContains(e, ek), AssertPre, state)

    case _ =>
      expr.productIterator.forall:
        case e: Expr => checkSide(e, state)
        case _ => true

  private def evalAndSimplify(expr: Expr, state: State): Expr =
    val v = expr.transform { case Var(x) if state.values.contains(x) => state.values(x) }
    v.simplify

  private def printGoal(expr: Expr, kind: AssertKind, state: State): Unit =
    logger.whenInfoEnabled:
      logger.info("Goal {}: {}", goalCounter.get(), kind)
      val m = Map.from(for (x, v) <- state.values yield v -> x)
      val xs = mutable.Set.empty[String]
      for assumption <- state.assumptions do
        val e = assumption.transform[Expr] { case v: Expr if m.contains(v) => xs += m(v); Var(m(v)) }
        logger.info("  {}", ppExpr(e)(using state.freshInfo))
      xs ++= expr.collectVars.filter(state.values.contains)
      logger.info("  ⇒ {}", ppExpr(expr)(using state.freshInfo))
      if xs.nonEmpty then
        logger.info("where")
      for x <- xs do
        logger.info("  {} = {}", x, ppExpr(state.values(x))(using state.freshInfo))

  private enum Owner:
    case FUN, IF, WHILE

  private case class CodeItem(body: List[Stmt], owner: Owner, pc: Int = 0):
    def current: Option[Stmt] = body.lift(pc)

    def next: CodeItem = copy(pc = pc + 1)
