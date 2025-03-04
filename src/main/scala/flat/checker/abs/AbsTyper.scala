package flat.checker.abs

import flat.checker
import flat.checker.abs.analysis.{AbsPos, RelPos}
import flat.checker.ast.*
import flat.checker.{Issuer, ast}

import scala.collection.mutable

class AbsTyper:
  given issuer: Issuer = new Issuer

  private val globalUbs = mutable.Map.empty[String, AVal]

  def process(script: Seq[FunDef]): Unit =
    for FunDef(Ident(f), xs, t, _) <- script do
      globalUbs(f) = AVal.fromType(FunType(xs.map(_._2), t))
    for FunDef(_, xs, t, s) <- script do
      val state = State.from(xs :+ ("return" -> t))
      s.accept(Executor, state)

  private object Executor extends NodeVisitor[State, State]:
    override def visitDeclare(node: Declare, state: State): State =
      state.added(node.localName, node.typ)

    override def visitAssign(node: Assign, state: State): State =
      val newVal = node.value.accept(Evaluator, state)
      node.target match
        case Some(x) => state.updated(x, newVal, node.value.loc)
        case None => state

    override def visitAssert(node: Assert, state: State): State =
      val booleanVal = node.cond.accept(Evaluator, state).asInstanceOf[ABool]
      if booleanVal.contains(false) then
        issuer.report(new AssertionError(node.cond.loc))
      state

    override def visitReturn(node: Return, state: State): State =
      val returnVal = node.value.accept(Evaluator, state)
      state.updated("return", returnVal, node.value.loc)

    override def visitIfStmt(node: IfStmt, state: State): State =
      val booleanVal = node.cond.accept(Evaluator, state).asInstanceOf[ABool]
      val state1 = if booleanVal.contains(true) then node.body.accept(this, state) else state
      val state2 = if booleanVal.contains(false) then node.elseBody.accept(this, state) else state
      state1 | state2

    override def visitWhile(node: While, state: State): State =
      def loop(f: State => State)(st: State): State =
        val booleanVal = node.cond.accept(Evaluator, st).asInstanceOf[ABool]
        if booleanVal.contains(true) then f(node.body.accept(this, st)) else st

      kleene(loop)(state)

    override def visitStmtBlock(node: StmtBlock, state: State): State =
      val finalState = node.body.foldLeft(state.push) { (st, s) => s.accept(this, st) }
      finalState.pop

  private object Evaluator extends NodeVisitor[State, AVal]:
    override def visitLiteral(node: Literal, state: State): AVal =
      node.value match
        case n: Int => AInt.fromRange(Range.fromInt(n))
        case b: Boolean => ABool.fromBoolean(b)
        case c: Char => ???
        case s: String => ReLang.fromString(s)

    override def visitGlobalRef(node: GlobalRef, state: State): AVal = globalUbs(node.name)

    override def visitLocalRef(node: LocalRef, state: State): AVal = state(node.localName)

    override def visitApply(node: Apply, state: State): AVal =
      node.fun match
        case op: Op =>
          val vs = node.args.map(_.accept(this, state))
          op.accept(OpEvaluator, vs)
        case e: Expr =>
          val AFun(argUbs, returnUb) = e.accept(this, state).asInstanceOf[AFun]
          val vs = node.args.map(_.accept(this, state))
          for
            (ub, arg) <- argUbs zip node.args
            v = arg.accept(this, state)
            if !(v :<: ub)
          do issuer.report(TypeMismatch(arg.loc, ub.show, v.show))
          returnUb

  private object OpEvaluator extends OpVisitor[Seq[AVal], AVal]:
    override def visitEqual(input: Seq[AVal]): AVal = ???

    override def visitNotEqual(input: Seq[AVal]): AVal = ???

    override def visitAdd(input: Seq[AVal]): AVal = input match
      case Seq(AInt(r1, Some(cnf, k)), AInt(r2, _)) if r2.isInt =>
        analysis.rightShift(RelPos(cnf, k), r2.asInt) match
          case Left(err) => throw RuntimeException(err)
          case Right(RelPos(cnf, k)) => AInt.fromIndex(cnf, k)
      case Seq(AInt(r1, _), AInt(r2, _)) => AInt.fromRange(r1 + r2)
      case _ => throw UnsupportedOperationException()

    override def visitSub(input: Seq[AVal]): AVal = input match
      case Seq(AInt(r1, _), AInt(r2, _)) => AInt.fromRange(r1 - r2)
      case _ => throw UnsupportedOperationException()

    override def visitLessEqual(input: Seq[AVal]): AVal = ???

    override def visitLessThan(input: Seq[AVal]): AVal = ???

    override def visitGreaterEqual(input: Seq[AVal]): AVal = ???

    override def visitGreaterThan(input: Seq[AVal]): AVal = ???

    override def visitAnd(input: Seq[AVal]): AVal = input match
      case Seq(b1: ABool, b2: ABool) => b1 && b2
      case _ => assert(false)

    override def visitOr(input: Seq[AVal]): AVal = input match
      case Seq(b1: ABool, b2: ABool) => b1 || b2
      case _ => assert(false)

    override def visitNot(input: Seq[AVal]): AVal = input match
      case Seq(b: ABool) => !b
      case _ => assert(false)

    override def visitCharToCode(input: Seq[AVal]): AVal = ???

    override def visitCharFromCode(input: Seq[AVal]): AVal = ???

    override def visitStringConcat(input: Seq[AVal]): AVal = ???

    override def visitStringReverse(input: Seq[AVal]): AVal = input match
      case Seq(r: ReLang) => ???

    override def visitStringLength(input: Seq[AVal]): AVal = input match
      case Seq(r: ReLang) => AInt.fromRange(analysis.measureLength(r))
      case _ => assert(false)

    override def visitStringAt(input: Seq[AVal]): AVal = ???

    override def visitSubstring(input: Seq[AVal]): AVal = input match
      case Seq(l: ReLang, AInt(_, Some(l1, k1)), AInt(_, Some(l2, k2))) if l1 == l.toCNF && l2 == l.toCNF =>
        ReLang.fromCNF(l.toCNF.slice(k1, k2))
      case Seq(l, i, j) => throw UnsupportedOperationException(s"SUBSTR($l, $i, $j)")
      case _ => assert(false)

    override def visitStringIndexOf(input: Seq[AVal]): AVal = input match
      case Seq(l: ReLang, t: ReLang, i: AInt) if t.isChar =>
        val pos = i match
          case AInt(_, Some(l1, k)) => RelPos(l1, k)
          case AInt(r, None) => AbsPos(r.asInt)
        analysis.find(l.toCNF, t.asChar, pos) match
          case Left(err) => throw RuntimeException(err)
          case Right(RelPos(cnf, k)) => AInt.fromIndex(cnf, k)
      case Seq(l: ReLang, t: ReLang, i: AInt) => throw UnsupportedOperationException()
      case _ => assert(false)

    override def visitStringSplit(input: Seq[AVal]): AVal = ???

    override def visitStringStartsWith(input: Seq[AVal]): AVal = ???

    override def visitStringEndsWith(input: Seq[AVal]): AVal = ???

    override def visitStringContains(input: Seq[AVal]): AVal = ???

    override def visitStringToInt(input: Seq[AVal]): AVal = ???

    override def visitStringFromInt(input: Seq[AVal]): AVal = ???

    override def visitNewArray(input: Seq[AVal]): AVal = ???

    override def visitArrayAt(input: Seq[AVal]): AVal = ???

    override def visitArrayUpdate(input: Seq[AVal]): AVal = ???

    override def visitArrayContains(input: Seq[AVal]): AVal = ???

    override def visitArrayForallTrue(input: Seq[AVal]): AVal = ???

    override def visitArrayExistsTrue(input: Seq[AVal]): AVal = ???
