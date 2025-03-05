package flat.checker.abs

import flat.checker
import flat.checker.Issuer
import flat.checker.ast.*

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
        case n: Int => Range.fromInt(n)
        case b: Boolean => ABool.fromBoolean(b)
        case s: String => ReLang.fromString(s)

    override def visitGlobalRef(node: GlobalRef, state: State): AVal = globalUbs(node.name)

    override def visitLocalRef(node: LocalRef, state: State): AVal = state(node.localName)

    override def visitApply(node: Apply, state: State): AVal =
      val AFun(argUbs, returnUb) = node.fun.accept(this, state).asInstanceOf[AFun]
      val vs = node.args.map(_.accept(this, state))
      for
        (ub, arg) <- argUbs zip node.args
        v = arg.accept(this, state)
        if !(v :<: ub)
      do issuer.report(TypeMismatch(arg.loc, ub.show, v.show))
      returnUb

    // library operations
    override def visitAdd(node: ApplyOp, state: State): AInt =
      assert(node.args.length == 2)
      val i1 = node.args.head.accept(this, state).asInstanceOf[AInt]
      val i2 = node.args(1).accept(this, state).asInstanceOf[AInt]
      val res = (i1, i2) match
        case (index: Index, range: Range) if range.isInt => CNFOps.shiftIndex(index, range.asInt).toOption
        case (range: Range, index: Index) if range.isInt => CNFOps.shiftIndex(index, range.asInt).toOption
        case _ => None
      res.getOrElse(i1.asRange + i2.asRange)

    override def visitAnd(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 2)
      val b1 = node.args.head.accept(this, state).asInstanceOf[ABool]
      val b2 = node.args(1).accept(this, state).asInstanceOf[ABool]
      b1 && b2

    override def visitOr(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 2)
      val b1 = node.args.head.accept(this, state).asInstanceOf[ABool]
      val b2 = node.args(1).accept(this, state).asInstanceOf[ABool]
      b1 || b2

    override def visitNot(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 1)
      val b = node.args.head.accept(this, state).asInstanceOf[ABool]
      !b

    override def visitCharToCode(node: ApplyOp, state: State): AInt =
      assert(node.args.length == 1)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val len = s.length
      if len.isInt && len.asInt == 1 then
        if s.isChar then s.asChar.toInt else Range.full
      else
        issuer.report(TypeMismatch(node.args.head.loc, "String of length 1", s"String of length $len"))
        Range.full

    override def visitCharFromCode(node: ApplyOp, state: State): AString =
      assert(node.args.length == 1)
      val range = node.args.head.accept(this, state).asInstanceOf[AInt].asRange
      if range.isInt then ReLang.fromChar(range.asInt.toChar)
      else ReLang.allChar

    override def visitStringConcat(node: ApplyOp, state: State): AString =
      assert(node.args.length == 2)
      val s1 = node.args.head.accept(this, state).asInstanceOf[AString]
      val s2 = node.args(1).accept(this, state).asInstanceOf[AString]
      ReLangOps.concat(s1, s2)

    override def visitStringReverse(node: ApplyOp, state: State): AString =
      assert(node.args.length == 1)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      s.reverse

    override def visitStringLength(node: ApplyOp, state: State): AInt =
      assert(node.args.length == 1)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      s.length

    override def visitStringAt(node: ApplyOp, state: State): AString =
      assert(node.args.length == 2)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val k = node.args(1).accept(this, state).asInstanceOf[AInt]
      val cnf = s.toCNF
      val range = k.asRange
      if range.isInt then
        ReLangOps.charAt(s, range.asInt) match
          case Some(cs) => ReLang.ReChars(cs)
          case None =>
            issuer.report(IndexOutOfBounds(node.args(1).loc))
            ReLang.ReChars(CharSet.full)
      else
        issuer.report(OverApprox(node.loc, "index is non-constant"))
        ReLang.ReChars(CharSet.full)

    override def visitSubstring(node: ApplyOp, state: State): AString =
      assert(node.args.length == 3)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val k1 = node.args(1).accept(this, state).asInstanceOf[AInt]
      val k2 = node.args(2).accept(this, state).asInstanceOf[AInt]
      val cnf = s.toCNF
      CNFOps.convertToRelative(cnf, k1) match
        case Right(fromPos) =>
          CNFOps.convertToRelative(cnf, k2) match
            case Right(untilPos) => CNFOps.substring(cnf, fromPos, untilPos)
            case Left(reason) =>
              issuer.report(OverApprox(node.loc, reason))
              ReLang.full
        case Left(reason) =>
          issuer.report(OverApprox(node.loc, reason))
          ReLang.full

    override def visitStringIndexOf(node: ApplyOp, state: State): AInt =
      assert(node.args.length == 3)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val t = node.args(1).accept(this, state).asInstanceOf[AString]
      val k = node.args(2).accept(this, state).asInstanceOf[AInt]
      if t.isChar then
        val cnf = s.toCNF
        CNFOps.convertToRelative(cnf, k) match
          case Left(reason) =>
            issuer.report(OverApprox(node.loc, reason))
            Range.full
          case Right(fromPos) =>
            CNFOps.indexOf(cnf, t.asChar, fromPos) match
              case Right(pos) => Index(cnf, pos)
              case Left(reason) =>
                issuer.report(OverApprox(node.loc, reason))
                Range.full
      else
        issuer.report(OverApprox(node.loc, "pattern is not a constant char"))
        Range.full

    override def visitStringSplit(node: ApplyOp, state: State): AList =
      assert(node.args.length == 3)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val t = node.args(1).accept(this, state).asInstanceOf[AString]
      if t.isChar then
        val cnf = s.toCNF
        CNFOps.split(cnf, t.asChar) match
          case Right(l) => l
          case Left(reason) =>
            issuer.report(OverApprox(node.loc, reason))
            AList.top(ReLang.full)
      else
        issuer.report(OverApprox(node.loc, "seperator is not a constant char"))
        AList.top(ReLang.full)

    override def visitStringStartsWith(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 2)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val t = node.args(1).accept(this, state).asInstanceOf[AString]
      if t.isString then ReLangOps.startsWith(s, t.asString)
      else
        issuer.report(OverApprox(node.loc, "prefix is not a constant string"))
        ABool.Top

    override def visitStringEndsWith(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 2)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val t = node.args(1).accept(this, state).asInstanceOf[AString]
      if t.isString then ReLangOps.endsWith(s, t.asString)
      else
        issuer.report(OverApprox(node.loc, "suffix is not a constant string"))
        ABool.Top

    override def visitStringContains(node: ApplyOp, state: State): ABool =
      assert(node.args.length == 2)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      val t = node.args(1).accept(this, state).asInstanceOf[AString]
      if t.isChar then s.contains(t.asChar)
      else
        issuer.report(OverApprox(node.loc, "pattern is not a constant char"))
        ABool.Top

    override def visitStringToInt(node: ApplyOp, state: State): AInt =
      assert(node.args.length == 1)
      val s = node.args.head.accept(this, state).asInstanceOf[AString]
      if s.isNumber then
        if s.isString then s.asString.toInt else Range.full
      else
        issuer.report(TypeMismatch(node.args.head.loc, "String of digits", s.show))
        Range.full

    override def visitStringFromInt(node: ApplyOp, state: State): AString =
      assert(node.args.length == 1)
      val range = node.args.head.accept(this, state).asInstanceOf[AInt].asRange
      if range.isInt then ReLang.fromString(range.asInt.toString)
      else ReLang.number