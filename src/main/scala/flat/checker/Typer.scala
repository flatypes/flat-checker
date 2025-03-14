package flat.checker

import flat.checker.Bound.PosInf
import flat.checker.ast.*

class Typer:
  given issuer: Issuer = new Issuer

  def process(script: Seq[FunDef], ctx: GlobalContext = GlobalContext.empty): GlobalContext =
    val gCtx = script.foldLeft(ctx) { (c, f) => declareFun(f, c) }

    given GlobalContext = gCtx

    val exprChecker = new ExprChecker
    val stmtChecker = new StmtChecker(exprChecker)
    for FunDef(Ident(f), paramTypes, returnType, varTypes, body) <- script do
      val lCtx = LocalContext.from(f, paramTypes ++ varTypes)
      body.accept(stmtChecker, lCtx)

    issuer.ensureNoError()
    gCtx

  private def declareFun(funDef: FunDef, ctx: GlobalContext): GlobalContext =
    ctx.lookup(funDef.ident.name) match
      case None =>
        ctx.declare(funDef.ident.name, FunInfo(funDef.paramTypes, funDef.returnType, funDef.ident.loc))
      case Some(_) =>
        val err = SortError(s"function '${funDef.ident.name}' is already defined", funDef.ident.loc)
        throw RuntimeException(err.longString)

  extension (typ: Type)
    def show: String = typ match
      case AnyType => "⊤"
      case NoType => "?"
      case UnitType => "Unit"
      case IntervalType(i) => if i == Interval.full then "Int" else i.toString
      case TernaryType(b) => b.toString
      case LangType(r) => if r == ReLang.full then "String" else "/" + r.toString + "/"
      case ArrayType(t) => s"Array[${t.show}]"
      case FunType(ts, t) => "(" + ts.map(_.show).mkString(", ") + ") → " + t.show
      case HintType(h) => h.toType.show + s"(with hint: $h)"

  private def ensureSort(typ: Type, sort: Sort, exprLoc: Location): Unit =
    if !(typ.toSort :<: sort) then
      val err = SortError(s"expect sort $sort, but found ${typ.toSort}", exprLoc)
      throw RuntimeException(err.longString)

  private class StmtChecker(exprChecker: ExprChecker)(using gCtx: GlobalContext)
    extends NodeVisitor[LocalContext, LocalContext]:
    override def visitAssign(node: Assign, ctx: LocalContext): LocalContext =
      val actual = node.value.accept(exprChecker, ctx)
      //      issuer.report(ShowType(node.value.loc, actual.toString))
      val info = ctx(node.id)
      if actual :<: info.declaredType then ctx.update(node.id, actual)
      else
        issuer.report(TypeMayMismatch(info.declaredType.show, actual.show, node.value.loc))
        ctx

    override def visitAssert(node: Assert, ctx: LocalContext): LocalContext =
      val t = node.cond.accept(exprChecker, ctx)
      ensureSort(t, Sort.Bool, node.cond.loc)
      t.asInstanceOf[TernaryType].value match
        case Ternary.True => /* well-typed */
        case _ => issuer.report(new AssertionMayFail(node.cond.loc))
      ctx

    override def visitReturn(node: Return, ctx: LocalContext): LocalContext =
      val actual = node.value.accept(exprChecker, ctx)
      val expected = gCtx(ctx.currentFun).returnType
      if !(actual :<: expected) then
        issuer.report(TypeMayMismatch(expected.show, actual.show, node.value.loc))
      ctx

    override def visitIfStmt(node: IfStmt, ctx: LocalContext): LocalContext =
      val t = node.cond.accept(exprChecker, ctx)
      ensureSort(t, Sort.Bool, node.cond.loc)
      val b = t.asInstanceOf[TernaryType].value
      val ctx1 = if b.contains(true) then node.body.accept(this, tryAttachCond(node.cond, true, ctx)) else ctx
      val ctx2 = if b.contains(false) then node.elseBody.accept(this, tryAttachCond(node.cond, false, ctx)) else ctx
      ctx1 | ctx2

    private def tryAttachCond(cond: Expr, value: Boolean, ctx: LocalContext): LocalContext =
      cond match
        case LocalRef(id) => ctx.update(id, TernaryType(value))
        case _ => ctx

    override def visitWhile(node: While, ctx: LocalContext): LocalContext =
      // invariants should hold at entry
      checkInvariants(node.invariants, ctx)
      // whenever loop is entered, invariants should hold again after each iteration
      val invCtx = node.invariants.foldLeft(ctx.havoc) { case (c, Invariant(id, t)) => ctx.update(id, t) }
      val initCtx = tryAttachCond(node.cond, true, invCtx)
      val finalCtx = node.body.accept(this, initCtx)
      checkInvariants(node.invariants, finalCtx)
      // when loop is exited, invariants hold again
      tryAttachCond(node.cond, false, invCtx)

    private def checkInvariants(invariants: Seq[Invariant], ctx: LocalContext): Unit =
      for inv <- invariants do
        if !(ctx(inv.id).latestType :<: inv.typ) then
          issuer.report(InvariantMayViolate(inv.loc))

    override def visitStmtBlock(node: StmtBlock, ctx: LocalContext): LocalContext =
      node.body.foldLeft(ctx) { (c, s) => s.accept(this, c) }

  private class ExprChecker(using gCtx: GlobalContext) extends NodeVisitor[LocalContext, Type]:
    override def visitLiteral(node: Literal, ctx: LocalContext): Type =
      node.value match
        case i: Int => IntervalType(i)
        case b: Boolean => TernaryType(b)
        case s: String => LangType(s)
        case () => UnitType

    override def visitGlobalRef(node: GlobalRef, ctx: LocalContext): Type =
      gCtx.lookup(node.name) match
        case Some(info) => FunType(info.paramTypes, info.returnType)
        case None =>
          val err = SortError(s"function '${node.name}' is not defined", node.loc)
          throw RuntimeException(err.longString)

    override def visitLocalRef(node: LocalRef, ctx: LocalContext): Type =
      ctx(node.id).latestType

    override def visitIfExpr(node: IfExpr, ctx: LocalContext): Type =
      val t = node.cond.accept(this, ctx)
      ensureSort(t, Sort.Bool, node.cond.loc)
      val b = t.asInstanceOf[TernaryType].value
      val t1 = if b.contains(true) then node.body.accept(this, ctx) else NoType
      val t2 = if b.contains(false) then node.body.accept(this, ctx) else NoType
      t1 | t2

    override def visitApply(node: Apply, ctx: LocalContext): Type =
      node.fun.accept(this, ctx) match
        case FunType(expectedTypes, returnType) =>
          for (e, expected) <- node.args zip expectedTypes do
            val actual = e.accept(this, ctx)
            if !(actual :<: expected) then
              issuer.report(TypeMayMismatch(expected.show, actual.show, e.loc))
          returnType
        case actual =>
          val err = SortError(s"expect function, but found ${actual.toSort}", node.fun.loc)
          throw RuntimeException(err.longString)

    // library operations
    private def checkArgs(node: ApplyOp, paramSorts: Seq[Sort], ctx: LocalContext): Seq[Type] =
      if node.args.length != paramSorts.length then
        val err = SortError(s"arity mismatch: expect ${paramSorts.length}, but found ${node.args.length}", node.loc)
        throw RuntimeException(err.longString)
      for (arg, expected) <- node.args zip paramSorts yield
        val actual = arg.accept(this, ctx)
        ensureSort(actual, expected, arg.loc)
        actual

    // Boolean
    override def visitAnd(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Bool, Sort.Bool), ctx)
      types match
        case Seq(TernaryType(b1), TernaryType(b2)) => TernaryType(b1 && b2)
        case _ => boolType

    override def visitOr(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Bool, Sort.Bool), ctx)
      types match
        case Seq(TernaryType(b1), TernaryType(b2)) => TernaryType(b1 || b2)
        case _ => boolType

    override def visitNot(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Bool), ctx)
      types match
        case Seq(TernaryType(b)) => TernaryType(!b)
        case _ => boolType

    // Comparison
    override def visitEqual(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Top, Sort.Top), ctx)
      val Seq(t1, t2) = types.map(_.ignoreHint)
      (t1, t2) match
        case (IntervalType(i1), IntervalType(i2)) => TernaryType(i1 equiv i2)
        case (TernaryType(b1), TernaryType(b2)) if b1.isBoolean && b2.isBoolean =>
          TernaryType(b1.asBoolean == b2.asBoolean)
        case (LangType(s1), LangType(s2)) if s1.isString && s2.isString =>
          TernaryType(s1.asString == s2.asString)
        case _ => boolType

    override def visitLessThan(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Top, Sort.Top), ctx)
      val Seq(t1, t2) = types.map(_.ignoreHint)
      (t1, t2) match
        case (IntervalType(i1), IntervalType(i2)) => TernaryType(i1 < i2)
        case _ => boolType

    // Int
    override def visitAdd(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int, Sort.Int), ctx)
      types match
        case Seq(HintType(index: Index), IntervalType(i)) if i.isInt =>
          CNFOps.shiftIndex(index, i.asInt) match
            case Right(newIndex) => return HintType(newIndex)
            case _ =>
        case Seq(IntervalType(i), HintType(index: Index)) if i.isInt =>
          CNFOps.shiftIndex(index, i.asInt) match
            case Right(newIndex) => return HintType(newIndex)
            case _ =>
        case _ =>
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i1), IntervalType(i2)) => IntervalType(i1 + i2)
        case _ => intType

    override def visitSub(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int, Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i1), IntervalType(i2)) => IntervalType(i1 - i2)
        case _ => intType

    // Char
    override def visitCharToCode(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) if r.isChar => IntervalType(r.asChar.toInt)
        case Seq(LangType(r)) =>
          val k = r.length
          if !(k.isInt && k.asInt == 1) then
            issuer.report(TypeMayMismatch("String of length 1", s"String of length $k", node.args.head.loc))
          intType
        case _ => intType

    override def visitCharFromCode(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i)) if i.isInt => LangType(ReLang.fromChar(i.asInt.toChar))
        case _ => LangType(ReLang.allChar)

    // String
    override def visitStringConcat(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r1), LangType(r2)) => LangType(ReLangOps.concat(r1, r2))
        case _ => stringType

    override def visitStringReverse(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) => LangType(r.reverse)
        case _ => stringType

    override def visitStringLength(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) => IntervalType(r.length)
        case _ => IntervalType(Interval(0, PosInf))

    override def visitStringAt(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(LangType(r), IntervalType(i)) =>
          if i.isInt then
            ReLangOps.charAt(r, i.asInt) match
              case Some(cs) => LangType(ReLang.ReChars(cs))
              case None =>
                issuer.report(IndexMayOutOfBounds(node.args(1).loc))
                LangType(ReLang.allChar)
          else
            issuer.report(OverApprox("index is non-constant", node.loc))
            LangType(ReLang.allChar)
        case _ => LangType(ReLang.allChar)

    private def getRelPos(cnf: List[ReLang], intType: Type): Either[String, Int] = intType match
      case HintType(Index(cnf1, pos)) =>
        if cnf1 == cnf then Right(pos) else Left("cnf mismatch")
      case IntervalType(r) if r.isInt => CNFOps.convertToRelative(cnf, r.asInt)
      case IntervalType(r) => Left("index not constant")
      case _ => throw IllegalArgumentException()

    override def visitSubstring(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.Int, Sort.Int), ctx)
      types match
        case Seq(LangType(r), from, until) =>
          val cnf = r.toCNF
          getRelPos(cnf, from) match
            case Right(fromPos) =>
              getRelPos(cnf, until) match
                case Right(untilPos) => LangType(CNFOps.substring(cnf, fromPos, untilPos))
                case Left(reason) =>
                  issuer.report(OverApprox(reason, node.loc))
                  stringType
            case Left(reason) =>
              issuer.report(OverApprox(reason, node.loc))
              stringType
        case _ => stringType

    override def visitStringIndexOf(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String, Sort.Int), ctx)
      types match
        case Seq(LangType(r), LangType(rt), from) =>
          if rt.isChar then
            val cnf = r.toCNF
            getRelPos(cnf, from) match
              case Left(reason) =>
                issuer.report(OverApprox(reason, node.args(2).loc))
                intType
              case Right(fromPos) =>
                CNFOps.indexOf(cnf, rt.asChar, fromPos) match
                  case Right(pos) => HintType(Index(cnf, pos))
                  case Left(reason) =>
                    issuer.report(OverApprox(reason, node.loc))
                    intType
          else
            issuer.report(OverApprox("pattern is not a constant char", node.loc))
            intType
        case _ => intType

    override def visitStringSplit(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isChar then
            val cnf = r.toCNF
            CNFOps.split(cnf, rt.asChar) match
              case Right(split) => HintType(split)
              case Left(reason) =>
                issuer.report(OverApprox(reason, node.loc))
                ArrayType(stringType)
          else
            issuer.report(OverApprox("seperator is not a constant char", node.loc))
            ArrayType(stringType)
        case _ => ArrayType(stringType)

    override def visitStringStartsWith(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isString then TernaryType(ReLangOps.startsWith(r, rt.asString))
          else
            issuer.report(OverApprox("prefix is not a constant string", node.loc))
            boolType
        case _ => boolType

    override def visitStringEndsWith(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isString then TernaryType(ReLangOps.endsWith(r, rt.asString))
          else
            issuer.report(OverApprox("prefix is not a constant string", node.loc))
            boolType
        case _ => boolType

    override def visitStringContains(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isChar then TernaryType(r.contains(rt.asChar))
          else
            issuer.report(OverApprox("prefix is not a constant char", node.loc))
            boolType
        case _ => boolType

    override def visitStringToInt(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) if r.isNumber && r.isString => IntervalType(r.asString.toInt)
        case Seq(t@LangType(r)) if !r.isNumber =>
          issuer.report(TypeMayMismatch("String of digits", t.show, node.args.head.loc))
          intType
        case _ => intType

    override def visitStringFromInt(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i)) if i.isInt => LangType(i.asInt.toString)
        case _ => LangType(ReLang.number)

    override def visitArrayAt(node: ApplyOp, ctx: LocalContext): Type =
      val t1 = node.args.head.accept(this, ctx)
      val t2 = node.args(1).accept(this, ctx)
      (t1, t2.ignoreHint) match
        case (HintType(split: Split), IntervalType(i)) if i.isInt =>
          return split.get(i.asInt) match
            case Some(r) => LangType(r)
            case None => stringType
        case _ =>
      t1.ignoreHint match
        case ArrayType(elemType) => elemType
        case _ => NoType