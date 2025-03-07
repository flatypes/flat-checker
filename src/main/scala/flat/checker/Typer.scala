package flat.checker

import flat.checker.Bound.PosInf
import flat.checker.ast.*

class Typer:
  given issuer: Issuer = new Issuer

  def process(script: Seq[FunDef], ctx: GlobalContext = GlobalContext.empty): GlobalContext =
    val gCtx = script.foldLeft(ctx) { case (c, f) => declareFun(f, c) }

    given GlobalContext = gCtx

    val exprChecker = new ExprChecker
    val stmtChecker = new StmtChecker(exprChecker)
    for FunDef(Ident(f), params, returnType, body) <- script do
      val lCtx = LocalContext(f)
      val newCtx = params.foldLeft(lCtx) { case (c, p) => declareParam(p, c) }
      body.accept(stmtChecker, newCtx)
    gCtx

  private def declareFun(funDef: FunDef, ctx: GlobalContext): GlobalContext =
    ctx.lookup(funDef.ident.name) match
      case None =>
        val paramTypes = funDef.params.map(_._2)
        ctx.declare(funDef.ident.name, FunInfo(paramTypes, funDef.returnType, funDef.ident.loc))
      case Some(info: FunInfo) =>
        issuer.report(NameError(funDef.ident.loc, s"redefined function '${funDef.ident.name}'"))
        ctx

  private def declareParam(param: (Ident, Type), ctx: LocalContext): LocalContext =
    val (ident, typ) = param
    ctx.lookup(ident.name) match
      case None => ctx.declare(ident.name, typ)
      case Some(info: VarInfo) =>
        issuer.report(NameError(ident.loc, s"redefined parameter '${ident.name}'"))
        ctx

  extension (typ: Type)
    def show: String = typ match
      case AnyType => "⊤"
      case NoType => "?"
      case IntervalType(i) => if i == Interval.full then "Int" else i.toString
      case TernaryType(b) => b.toString
      case LangType(r) => if r == ReLang.full then "String" else "/" + r.toString + "/"
      case ArrayType(t) => s"Array[${t.show}]"
      case FunType(ts, t) => "(" + ts.map(_.show).mkString(", ") + ") → " + t.show
      case HintType(h) => h.toType.show + s"(with hint: $h)"

  private class StmtChecker(exprChecker: ExprChecker)(using gCtx: GlobalContext)
    extends NodeVisitor[LocalContext, LocalContext]:
    override def visitDeclare(node: Declare, ctx: LocalContext): LocalContext =
      ctx.lookup(node.ident.name) match
        case None => ctx.declare(node.ident.name, node.typ)
        case Some(info: VarInfo) =>
          issuer.report(NameError(node.ident.loc, s"redefined local variable '${node.ident.name}'"))
          ctx

    override def visitAssign(node: Assign, ctx: LocalContext): LocalContext =
      val actual = node.value.accept(exprChecker, ctx)
      //      issuer.report(ShowType(node.value.loc, actual.toString))
      node.target match
        case Some(ident) =>
          ctx.lookup(ident.name) match
            case Some(info) =>
              if actual :<: info.declaredType then ctx.update(ident.name, actual)
              else
                issuer.report(TypeMismatch(node.value.loc, info.declaredType.show, actual.show))
                ctx
            case None =>
              issuer.report(NameError(ident.loc, s"undefined local variable '${ident.name}'"))
              ctx
        case None => ctx

    override def visitAssert(node: Assert, ctx: LocalContext): LocalContext =
      checkCond(node.cond, ctx) match
        case Some(Ternary.True) => /* well-typed */
        case Some(_) => issuer.report(new AssertionError(node.cond.loc))
        case None =>
      ctx

    private def checkCond(cond: Expr, ctx: LocalContext): Option[Ternary] =
      cond.accept(exprChecker, ctx) match
        case TernaryType(b) => Some(b)
        case actual =>
          issuer.report(SortMismatch(Sort.Bool.toString, actual.toSort.toString, cond.loc))
          None

    override def visitReturn(node: Return, ctx: LocalContext): LocalContext =
      val actual = node.value.accept(exprChecker, ctx)
      val expected = gCtx(ctx.currentFun).returnType
      if !(actual :<: expected) then
        issuer.report(TypeMismatch(node.value.loc, expected.show, actual.show))
      ctx

    override def visitIfStmt(node: IfStmt, ctx: LocalContext): LocalContext =
      val b = checkCond(node.cond, ctx).getOrElse(Ternary.Maybe)
      val ctx1 = if b.contains(true) then node.body.accept(this, ctx) else ctx
      val ctx2 = if b.contains(false) then node.elseBody.accept(this, ctx) else ctx
      ctx1 | ctx2

    override def visitWhile(node: While, ctx: LocalContext): LocalContext =
      throw UnsupportedOperationException("while")

    override def visitStmtBlock(node: StmtBlock, ctx: LocalContext): LocalContext =
      val newCtx = node.body.foldLeft(ctx.push) { (c, s) => s.accept(this, c) }
      newCtx.pop

  private class ExprChecker(using gCtx: GlobalContext) extends NodeVisitor[LocalContext, Type]:
    override def visitLiteral(node: Literal, ctx: LocalContext): Type =
      node.value match
        case i: Int => IntervalType(i)
        case b: Boolean => TernaryType(b)
        case s: String => LangType(s)

    override def visitGlobalRef(node: GlobalRef, ctx: LocalContext): Type =
      gCtx.lookup(node.ident.name) match
        case Some(info) => FunType(info.paramTypes, info.returnType)
        case None =>
          issuer.report(NameError(node.ident.loc, s"undefined function: ${node.ident.name}"))
          NoType

    override def visitLocalRef(node: LocalRef, ctx: LocalContext): Type =
      ctx.lookup(node.ident.name) match
        case Some(info) => info.latestType
        case None =>
          issuer.report(NameError(node.ident.loc, s"undefined function: ${node.ident.name}"))
          NoType

    override def visitIfExpr(node: IfExpr, ctx: LocalContext): Type =
      val b = node.cond.accept(this, ctx) match
        case TernaryType(b) => b
        case actual =>
          issuer.report(SortMismatch(Sort.Bool.toString, actual.toSort.toString, node.cond.loc))
          Ternary.Maybe
      val t1 = if b.contains(true) then node.body.accept(this, ctx) else NoType
      val t2 = if b.contains(false) then node.body.accept(this, ctx) else NoType
      t1 | t2

    override def visitApply(node: Apply, ctx: LocalContext): Type =
      node.fun.accept(this, ctx) match
        case FunType(expectedTypes, returnType) =>
          for (e, expected) <- node.args zip expectedTypes do
            val actual = e.accept(this, ctx)
            if !(actual :<: expected) then
              issuer.report(TypeMismatch(e.loc, expected.show, actual.show))
          returnType
        case actual =>
          issuer.report(SortMismatch("function", actual.toString, node.fun.loc))
          NoType

    // library operations
    private def checkArgs(node: ApplyOp, paramSorts: Seq[Sort], ctx: LocalContext): Seq[Type] =
      if node.args.length != paramSorts.length then
        issuer.report(???)
      for (arg, expected) <- node.args zip paramSorts yield
        val actual = arg.accept(this, ctx)
        if !(actual.toSort :<: expected) then
          issuer.report(SortMismatch(expected.toString, actual.toSort.toString, arg.loc))
        actual

    private def checkSort(node: Expr, expected: Sort, ctx: LocalContext): Type =
      val actual = node.accept(this, ctx)
      if !(actual.toSort :<: expected) then
        issuer.report(SortMismatch(expected.toString, actual.toSort.toString, node.loc))
      actual

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

    override def visitCharToCode(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) if r.isChar => IntervalType(r.asChar.toInt)
        case Seq(LangType(r)) =>
          val k = r.length
          if !(k.isInt && k.asInt == 1) then
            issuer.report(TypeMismatch(node.args.head.loc, "String of length 1", s"String of length $k"))
          intType
        case _ => intType

    override def visitCharFromCode(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i)) if i.isInt => LangType(ReLang.fromChar(i.asInt.toChar))
        case _ => LangType(ReLang.allChar)

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
                issuer.report(IndexOutOfBounds(node.args(1).loc))
                LangType(ReLang.allChar)
          else
            issuer.report(OverApprox(node.loc, "index is non-constant"))
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
                  issuer.report(OverApprox(node.loc, reason))
                  stringType
            case Left(reason) =>
              issuer.report(OverApprox(node.loc, reason))
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
                issuer.report(OverApprox(node.args(2).loc, reason))
                intType
              case Right(fromPos) =>
                CNFOps.indexOf(cnf, rt.asChar, fromPos) match
                  case Right(pos) => HintType(Index(cnf, pos))
                  case Left(reason) =>
                    issuer.report(OverApprox(node.loc, reason))
                    intType
          else
            issuer.report(OverApprox(node.loc, "pattern is not a constant char"))
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
                issuer.report(OverApprox(node.loc, reason))
                ArrayType(stringType)
          else
            issuer.report(OverApprox(node.loc, "seperator is not a constant char"))
            ArrayType(stringType)
        case _ => ArrayType(stringType)

    override def visitStringStartsWith(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isString then TernaryType(ReLangOps.startsWith(r, rt.asString))
          else
            issuer.report(OverApprox(node.loc, "prefix is not a constant string"))
            boolType
        case _ => boolType

    override def visitStringEndsWith(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isString then TernaryType(ReLangOps.endsWith(r, rt.asString))
          else
            issuer.report(OverApprox(node.loc, "prefix is not a constant string"))
            boolType
        case _ => boolType

    override def visitStringContains(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String, Sort.String), ctx)
      types match
        case Seq(LangType(r), LangType(rt)) =>
          if rt.isChar then TernaryType(r.contains(rt.asChar))
          else
            issuer.report(OverApprox(node.loc, "prefix is not a constant char"))
            boolType
        case _ => boolType

    override def visitStringToInt(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.String), ctx)
      types match
        case Seq(LangType(r)) if r.isNumber && r.isString => IntervalType(r.asString.toInt)
        case Seq(t@LangType(r)) if !r.isNumber =>
          issuer.report(TypeMismatch(node.args.head.loc, "String of digits", t.show))
          intType
        case _ => intType

    override def visitStringFromInt(node: ApplyOp, ctx: LocalContext): Type =
      val types = checkArgs(node, Seq(Sort.Int), ctx)
      types.map(_.ignoreHint) match
        case Seq(IntervalType(i)) if i.isInt => LangType(i.asInt.toString)
        case _ => LangType(ReLang.number)

    override def visitArrayAt(node: ApplyOp, ctx: LocalContext): Type =
      if node.args.length != 2 then
        issuer.report(???)
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