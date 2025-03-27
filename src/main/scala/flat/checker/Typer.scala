//package flat.checker
//
//import flat.checker.Bound.PosInf
//import flat.checker.backend.{AssertionMayFail, Index, IndexMayOutOfBounds, OverApprox, SortError, Split, TypeMayMismatch, core}
//import flat.checker.backend.core.*
//
//class Typer:
//  given issuer: Issuer = new Issuer
//
//  def process(script: Seq[FunDef], ctx: GlobalContext = GlobalContext.empty): GlobalContext =
//    val gCtx = script.foldLeft(ctx) { (c, f) => declareFun(f, c) }
//
//    given GlobalContext = gCtx
//
//    val exprChecker = new ExprChecker
//    val stmtChecker = new StmtChecker(exprChecker)
//    for FunDef(Ident(f), paramTypes, returnType, varTypes, body) <- script do
//      val lCtx = LocalContext.from(f, paramTypes ++ varTypes)
//      body.accept(stmtChecker, lCtx)
//
//    issuer.ensureNoError()
//    gCtx
//
//  private def declareFun(funDef: FunDef, ctx: GlobalContext): GlobalContext =
//    ctx.lookup(funDef.ident.name) match
//      case None =>
//        ctx.declare(funDef.ident.name, FunInfo(funDef.paramTypes, funDef.returnType, funDef.ident.loc))
//      case Some(_) =>
//        val err = SortError(s"function '${funDef.ident.name}' is already defined", funDef.ident.loc)
//        throw RuntimeException(err.longString)
//
//  extension (typ: Type)
//    def show: String = typ match
//      case AnyType => "⊤"
//      case NoType => "?"
//      case `unitType` => "Unit"
//      case IntervalType(i) => if i == Interval.full then "Int" else i.toString
//      case TernaryType(b) => b.toString
//      case LangType(r) => if r == ReLang.full then "String" else "/" + r.toString + "/"
//      case TupleType(ts) => "(" + ts.map(_.show).mkString(", ") + ")"
//      case ArrayType(t) => s"Array[${t.show}]"
//      case FunType(ts, t) => "(" + ts.map(_.show).mkString(", ") + ") → " + t.show
//      case HintType(h) => h.toType.show + s"(with hint: $h)"
//
//  private def ensureSort(typ: Type, sort: Sort, exprLoc: Location): Unit =
//    if !(typ.toSort :<: sort) then
//      val err = SortError(s"expect sort $sort, but found ${typ.toSort}", exprLoc)
//      throw RuntimeException(err.longString)
//
//  private class StmtChecker(exprChecker: ExprChecker)(using gCtx: GlobalContext)
//    extends StmtVisitor[LocalContext, LocalContext]:
//    override def visitAssign(node: Assign, ctx: LocalContext): LocalContext =
//      val actual = node.value.accept(exprChecker, ctx)
//      //      issuer.report(ShowType(node.value.loc, actual.toString))
//      val info = ctx(node.id)
//      if actual :<: info.declaredType then ctx.update(node.id, actual)
//      else
//        issuer.report(TypeMayMismatch(info.declaredType.show, actual.show, node.value.loc))
//        ctx
//
//    override def visitAssert(node: Assert, ctx: LocalContext): LocalContext =
//      val t = node.cond.accept(exprChecker, ctx)
//      ensureSort(t, Sort.Bool, node.cond.loc)
//      t.asInstanceOf[TernaryType].value match
//        case Ternary.True => /* well-typed */
//        case _ => issuer.report(new AssertionMayFail(node.cond.loc))
//      ctx
//
//    override def visitReturn(node: Return, ctx: LocalContext): LocalContext =
//      val actual = node.value.accept(exprChecker, ctx)
//      val expected = gCtx(ctx.currentFun).returnType
//      if !(actual :<: expected) then
//        issuer.report(TypeMayMismatch(expected.show, actual.show, node.value.loc))
//      ctx
//
//    override def visitIfStmt(node: IfStmt, ctx: LocalContext): LocalContext =
//      val t = node.cond.accept(exprChecker, ctx)
//      ensureSort(t, Sort.Bool, node.cond.loc)
//      val b = t.asInstanceOf[TernaryType].value
//      val ctx1 = if b.contains(true) then node.body.accept(this, tryAttachCond(node.cond, true, ctx)) else ctx
//      val ctx2 = if b.contains(false) then node.elseBody.accept(this, tryAttachCond(node.cond, false, ctx)) else ctx
//      ctx1 | ctx2
//
//    private def tryAttachCond(cond: Expr, value: Boolean, ctx: LocalContext): LocalContext =
//      cond match
//        case Var(id) => ctx.update(id, TernaryType(value))
//        case _ => ctx
//
//    override def visitWhile(node: While, ctx: LocalContext): LocalContext =
//      // invariants should hold at entry
//      checkInvariants(node.invariants, ctx)
//      // whenever loop is entered, invariants should hold again after each iteration
//      val invCtx: LocalContext = ???
//      val initCtx = tryAttachCond(node.cond, true, invCtx)
//      val finalCtx = node.body.accept(this, initCtx)
//      checkInvariants(node.invariants, finalCtx)
//      // when loop is exited, invariants hold again
//      tryAttachCond(node.cond, false, invCtx)
//
//    private def checkInvariants(invariants: Seq[Any], ctx: LocalContext): Unit = ()
////      for inv <- invariants do
////        if !(ctx(inv.id).latestType :<: inv.typ) then
////          issuer.report(InvariantMayViolate(inv.loc))
//
//    override def visitStmtBlock(node: StmtBlock, ctx: LocalContext): LocalContext =
//      node.body.foldLeft(ctx) { (c, s) => s.accept(this, c) }
//
//  private class ExprChecker(using gCtx: GlobalContext) extends ExprVisitor[LocalContext, Type]:
//    override def visitConst(node: Const, ctx: LocalContext): Type =
//      node.value match
//        case i: Int => IntervalType(i)
//        case b: Boolean => TernaryType(b)
//        case s: String => LangType(s)
//
//    override def visitGlobalRef(node: GlobalRef, ctx: LocalContext): Type =
//      gCtx.lookup(node.name) match
//        case Some(info) => FunType(info.paramTypes, info.returnType)
//        case None =>
//          val err = SortError(s"function '${node.name}' is not defined", node.loc)
//          throw RuntimeException(err.longString)
//
//    override def visitVar(node: Var, ctx: LocalContext): Type =
//      ctx(node.id).latestType
//
//    override def visitTupleExpr(node: TupleExpr, ctx: LocalContext): Type =
//      val ts = for e <- node.elems yield e.accept(this, ctx)
//      core.TupleType(ts)
//
//    override def visitIte(node: Ite, ctx: LocalContext): Type =
//      val t = node.test.accept(this, ctx)
//      ensureSort(t, Sort.Bool, node.test.loc)
//      val b = t.asInstanceOf[TernaryType].value
//      val t1 = if b.contains(true) then node.thenValue.accept(this, ctx) else NoType
//      val t2 = if b.contains(false) then node.elseValue.accept(this, ctx) else NoType
//      t1 | t2
//
//    override def visitApply(node: Apply, ctx: LocalContext): Type =
//      node.fun.accept(this, ctx) match
//        case FunType(expectedTypes, returnType) =>
//          for (e, expected) <- node.args zip expectedTypes do
//            val actual = e.accept(this, ctx)
//            if !(actual :<: expected) then
//              issuer.report(TypeMayMismatch(expected.show, actual.show, e.loc))
//          returnType
//        case actual =>
//          val err = SortError(s"expect function, but found ${actual.toSort}", node.fun.loc)
//          throw RuntimeException(err.longString)
//
//    // Boolean
//    override def visitAnd(node: And, ctx: LocalContext): Type =
//      val t1 = node.left.accept(this, ctx)
//      val t2 = node.right.accept(this, ctx)
//      (t1, t2) match
//        case (TernaryType(b1), TernaryType(b2)) => TernaryType(b1 && b2)
//        case _ => boolType
//
//    override def visitOr(node: Or, ctx: LocalContext): Type =
//      val t1 = node.left.accept(this, ctx)
//      val t2 = node.right.accept(this, ctx)
//      (t1, t2) match
//        case (TernaryType(b1), TernaryType(b2)) => TernaryType(b1 || b2)
//        case _ => boolType
//
//    override def visitNot(node: Not, ctx: LocalContext): Type =
//      val t = node.operand.accept(this, ctx)
//      not(t)
//
//    private def not(t: Type): Type =
//      t match
//        case TernaryType(b) => TernaryType(!b)
//        case _ => boolType
//
//    // Comparison
//    override def visitCmp(node: Cmp, ctx: LocalContext): Type =
//      val t1 = node.left.accept(this, ctx)
//      val t2 = node.right.accept(this, ctx)
//      node.op match
//        case CmpOp.EQ => equal(t1, t2)
//        case CmpOp.NE => not(equal(t1, t2))
//        case CmpOp.LE => not(lessThan(t2, t1)) // x <= y iff !(y < x)
//        case CmpOp.LT => lessThan(t1, t2)
//        case CmpOp.GE => not(lessThan(t1, t2)) // x >= y iff !(x < y)
//        case CmpOp.GT => lessThan(t2, t1) // x > y iff y < x
//
//    private def equal(t1: Type, t2: Type): Type =
//      (t1.ignoreHint, t2.ignoreHint) match
//        case (IntervalType(i1), IntervalType(i2)) => TernaryType(i1 equiv i2)
//        case (TernaryType(b1), TernaryType(b2)) if b1.isBoolean && b2.isBoolean =>
//          TernaryType(b1.asBoolean == b2.asBoolean)
//        case (LangType(s1), LangType(s2)) if s1.isString && s2.isString =>
//          TernaryType(s1.asString == s2.asString)
//        case _ => boolType
//
//    private def lessThan(t1: Type, t2: Type): Type =
//      (t1.ignoreHint, t2.ignoreHint) match
//        case (IntervalType(i1), IntervalType(i2)) => TernaryType(i1 < i2)
//        case _ => boolType
//
//    // Int
//    override def visitArith(node: Arith, ctx: LocalContext): Type =
//      val t1 = node.left.accept(this, ctx)
//      val t2 = node.right.accept(this, ctx)
//      node.op match
//        case ArithOp.ADD => add(t1, t2)
//        case ArithOp.SUB =>
//          (t1.ignoreHint, t2.ignoreHint) match
//            case (IntervalType(i1), IntervalType(i2)) => IntervalType(i1 - i2)
//            case _ => intType
//
//    private def add(t1: Type, t2: Type): Type =
//      (t1, t2) match
//        case (HintType(index: Index), IntervalType(i)) if i.isInt =>
//          CNFOps.shiftIndex(index, i.asInt) match
//            case Right(newIndex) => return HintType(newIndex)
//            case _ =>
//        case (IntervalType(i), HintType(index: Index)) if i.isInt =>
//          CNFOps.shiftIndex(index, i.asInt) match
//            case Right(newIndex) => return HintType(newIndex)
//            case _ =>
//        case _ =>
//      (t1.ignoreHint, t2.ignoreHint) match
//        case (IntervalType(i1), IntervalType(i2)) => IntervalType(i1 + i2)
//        case _ => intType
//
//    // Char
//    override def visitCharToCode(node: CharToCode, ctx: LocalContext): Type =
//      val t = node.char.accept(this, ctx)
//      t match
//        case LangType(r) if r.isChar => IntervalType(r.asChar.toInt)
//        case LangType(r) =>
//          val k = r.length
//          if !(k.isInt && k.asInt == 1) then
//            issuer.report(TypeMayMismatch("String of length 1", s"String of length $k", node.char.loc))
//          intType
//        case _ => intType
//
//    override def visitCharFromCode(node: CharFromCode, ctx: LocalContext): Type =
//      val t = node.code.accept(this, ctx)
//      t.ignoreHint match
//        case IntervalType(i) if i.isInt => LangType(ReLang.fromChar(i.asInt.toChar))
//        case _ => LangType(ReLang.allChar)
//
//    // String
//    override def visitStrConcat(node: StrConcat, ctx: LocalContext): Type =
//      val t1 = node.left.accept(this, ctx)
//      val t2 = node.right.accept(this, ctx)
//      (t1, t2) match
//        case (LangType(r1), LangType(r2)) => LangType(ReLangOps.concat(r1, r2))
//        case _ => strType
//
//    override def visitStrRev(node: StrRev, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      t match
//        case LangType(r) => LangType(r.reverse)
//        case _ => strType
//
//    override def visitStrLen(node: StrLen, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      t match
//        case LangType(r) => IntervalType(r.length)
//        case _ => IntervalType(Interval(0, PosInf))
//
//    override def visitStrAt(node: StrAt, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.index.accept(this, ctx)
//      (t.ignoreHint, t1.ignoreHint) match
//        case (LangType(r), IntervalType(i)) =>
//          if i.isInt then
//            ReLangOps.charAt(r, i.asInt) match
//              case Some(cs) => LangType(ReLang.ReChars(cs))
//              case None =>
//                issuer.report(IndexMayOutOfBounds(node.index.loc))
//                LangType(ReLang.allChar)
//          else
//            issuer.report(OverApprox("index is non-constant", node.loc))
//            LangType(ReLang.allChar)
//        case _ => LangType(ReLang.allChar)
//
//    private def getRelPos(cnf: List[ReLang], intType: Type): Either[String, Int] = intType match
//      case HintType(Index(cnf1, pos)) =>
//        if cnf1 == cnf then Right(pos) else Left("cnf mismatch")
//      case IntervalType(r) if r.isInt => CNFOps.convertToRelative(cnf, r.asInt)
//      case IntervalType(r) => Left("index not constant")
//      case _ => throw IllegalArgumentException()
//
//    override def visitStrSlice(node: StrSlice, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.fromIndex.accept(this, ctx)
//      val t2 = node.untilIndex.accept(this, ctx)
//      (t, t1, t2) match
//        case (LangType(r), from, until) =>
//          val cnf = r.toCNF
//          getRelPos(cnf, from) match
//            case Right(fromPos) =>
//              getRelPos(cnf, until) match
//                case Right(untilPos) => LangType(CNFOps.substring(cnf, fromPos, untilPos))
//                case Left(reason) =>
//                  issuer.report(OverApprox(reason, node.loc))
//                  strType
//            case Left(reason) =>
//              issuer.report(OverApprox(reason, node.loc))
//              strType
//        case _ => strType
//
//    override def visitStrFind(node: StrFind, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.target.accept(this, ctx)
//      val t2 = node.fromIndex.accept(this, ctx)
//      (t, t1, t2) match
//        case (LangType(r), LangType(rt), from) =>
//          if rt.isChar then
//            val cnf = r.toCNF
//            getRelPos(cnf, from) match
//              case Left(reason) =>
//                issuer.report(OverApprox(reason, node.fromIndex.loc))
//                intType
//              case Right(fromPos) =>
//                CNFOps.indexOf(cnf, rt.asChar, fromPos) match
//                  case Right(pos) => HintType(Index(cnf, pos))
//                  case Left(reason) =>
//                    issuer.report(OverApprox(reason, node.loc))
//                    intType
//          else
//            issuer.report(OverApprox("pattern is not a constant char", node.loc))
//            intType
//        case _ => intType
//
//    override def visitStrSplit(node: StrSplit, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.sep.accept(this, ctx)
//      (t, t1) match
//        case (LangType(r), LangType(rt)) =>
//          if rt.isChar then
//            val cnf = r.toCNF
//            CNFOps.split(cnf, rt.asChar) match
//              case Right(split) => HintType(split)
//              case Left(reason) =>
//                issuer.report(OverApprox(reason, node.loc))
//                ArrayType(strType)
//          else
//            issuer.report(OverApprox("seperator is not a constant char", node.loc))
//            ArrayType(strType)
//        case _ => ArrayType(strType)
//
//    override def visitStrStartsWith(node: StrStartsWith, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.prefix.accept(this, ctx)
//      (t, t1) match
//        case (LangType(r), LangType(rt)) =>
//          if rt.isString then TernaryType(ReLangOps.startsWith(r, rt.asString))
//          else
//            issuer.report(OverApprox("prefix is not a constant string", node.loc))
//            boolType
//        case _ => boolType
//
//    override def visitStrEndsWith(node: StrEndsWith, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.suffix.accept(this, ctx)
//      (t, t1) match
//        case (LangType(r), LangType(rt)) =>
//          if rt.isString then TernaryType(ReLangOps.endsWith(r, rt.asString))
//          else
//            issuer.report(OverApprox("prefix is not a constant string", node.loc))
//            boolType
//        case _ => boolType
//
//    override def visitStrContains(node: StrContains, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      val t1 = node.infix.accept(this, ctx)
//      (t, t1) match
//        case (LangType(r), LangType(rt)) =>
//          if rt.isChar then TernaryType(r.contains(rt.asChar))
//          else
//            issuer.report(OverApprox("prefix is not a constant char", node.loc))
//            boolType
//        case _ => boolType
//
//    override def visitStrToInt(node: StrToInt, ctx: LocalContext): Type =
//      val t = node.str.accept(this, ctx)
//      t match
//        case LangType(r) if r.isNumber && r.isString => IntervalType(r.asString.toInt)
//        case LangType(r) if !r.isNumber =>
//          issuer.report(TypeMayMismatch("String of digits", t.show, node.str.loc))
//          intType
//        case _ => intType
//
//    override def visitStrFromInt(node: StrFromInt, ctx: LocalContext): Type =
//      val t = node.int.accept(this, ctx)
//      t.ignoreHint match
//        case IntervalType(i) if i.isInt => LangType(i.asInt.toString)
//        case _ => LangType(ReLang.number)
//
//    override def visitArraySelect(node: ArraySelect, ctx: LocalContext): Type =
//      val t1 = node.array.accept(this, ctx)
//      val t2 = node.index.accept(this, ctx)
//      (t1, t2.ignoreHint) match
//        case (HintType(split: Split), IntervalType(i)) if i.isInt =>
//          return split.get(i.asInt) match
//            case Some(r) => LangType(r)
//            case None => strType
//        case _ =>
//      t1.ignoreHint match
//        case ArrayType(elemType) => elemType
//        case _ => NoType