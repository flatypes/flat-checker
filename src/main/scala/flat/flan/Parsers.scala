package flat.flan

import com.ibm.icu.text.UnicodeSet
import flat.antlr.{FlanLexer, FlanParser, FlanParserBaseVisitor}
import flat.flan.untpd.*
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.tree.TerminalNode
import org.apache.commons.text.StringEscapeUtils
import org.eclipse.lsp4j.{Diagnostic, Position, Range}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

object Parsers:
  def parseProgram(source: String, uri: String): Either[List[Diagnostic], Program] =
    // Setup lexer
    val stream = CharStreams.fromString(source, uri)
    val lexer = new FlanLexer(stream)
    val diagnostics = ListBuffer.empty[Diagnostic]
    lexer.removeErrorListeners()
    lexer.addErrorListener(new ErrorListener(diagnostics))
    // Setup parser
    val tokens = new CommonTokenStream(lexer)
    val parser = new FlanParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new ErrorListener(diagnostics))
    // Perform parsing
    val ctx = parser.program()
    if diagnostics.isEmpty then
      val defs = ctx.topDef.asScala.toList.map(_.accept(TopDefVisitor))
      Right(Program(defs)(uri))
    else
      Left(diagnostics.toList)

  private class ErrorListener(diagnostics: ListBuffer[Diagnostic]) extends BaseErrorListener:
    override def syntaxError(recognizer: Recognizer[?, ?], offendingSymbol: Any,
                             line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
      val range = offendingSymbol match
        case token: Token => getRange(token)
        case _ => Range(Position(line - 1, charPositionInLine), Position(line - 1, charPositionInLine + 1))
      val desc = e match
        case e: InputMismatchException =>
          s"unexpected token (expected ${e.getExpectedTokens.toString(recognizer.getVocabulary)})"
        case _ => s"unexpected token ($msg)"
      diagnostics += Diagnostic(range, s"Syntax Error: $desc")

  given Conversion[TerminalNode, Token] = _.getSymbol

  private object TopDefVisitor extends FlanParserBaseVisitor[TopDef]:
    override def visitTypeDef(ctx: FlanParser.TypeDefContext): TypeDef =
      val ident = mkIdent(ctx.IDENT)
      val value = ctx.`type`.accept(TypeVisitor)
      TypeDef(ident, value)

    override def visitLangDef(ctx: FlanParser.LangDefContext): LangDef =
      val ident = mkIdent(ctx.IDENT)
      val clause = ctx.clause.accept(ClauseVisitor)
      LangDef(ident, clause)

    override def visitConstDef(ctx: FlanParser.ConstDefContext): ConstDef =
      val ident = mkIdent(ctx.IDENT)
      val value = ctx.expr.accept(ExprVisitor)
      ConstDef(ident, value)

    override def visitMethodDef(ctx: FlanParser.MethodDefContext): MethodDef =
      val ident = mkIdent(ctx.IDENT)
      val params = mkParamList(ctx.paramList)
      val returnType =
        if ctx.`type` == null then TupleType(Nil)(getRange(ctx.paramList.CLOSE_PAREN))
        else ctx.`type`.accept(TypeVisitor)
      val specs = ctx.methodSpec.asScala.toList.map(_.accept(MethodSpecVisitor))
      val body = ctx.stmt.accept(StmtVisitor)
      MethodDef(ident, params, returnType, specs, body)

  private def mkIdent(node: TerminalNode): Ident = Ident(node.getText)(getRange(node))

  private def mkParam(ctx: FlanParser.ParamContext): Param =
    val ident = mkIdent(ctx.IDENT)
    val typ = ctx.`type`.accept(TypeVisitor)
    Param(ident, typ)

  private def mkParamList(ctx: FlanParser.ParamListContext): List[Param] =
    ctx.param.asScala.toList.map(mkParam)

  private object MethodSpecVisitor extends FlanParserBaseVisitor[MethodSpec]:
    override def visitRequireSpec(ctx: FlanParser.RequireSpecContext): RequireSpec =
      val cond = ctx.expr.accept(ExprVisitor)
      RequireSpec(cond)(getRange(ctx))

    override def visitEnsureSpec(ctx: FlanParser.EnsureSpecContext): EnsureSpec =
      val cond = ctx.expr.accept(ExprVisitor)
      EnsureSpec(cond)(getRange(ctx))

  private object TypeVisitor extends FlanParserBaseVisitor[Type]:
    override def visitTypeName(ctx: FlanParser.TypeNameContext): TypeName =
      val name = ctx.IDENT.getText
      TypeName(name)(getRange(ctx))

    override def visitParenType(ctx: FlanParser.ParenTypeContext): Type =
      if ctx.typeList == null then
        TupleType(Nil)(getRange(ctx))
      else
        val types = mkTypeList(ctx.typeList)
        if types.length == 1 then types.head else TupleType(types)(getRange(ctx))

    override def visitTypeApply(ctx: FlanParser.TypeApplyContext): TypeApply =
      val typeFun = ctx.`type`.accept(this)
      val typeArgs = mkTypeList(ctx.typeList)
      TypeApply(typeFun, typeArgs)(getRange(ctx))

    override def visitOptType(ctx: FlanParser.OptTypeContext): OptType =
      val typ = ctx.`type`.accept(this)
      OptType(typ)(getRange(ctx))

    override def visitUnionType(ctx: FlanParser.UnionTypeContext): UnionType =
      val left = ctx.`type`(0).accept(this)
      val right = ctx.`type`(1).accept(this)
      UnionType(left, right)(getRange(ctx))

    override def visitFunType(ctx: FlanParser.FunTypeContext): FunType =
      val paramTypes = ctx.`type`(0).accept(this) match
        case TupleType(ts) => ts
        case t => List(t)
      val returnType = ctx.`type`(1).accept(this)
      FunType(paramTypes, returnType)(getRange(ctx))

    private def mkTypeList(ctx: FlanParser.TypeListContext): List[Type] =
      ctx.`type`.asScala.toList.map(_.accept(this))

  private object ClauseVisitor extends FlanParserBaseVisitor[Clause]:
    override def visitCharClause(ctx: FlanParser.CharClauseContext): ConcreteCharSet =
      val c = unescapeChar(ctx.getText)
      ConcreteCharSet(Set(c))(getRange(ctx))

    override def visitStringClause(ctx: FlanParser.StringClauseContext): Clause =
      val s = unescapeString(ctx.getText)
      val range = getRange(ctx)
      val parts = s.map { c => ConcreteCharSet(Set(c))(range) }.toList
      parts.reduceRight { Concat(_, _)(range) }

    override def visitLangName(ctx: FlanParser.LangNameContext): LangName =
      val name = ctx.IDENT.getText
      LangName(name)(getRange(ctx))

    override def visitCharClassClause(ctx: FlanParser.CharClassClauseContext): CharSet = mkCharClass(ctx.charClass)

    override def visitRegExClause(ctx: FlanParser.RegExClauseContext): Clause = ctx.regEx.accept(RegExVisitor)

    override def visitParenClause(ctx: FlanParser.ParenClauseContext): Clause = ctx.clause.accept(this)

    override def visitRepeat(ctx: FlanParser.RepeatContext): Repeat =
      val part = ctx.clause.accept(this)
      val quant = ctx.quant.accept(QuantVisitor)
      Repeat(part, quant)(getRange(ctx))

    override def visitConcat(ctx: FlanParser.ConcatContext): Concat =
      val left = ctx.clause(0).accept(this)
      val right = ctx.clause(1).accept(this)
      Concat(left, right)(getRange(ctx))

    override def visitUnion(ctx: FlanParser.UnionContext): Union =
      val left = ctx.clause(0).accept(this)
      val right = ctx.clause(1).accept(this)
      Union(left, right)(getRange(ctx))

  private object QuantVisitor extends FlanParserBaseVisitor[Quant]:
    override def visitStar(ctx: FlanParser.StarContext): Star = Star()(getRange(ctx))

    override def visitPlus(ctx: FlanParser.PlusContext): Plus = Plus()(getRange(ctx))

    override def visitOpt(ctx: FlanParser.OptContext): Opt = Opt()(getRange(ctx))

    override def visitExactly(ctx: FlanParser.ExactlyContext): Exactly =
      val count = mkCount(ctx.INT)
      Exactly(count)(getRange(ctx))

    override def visitBetween(ctx: FlanParser.BetweenContext): Between =
      val min = mkCount(ctx.INT(0))
      val max = if ctx.INT.size == 2 then Some(mkCount(ctx.INT(1))) else None
      Between(min, max)(getRange(ctx))

    private def mkCount(node: TerminalNode): Int =
      val int = node.getText
      if int.startsWith("-") || int.startsWith("0x") || int.startsWith("0b") then
        throw new IllegalArgumentException(s"invalid quantifier: $int")
      int.toInt

  private object RegExVisitor extends FlanParserBaseVisitor[Clause]:
    override def visitRE_Char(ctx: FlanParser.RE_CharContext): ConcreteCharSet =
      val c = unescapeREChar(ctx.getText)
      ConcreteCharSet(Set(c))(getRange(ctx))

    override def visitRE_CharType(ctx: FlanParser.RE_CharTypeContext): CharSet =
      if ctx.getText == "." then AllChar()(getRange(ctx))
      else ConcreteCharSet(unescapeCharType(ctx.getText))(getRange(ctx))

    override def visitRE_CharClass(ctx: FlanParser.RE_CharClassContext): CharSet = mkCharClass(ctx.charClass)

    override def visitRE_Paren(ctx: FlanParser.RE_ParenContext): Clause = ctx.regEx.accept(this)

    override def visitRE_Repeat(ctx: FlanParser.RE_RepeatContext): Repeat =
      val part = ctx.regEx.accept(this)
      val quant = ctx.quant.accept(QuantVisitor)
      Repeat(part, quant)(getRange(ctx))

    override def visitRE_Concat(ctx: FlanParser.RE_ConcatContext): Concat =
      val left = ctx.regEx(0).accept(this)
      val right = ctx.regEx(1).accept(this)
      Concat(left, right)(getRange(ctx))

    override def visitRE_Union(ctx: FlanParser.RE_UnionContext): Union =
      val left = ctx.regEx(0).accept(this)
      val right = ctx.regEx(1).accept(this)
      Union(left, right)(getRange(ctx))

  private def unescapeREChar(source: String): Char =
    if source.startsWith("\\") then
      source.charAt(1) match
        case 't' => '\t'
        case 'n' => '\n'
        case 'r' => '\r'
        case 'f' => '\f'
        case 'c' => (source.charAt(2).toLower - 'a' + 1).toChar
        case 'x' | 'u' => Integer.parseInt(source.substring(2), 16).toChar
        case c => c // identity escape
    else
      assert(source.length == 1)
      source.charAt(0)

  private object CharClassAtomVisitor extends FlanParserBaseVisitor[CharSet]:
    override def visitCC_Char(ctx: FlanParser.CC_CharContext): ConcreteCharSet =
      val c = unescapeChar(ctx.getText)
      ConcreteCharSet(Set(c))(getRange(ctx))

    override def visitCC_Range(ctx: FlanParser.CC_RangeContext): CharRange =
      val c1 = unescapeChar(ctx.CC_CHAR(0).getText)
      val c2 = unescapeChar(ctx.CC_CHAR(1).getText)
      CharRange(c1, c2)(getRange(ctx))

    override def visitCC_CharType(ctx: FlanParser.CC_CharTypeContext): CharSet =
      ConcreteCharSet(unescapeCharType(ctx.getText))(getRange(ctx))

  private def unescapeCharType(source: String): Set[Char] =
    assert(source.startsWith("\\"))
    val set = UnicodeSet(source)
    set.codePoints.asScala.toSet.map(_.toChar)

  private def mkCharClass(ctx: FlanParser.CharClassContext): CharClass =
    val isPos = !ctx.CC_BEGIN.getText.endsWith("^")
    val items = ctx.charClassAtom.asScala.toList.map(_.accept(CharClassAtomVisitor))
    CharClass(isPos, items)(getRange(ctx))

  private object ExprVisitor extends FlanParserBaseVisitor[Expr]:
    override def visitConst(ctx: FlanParser.ConstContext): Const =
      val literal = ctx.literal.accept(LiteralVisitor)
      Const(literal)(getRange(ctx))

    override def visitTermName(ctx: FlanParser.TermNameContext): TermName =
      val name = ctx.IDENT.getText
      TermName(name)(getRange(ctx))

    override def visitParenExpr(ctx: FlanParser.ParenExprContext): Expr =
      val exprs = mkExprList(ctx.exprList)
      if exprs.length == 1 then exprs.head else TupleExpr(exprs)(getRange(ctx))

    private def mkExprList(ctx: FlanParser.ExprListContext): List[Expr] =
      ctx.expr.asScala.toList.map(_.accept(this))

    override def visitAnnotExpr(ctx: FlanParser.AnnotExprContext): AnnotExpr =
      val expr = ctx.expr.accept(this)
      val typ = ctx.`type`.accept(TypeVisitor)
      AnnotExpr(expr, typ)(getRange(ctx))

    override def visitMemberAccess(ctx: FlanParser.MemberAccessContext): MemberAccess =
      val receiver = ctx.expr.accept(this)
      val member = mkIdent(ctx.IDENT)
      MemberAccess(receiver, member)(getRange(ctx))

    override def visitApply(ctx: FlanParser.ApplyContext): Apply =
      val fun = ctx.expr.accept(this)
      val args = mkExprList(ctx.exprList)
      Apply(fun, args)(getRange(ctx))

    def mkOperation(opName: String, opRange: Range, receiver: Expr, args: List[Expr], range: Range): Apply =
      val member = Ident(opName)(opRange)
      val fun = MemberAccess(receiver, member)(
        if args.isEmpty then range else Range(receiver.range.getStart, opRange.getEnd))
      Apply(fun, args)(range)

    override def visitAt(ctx: FlanParser.AtContext): Apply =
      mkOperation("at", getRange(ctx.OPEN_SQUARE), ctx.expr(0).accept(this), List(ctx.expr(1).accept(this)),
        getRange(ctx))

    override def visitPrefixExpr(ctx: FlanParser.PrefixExprContext): Apply =
      mkOperation("prefix_" + ctx.op.getText, getRange(ctx.op), ctx.expr.accept(this), Nil, getRange(ctx))

    override def visitInfixExpr(ctx: FlanParser.InfixExprContext): Apply =
      mkOperation(ctx.op.getText, getRange(ctx.op), ctx.expr(0).accept(this), List(ctx.expr(1).accept(this)),
        getRange(ctx))

    override def visitRelExpr(ctx: FlanParser.RelExprContext): Expr =
      val exprs = ctx.expr.asScala.toList.map(_.accept(this))
      val ops = ctx.relOp.asScala.toList
      if ops.length >= 2 && (ops.forall(isAsc) || ops.forall(isDesc)) then
        RelExpr(ops.map(op => Ident(op.getText)(getRange(op))), exprs)(getRange(ctx))
      else
        ops.zip(exprs.tail).foldLeft(exprs.head) { case (e1, (op, e2)) => mkRel(op, e1, e2) }

    private def isAsc(op: FlanParser.RelOpContext): Boolean = op.LE != null || op.LT != null

    private def isDesc(op: FlanParser.RelOpContext): Boolean = op.GE != null || op.GT != null

    private def mkRel(ctx: FlanParser.RelOpContext, left: Expr, right: Expr): Apply =
      val range = Range(left.range.getStart, right.range.getEnd)
      if ctx.IN != null then
        val expr = mkOperation("contains", getRange(ctx.IN), left, List(right), range)
        if ctx.NOT == null then expr else mkOperation("!", getRange(ctx.NOT), expr, Nil, range)
      else
        mkOperation(ctx.getText, getRange(ctx), left, List(right), range)

    override def visitArrowExpr(ctx: FlanParser.ArrowExprContext): TupleExpr =
      val fst = ctx.expr(0).accept(this)
      val snd = ctx.expr(1).accept(this)
      TupleExpr(List(fst, snd))(getRange(ctx))

    override def visitCondExpr(ctx: FlanParser.CondExprContext): CondExpr =
      val cond = ctx.expr(0).accept(this)
      val thenValue = ctx.expr(1).accept(this)
      val elseValue = ctx.expr(2).accept(this)
      CondExpr(cond, thenValue, elseValue)(getRange(ctx))

    override def visitLambda(ctx: FlanParser.LambdaContext): Lambda =
      val params = mkParamList(ctx.paramList)
      val returnValue = ctx.expr.accept(this)
      Lambda(params, returnValue)(getRange(ctx))

  private object LiteralVisitor extends FlanParserBaseVisitor[Literal]:
    override def visitNull(ctx: FlanParser.NullContext): Literal = null

    override def visitTrue(ctx: FlanParser.TrueContext): Literal = true

    override def visitFalse(ctx: FlanParser.FalseContext): Literal = false

    override def visitInt(ctx: FlanParser.IntContext): Literal =
      val isNeg = ctx.getText.startsWith("-")
      val value = if isNeg then ctx.getText.substring(1) else ctx.getText
      if value.startsWith("0x") then BigInt(value.substring(2), 16) * (if isNeg then -1 else 1)
      else if value.startsWith("0b") then BigInt(value.substring(2), 2) * (if isNeg then -1 else 1)
      else BigInt(value) * (if isNeg then -1 else 1)

    override def visitChar(ctx: FlanParser.CharContext): Literal = unescapeChar(ctx.getText)

    override def visitString(ctx: FlanParser.StringContext): Literal = unescapeString(ctx.getText)

  private def unescapeChar(source: String): Char =
    assert(source.startsWith("'") && source.endsWith("'"))
    val s = StringEscapeUtils.unescapeJava(source.substring(1, source.length - 1))
    assert(s.length == 1)
    s.charAt(0)

  private def unescapeString(source: String): String =
    assert(source.startsWith("\"") && source.endsWith("\""))
    StringEscapeUtils.unescapeJava(source.substring(1, source.length - 1))

  private object StmtVisitor extends FlanParserBaseVisitor[Stmt]:
    override def visitDeclare(ctx: FlanParser.DeclareContext): Declare =
      val ident = mkIdent(ctx.IDENT)
      val typ = Option(ctx.`type`).map(_.accept(TypeVisitor))
      val value = Option(ctx.value).map(mkValue)
      Declare(ident, typ, value)(getRange(ctx))

    private def mkValue(ctx: FlanParser.ValueContext): Expr | UnknownValue =
      if ctx.expr != null then ctx.expr.accept(ExprVisitor)
      else UnknownValue()(getRange(ctx))

    override def visitAssign(ctx: FlanParser.AssignContext): Assign =
      val ident = mkIdent(ctx.IDENT)
      val value = mkValue(ctx.value)
      Assign(ident, value)(getRange(ctx))

    override def visitAugAssign(ctx: FlanParser.AugAssignContext): Assign =
      val ident = mkIdent(ctx.IDENT)
      val delta = ctx.value.accept(ExprVisitor)
      val value = ExprVisitor.mkOperation(ctx.augOp.getText, getRange(ctx.augOp), TermName(ident.value)(ident.range),
        List(delta), getRange(ctx))
      Assign(ident, value)(getRange(ctx))

    override def visitAssume(ctx: FlanParser.AssumeContext): Assume =
      val cond = ctx.expr.accept(ExprVisitor)
      Assume(cond)(getRange(ctx))

    override def visitAssert(ctx: FlanParser.AssertContext): Assert =
      val cond = ctx.expr.accept(ExprVisitor)
      Assert(cond)(getRange(ctx))

    override def visitReturn(ctx: FlanParser.ReturnContext): Return =
      val value = Option(ctx.expr).map(_.accept(ExprVisitor))
      Return(value)(getRange(ctx))

    override def visitIf(ctx: FlanParser.IfContext): If =
      val cond = ctx.expr.accept(ExprVisitor)
      val thenBody = ctx.stmt(0).accept(this)
      val elseBody =
        if ctx.stmt.size == 2 then ctx.stmt(1).accept(this)
        else StmtList(Nil)(getLastCharRange(thenBody.range))
      If(cond, thenBody, elseBody)(getRange(ctx))

    override def visitWhile(ctx: FlanParser.WhileContext): While =
      val cond = ctx.expr.accept(ExprVisitor)
      val invariants = ctx.loopSpec.asScala.toList.map(_.expr.accept(ExprVisitor))
      val body = ctx.stmt.accept(this)
      While(cond, invariants, body)(getRange(ctx))

    override def visitFor(ctx: FlanParser.ForContext): For =
      val ident = mkIdent(ctx.IDENT)
      val iterable = ctx.expr.accept(ExprVisitor)
      val body = ctx.stmt.accept(this)
      For(ident, iterable, body)(getRange(ctx))

    override def visitBreak(ctx: FlanParser.BreakContext): Break =
      Break()(getRange(ctx))

    override def visitContinue(ctx: FlanParser.ContinueContext): Continue =
      Continue()(getRange(ctx))

    override def visitStmtList(ctx: FlanParser.StmtListContext): Stmt =
      val stmts = ctx.stmt.asScala.toList.map(_.accept(this))
      StmtList(stmts)(getRange(ctx))

  // Position and range utilities
  private def getStart(token: Token): Position =
    Position(token.getLine - 1, token.getCharPositionInLine)

  private def getEnd(token: Token): Position =
    val lines = token.getText.count(_ == '\n')
    if lines == 0 then
      Position(token.getLine - 1, token.getCharPositionInLine + token.getText.length)
    else
      Position(token.getLine - 1 + lines, token.getText.length - token.getText.lastIndexOf('\n') - 1)

  private def getRange(token: Token): Range = Range(getStart(token), getEnd(token))

  private def getRange(ctx: ParserRuleContext): Range = Range(getStart(ctx.getStart), getEnd(ctx.getStop))

  private def getLastCharRange(range: Range): Range =
    Range(Position(range.getEnd.getLine, range.getEnd.getCharacter - 1), range.getEnd)
