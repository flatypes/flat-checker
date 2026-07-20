package flat.checker.parsing

import flat.antlr.{FlanLexer, FlanParser, FlanParserBaseVisitor}
import flat.checker.flan.untpd.*
import flat.checker.flan.{Literal, parseInt, unescape}
import flat.checker.{Reporter, Source}
import flat.regex.REParser
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{Position, Range}

import scala.jdk.CollectionConverters.*

class Parser(using reporter: Reporter):
  def parse(source: Source): Program =
    // Setup lexer
    val stream = CharStreams.fromString(source.text, source.uri)
    val lexer = FlanLexer(stream)
    lexer.removeErrorListeners()
    lexer.addErrorListener(ErrorListener())
    // Setup parser
    val tokens = CommonTokenStream(lexer)
    val parser = FlanParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ErrorListener())
    // Process the parse tree
    val tree = parser.program
    if reporter.hasError then Program(Nil)
    else Program(tree.topDef.asScala.toList.map(_.accept(TopDefVisitor)))

  private class ErrorListener(using reporter: Reporter) extends BaseErrorListener:
    override def syntaxError(recognizer: Recognizer[?, ?], offendingSymbol: Any,
                             line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
      val desc = e match
        case e: InputMismatchException =>
          s"unexpected token (expected ${e.getExpectedTokens.toString(recognizer.getVocabulary)})"
        case _ => s"unexpected token ($msg)"
      val range = offendingSymbol match
        case token: Token => getRange(token)
        case _ => Range(Position(line - 1, charPositionInLine), Position(line - 1, charPositionInLine + 1))
      reporter.reportSyntaxError(desc, range)

  private object TopDefVisitor extends FlanParserBaseVisitor[TopDef]:
    override def visitMethodDef(node: FlanParser.MethodDefContext): TopDef =
      val ident = toIdent(node.IDENT)
      val params = toParamList(node.paramList)
      val returnType = Option(node.`type`).map(_.accept(TypeVisitor))
      val requires = node.requiresSpec.asScala.toList.map(_.expr.accept(ExprVisitor))
      val ensures = node.ensuresSpec.asScala.toList.map(_.expr.accept(ExprVisitor))
      val body = Option(node.block).map(StmtVisitor.toStmtList)
      val endRange = if node.block != null then getRange(node.block.CLOSE_CURLY) else null
      MethodDef(ident, params, returnType, requires, ensures, body)(endRange)

    def toParamList(node: FlanParser.ParamListContext): List[Param] = node.param.asScala.toList.map(toParam)

    private def toParam(node: FlanParser.ParamContext): Param =
      val ident = toIdent(node.IDENT)
      val typ = node.`type`.accept(TypeVisitor)
      Param(ident, typ)

    override def visitConstDef(node: FlanParser.ConstDefContext): TopDef =
      val ident = toIdent(node.IDENT)
      val value = node.expr.accept(ExprVisitor)
      ConstDef(ident, value)

    override def visitTypeDef(node: FlanParser.TypeDefContext): TopDef =
      val ident = toIdent(node.IDENT)
      val value = node.`type`.accept(TypeVisitor)
      TypeDef(ident, value)

    override def visitLangDef(node: FlanParser.LangDefContext): TopDef =
      val ident = toIdent(node.IDENT)
      val value = node.lang.accept(LangVisitor)
      LangDef(ident, value)

    override def visitChildren(node: RuleNode): TopDef =
      throw NotImplementedError(s"TopDefVisitor.visit${node.getClass.getSimpleName}")

  private object StmtVisitor extends FlanParserBaseVisitor[Stmt]:
    override def visitVarStmt(node: FlanParser.VarStmtContext): Stmt =
      val ident = toIdent(node.IDENT)
      val typ = Option(node.`type`).map(_.accept(TypeVisitor))
      val value = if node.exprOrNondet != null then toExprOrNondet(node.exprOrNondet) else Nondet()(getRange(node))
      VarStmt(ident, typ, value)

    private def toExprOrNondet(node: FlanParser.ExprOrNondetContext): Expr | Nondet =
      if node.expr != null then node.expr.accept(ExprVisitor) else Nondet()(getRange(node))

    override def visitAssign(node: FlanParser.AssignContext): Stmt =
      val ident = toIdent(node.IDENT)
      val value = toExprOrNondet(node.exprOrNondet)
      Assign(ident, value)

    override def visitAugAssign(node: FlanParser.AugAssignContext): Stmt =
      val ident = toIdent(node.IDENT)
      val delta = node.expr.accept(ExprVisitor)
      val op = Ident(node.augAssignOp.getText.dropRight(1))(getRange(node.augAssignOp))
      val value = mkApply(TermName(ident.name)(getRange(node.IDENT)), op, delta)(getRange(node))
      Assign(ident, value)

    override def visitExprStmt(node: FlanParser.ExprStmtContext): Stmt =
      val expr = node.expr.accept(ExprVisitor)
      ExprStmt(expr)

    override def visitReturn(node: FlanParser.ReturnContext): Stmt =
      val value = Option(node.expr).map(_.accept(ExprVisitor))
      Return(value)(getRange(node.RETURN))

    override def visitIf(node: FlanParser.IfContext): Stmt =
      val branches = for b <- node.ifBranch.asScala.toList yield
        val cond = toExprOrNondet(b.guard)
        val thenBody = toStmtList(b.block)
        (cond, thenBody)
      val elseBody = if node.block != null then toStmtList(node.block) else Nil
      val lastIf = If(branches.last._1, branches.last._2, elseBody)
      branches.dropRight(1).foldRight(lastIf) { (b, s) => If(b._1, b._2, List(s)) }

    private def toExprOrNondet(node: FlanParser.GuardContext): Expr | Nondet =
      if node.expr != null then node.expr.accept(ExprVisitor) else Nondet()(getRange(node))

    def toStmtList(node: FlanParser.BlockContext): List[Stmt] = node.stmt.asScala.toList.map(_.accept(this))

    override def visitWhile(node: FlanParser.WhileContext): Stmt =
      val cond = toExprOrNondet(node.guard)
      val invariants = node.invariantSpec.asScala.toList.map(_.expr.accept(ExprVisitor))
      val body = toStmtList(node.block)
      While(cond, invariants, body)

    override def visitBreak(node: FlanParser.BreakContext): Stmt =
      Break()(getRange(node.BREAK))

    override def visitContinue(node: FlanParser.ContinueContext): Stmt =
      Continue()(getRange(node.CONTINUE))

    override def visitAssume(node: FlanParser.AssumeContext): Stmt =
      val cond = node.expr.accept(ExprVisitor)
      Assume(cond)

    override def visitAssert(node: FlanParser.AssertContext): Stmt =
      val cond = node.expr.accept(ExprVisitor)
      Assert(cond)

    override def visitChildren(node: RuleNode): Stmt =
      throw NotImplementedError(s"StmtVisitor.visit${node.getClass.getSimpleName}")

  private object ExprVisitor extends FlanParserBaseVisitor[Expr]:
    override def visitConst(node: FlanParser.ConstContext): Expr =
      val value = toLiteral(node.literal)
      Const(value)(getRange(node))

    private def toLiteral(node: FlanParser.LiteralContext): Literal =
      if node.NULL != null then null
      else if node.TRUE != null then true
      else if node.FALSE != null then false
      else if node.INT_LITERAL != null then parseInt(node.getText)
      else if node.CHAR_LITERAL != null then unescape(node.getText.drop(1).dropRight(1)).head
      else unescape(node.getText.drop(1).dropRight(1))

    override def visitTermName(node: FlanParser.TermNameContext): Expr =
      val name = node.IDENT.getText
      TermName(name)(getRange(node))

    override def visitSeqExpr(node: FlanParser.SeqExprContext): Expr =
      val args = toExprList(node.exprList)
      SeqExpr(args)(getRange(node))

    private def toExprList(node: FlanParser.ExprListContext): List[Expr] =
      node.expr.asScala.toList.map(_.accept(this))

    override def visitSetExpr(node: FlanParser.SetExprContext): Expr =
      val args = toExprList(node.exprList)
      SetExpr(args)(getRange(node))

    override def visitMapExpr(node: FlanParser.MapExprContext): Expr =
      val items = node.itemList.item.asScala.toList.map(i => (i.expr(0).accept(this), i.expr(1).accept(this)))
      val (keys, vals) = items.unzip
      MapExpr(keys, vals)(getRange(node))

    override def visitParenExpr(node: FlanParser.ParenExprContext): Expr =
      val exprs = toExprList(node.exprList)
      if exprs.length == 1 then exprs.head else TupleExpr(exprs)(getRange(node))

    override def visitSize(node: FlanParser.SizeContext): Expr =
      val expr = node.expr.accept(this)
      mkUnary(expr, Ident("size")(getRange(node.VERT(0))))(getRange(node))

    override def visitAccess(node: FlanParser.AccessContext): Expr =
      val receiver = node.expr.accept(this)
      val member = toIdent(node.IDENT)
      Access(receiver, member)(getRange(node))

    override def visitApply(node: FlanParser.ApplyContext): Expr =
      val fun = node.expr.accept(this)
      val args = toExprList(node.exprList)
      Apply(fun, args)(getRange(node))

    override def visitSelect(node: FlanParser.SelectContext): Expr =
      val receiver = node.expr(0).accept(this)
      val arg = node.expr(1).accept(this)
      mkApply(receiver, Ident("select")(getRange(node.OPEN_SQUARE)), arg)(getRange(node))

    override def visitSlice(node: FlanParser.SliceContext): Expr =
      val receiver = node.expr.accept(this)
      val op = Ident("slice")(getRange(node.OPEN_SQUARE))
      val start =
        if node.range.start == null then Const(0)(getRange(node.OPEN_SQUARE))
        else node.range.start.accept(this)
      if node.range.end == null then
        mkApply(receiver, op, start)(getRange(node))
      else
        val end = node.range.end.accept(this)
        mkApply(receiver, op, start, end)(getRange(node))

    override def visitUpdate(node: FlanParser.UpdateContext): Expr =
      val receiver = node.expr(0).accept(this)
      val op = Ident("update")(getRange(node.OPEN_SQUARE))
      val index = node.expr(1).accept(this)
      val value = node.expr(2).accept(this)
      mkApply(receiver, op, index, value)(getRange(node))

    override def visitPrefixExpr(node: FlanParser.PrefixExprContext): Expr =
      val operand = node.expr.accept(this)
      mkUnary(operand, Ident("prefix_" + node.op.getText)(getRange(node.op)))(getRange(node))

    override def visitInfixExpr(node: FlanParser.InfixExprContext): Expr =
      val left = node.expr(0).accept(this)
      val right = node.expr(1).accept(this)
      mkApply(left, Ident(node.op.getText)(getRange(node.op)), right)(getRange(node))

    override def visitRelExpr(node: FlanParser.RelExprContext): Expr =
      val left = node.expr(0).accept(this)
      val right = node.expr(1).accept(this)
      if node.relOp.EQ != null then Eq(left, right)(getRange(node))
      else if node.relOp.NE != null then Ne(left, right)(getRange(node))
      else if node.relOp.IN != null then
        val expr = mkApply(right, Ident("contains")(getRange(node.relOp.IN)), left)(getRange(node))
        if node.relOp.NOT == null then expr else mkNot(expr)(expr.range, getRange(node.relOp.NOT))
      else
        mkApply(left, Ident(node.relOp.getText)(getRange(node.relOp)), right)(getRange(node))

    override def visitIteExpr(node: FlanParser.IteExprContext): Expr =
      val cond = node.expr(0).accept(this)
      val thenValue = node.expr(1).accept(this)
      val elseValue = node.expr(2).accept(this)
      Ite(cond, thenValue, elseValue)(getRange(node))

    override def visitChildren(node: RuleNode): Expr =
      throw NotImplementedError(s"ExprVisitor.visit${node.getClass.getSimpleName}")

  private object TypeVisitor extends FlanParserBaseVisitor[Type]:
    override def visitNullType(node: FlanParser.NullTypeContext): Type = NullType

    override def visitBoolType(node: FlanParser.BoolTypeContext): Type = BoolType

    override def visitIntType(node: FlanParser.IntTypeContext): Type = IntType

    override def visitCharType(node: FlanParser.CharTypeContext): Type = CharType

    override def visitStringType(node: FlanParser.StringTypeContext): Type = stringType

    override def visitSeqType(node: FlanParser.SeqTypeContext): Type =
      val elemType = node.`type`.accept(this)
      SeqType(elemType)

    override def visitSetType(node: FlanParser.SetTypeContext): Type =
      val elemType = node.`type`.accept(this)
      SetType(elemType)

    override def visitMapType(node: FlanParser.MapTypeContext): Type =
      val keyType = node.`type`(0).accept(this)
      val valueType = node.`type`(1).accept(this)
      MapType(keyType, valueType)

    override def visitTypeName(node: FlanParser.TypeNameContext): Type =
      TypeName(node.IDENT.getText)(getRange(node))

    override def visitParenType(node: FlanParser.ParenTypeContext): Type =
      val types = node.`type`.asScala.toList.map(_.accept(this))
      if types.length == 1 then types.head else TupleType(types)

    override def visitOptType(node: FlanParser.OptTypeContext): Type =
      val elemType = node.`type`.accept(this)
      UnionType(elemType, NullType)

    override def visitUnionType(node: FlanParser.UnionTypeContext): Type =
      val left = node.`type`(0).accept(this)
      val right = node.`type`(1).accept(this)
      UnionType(left, right)

    override def visitFunType(node: FlanParser.FunTypeContext): Type =
      val returnType = node.`type`.asScala.toList.last.accept(this)
      node.`type`(0).accept(this) match
        case TupleType(ts) => FunType(ts, returnType)
        case t => FunType(List(t), returnType)

    override def visitChildren(node: RuleNode): Type =
      throw NotImplementedError(s"TypeVisitor.visit${node.getClass.getSimpleName}")

  private object LangVisitor extends FlanParserBaseVisitor[Lang]:
    override def visitSingletonLang(node: FlanParser.SingletonLangContext): Lang =
      val value =
        if node.CHAR_LITERAL != null then unescape(node.CHAR_LITERAL.getText.drop(1).dropRight(1))
        else unescape(node.STRING_LITERAL.getText.drop(1).dropRight(1))
      LangConst(value)(getRange(node))

    override def visitLangName(node: FlanParser.LangNameContext): Lang =
      LangName(node.IDENT.getText)(getRange(node))

    override def visitRegexLang(node: FlanParser.RegexLangContext): Lang =
      val pattern = node.REGEX_LITERAL.getText.drop(2).dropRight(1)
      val regEx = REParser.tryParse(pattern, Map.empty) match
        case Left(msg) =>
          reporter.reportSyntaxError(msg, getRange(node.REGEX_LITERAL))
          flat.regex.RegEx.RENone
        case Right(r) => r
      RegEx(regEx)

    override def visitParenLang(node: FlanParser.ParenLangContext): Lang = node.lang.accept(this)

    override def visitLangStar(node: FlanParser.LangStarContext): Lang =
      val lang = node.lang.accept(this)
      LangStar(lang)(getRange(node))

    override def visitLangPlus(node: FlanParser.LangPlusContext): Lang =
      val lang = node.lang.accept(this)
      LangPlus(lang)(getRange(node))

    override def visitLangOpt(node: FlanParser.LangOptContext): Lang =
      val lang = node.lang.accept(this)
      LangOpt(lang)(getRange(node))

    override def visitLangPower(node: FlanParser.LangPowerContext): Lang =
      val lang = node.lang.accept(this)
      val exp = parseInt(node.INT_LITERAL.getText)
      LangPower(lang, exp)(getRange(node))

    override def visitLangLoop(node: FlanParser.LangLoopContext): Lang =
      val lang = node.lang.accept(this)
      val min = parseInt(node.INT_LITERAL(0).getText)
      val max = if node.INT_LITERAL.size() > 1 then Some(parseInt(node.INT_LITERAL(1).getText)) else None
      LangLoop(lang, min, max)(getRange(node))

    override def visitLangConcat(node: FlanParser.LangConcatContext): Lang =
      val left = node.lang(0).accept(this)
      val right = node.lang(1).accept(this)
      LangConcat(left, right)(getRange(node))

    override def visitLangUnion(node: FlanParser.LangUnionContext): Lang =
      val left = node.lang(0).accept(this)
      val right = node.lang(1).accept(this)
      LangUnion(left, right)(getRange(node))

    override def visitChildren(node: RuleNode): Lang =
      throw NotImplementedError(s"LangVisitor.visit${node.getClass.getSimpleName}")

  private def toIdent(node: TerminalNode): Ident = Ident(node.getText)(getRange(node))

  // Range and position utilities
  def getStart(token: Token): Position =
    Position(token.getLine - 1, token.getCharPositionInLine)

  def getEnd(token: Token): Position =
    val lines = token.getText.count(_ == '\n')
    if lines == 0 then
      Position(token.getLine - 1, token.getCharPositionInLine + token.getText.length)
    else
      Position(token.getLine - 1 + lines, token.getText.length - token.getText.lastIndexOf('\n') - 1)

  def getRange(token: Token): Range = Range(getStart(token), getEnd(token))

  def getRange(node: TerminalNode): Range = getRange(node.getSymbol)

  def getRange(node: ParserRuleContext): Range = Range(getStart(node.getStart), getEnd(node.getStop))