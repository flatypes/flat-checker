package flat.checker

object ast:
  trait Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T

  final case class FunDef(ident: Ident, paramTypes: Seq[ast.Type], returnType: ast.Type,
                          varTypes: Seq[ast.Type], body: Stmt) extends Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitFunDef(this, ctx)

  final case class Ident(name: String) extends Locational

  sealed trait Type:
    def ignoreHint: Type = this

    def toSort: Sort

  case object AnyType extends Type:
    def toSort: Sort = Sort.Top

  case object NoType extends Type:
    def toSort: Sort = Sort.Bot

  final case class IntervalType(interval: Interval) extends Type:
    def toSort: Sort = Sort.Int

  val intType: Type = IntervalType(Interval.full)

  final case class TernaryType(value: Ternary) extends Type:
    def toSort: Sort = Sort.Bool

  val boolType: Type = TernaryType(Ternary.Maybe)

  final case class LangType(reLang: ReLang) extends Type:
    def toSort: Sort = Sort.String

  val stringType: Type = LangType(ReLang.full)

  def literalType(value: Int | Boolean | String): Type = value match
    case i: Int => IntervalType(i)
    case b: Boolean => TernaryType(b)
    case s: String => LangType(s)

  final case class ArrayType(elem: Type) extends Type:
    def toSort: Sort = Sort.Array(elem.toSort)

  final case class FunType(args: Seq[Type], returns: Type) extends Type:
    def toSort: Sort = Sort.Fun(args.map(_.toSort), returns.toSort)

  final case class HintType(hint: Hint) extends Type:
    override def ignoreHint: Type = hint.toType

    def toSort: Sort = hint.toType.toSort

  given fromSort: Conversion[Sort, Type] = {
    case Sort.Top => AnyType
    case Sort.Bot => NoType
    case Sort.Int => intType
    case Sort.Bool => boolType
    case Sort.String => stringType
    case Sort.Array(s) => ArrayType(s)
    case Sort.Fun(ss, s) => FunType(ss.map(fromSort), s)
  }

  sealed trait Stmt extends Node

  final case class Assign(id: Int, value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  final case class Assert(cond: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Return(value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class IfStmt(cond: Expr, body: Stmt, elseBody: Stmt) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfStmt(this, ctx)

  final case class While(cond: Expr, body: Stmt, invariants: Seq[Invariant] = Seq.empty) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  final case class Invariant(id: Int, typ: Type) extends Locational

  final case class StmtBlock(body: Seq[Stmt]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitStmtBlock(this, ctx)

  sealed trait Expr extends Node, Locational

  final case class Literal(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLiteral(this, ctx)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

  final case class LocalRef(id: Int) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLocalRef(this, ctx)

  final case class IfExpr(cond: Expr, body: Expr, elseBody: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExpr(this, ctx)

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

  final case class ApplyOp(op: Op, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApplyOp(this, ctx)

  def apply(op: Op, args: Expr*): Expr = ApplyOp(op, args.toSeq)

  val NoExpr: Expr = GlobalRef("")

  enum Op:
    case EQ
    case NE
    // integer arithmetic
    case ADD
    case SUB
    // integer comparison
    case LE
    case LT
    case GE
    case GT
    // Boolean
    case AND
    case OR
    case NOT
    // Char
    case CHAR_TO_CODE
    case CHAR_FROM_CODE
    // string basic
    case CONCAT
    case REVERSE
    case STR_LEN
    case STR_AT
    case SUBSTR
    case INDEX_OF
    case SPLIT
    // string test
    case STARTS_WITH
    case ENDS_WITH
    case CONTAINS
    // string conversion
    case STR_TO_INT
    case STR_FROM_INT
    // array
    case ARRAY_MK
    case ARRAY_AT
    case ARRAY_UPDATE
    case ARRAY_CONTAINS
    case ARRAY_FORALL_TRUE
    case ARRAY_EXISTS_TRUE

  trait NodeVisitor[C, T]:
    def visitNode(node: Node, ctx: C): T =
      throw UnsupportedOperationException(s"visit ${node.getClass.getCanonicalName}")

    // def
    def visitFunDef(node: FunDef, ctx: C): T = visitNode(node, ctx)

    // stmt
    def visitStmt(node: Stmt, ctx: C): T = visitNode(node, ctx)

    def visitAssign(node: Assign, ctx: C): T = visitStmt(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitStmt(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitStmt(node, ctx)

    def visitIfStmt(node: IfStmt, ctx: C): T = visitStmt(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitStmt(node, ctx)

    def visitStmtBlock(node: StmtBlock, ctx: C): T = visitStmt(node, ctx)

    // expr
    def visitExpr(node: Expr, ctx: C): T = visitNode(node, ctx)

    def visitLiteral(node: Literal, ctx: C): T = visitExpr(node, ctx)

    def visitGlobalRef(node: GlobalRef, ctx: C): T = visitExpr(node, ctx)

    def visitLocalRef(node: LocalRef, ctx: C): T = visitExpr(node, ctx)

    def visitApplyOp(node: ApplyOp, ctx: C): T =
      node.op match
        case Op.EQ => visitEqual(node, ctx)
        case Op.NE => visitNotEqual(node, ctx)
        case Op.ADD => visitAdd(node, ctx)
        case Op.SUB => visitSub(node, ctx)
        case Op.LE => visitLessEqual(node, ctx)
        case Op.LT => visitLessThan(node, ctx)
        case Op.GE => visitGreaterEqual(node, ctx)
        case Op.GT => visitGreaterThan(node, ctx)
        case Op.AND => visitAnd(node, ctx)
        case Op.OR => visitOr(node, ctx)
        case Op.NOT => visitNot(node, ctx)
        case Op.CHAR_TO_CODE => visitCharToCode(node, ctx)
        case Op.CHAR_FROM_CODE => visitCharFromCode(node, ctx)
        case Op.CONCAT => visitStringConcat(node, ctx)
        case Op.REVERSE => visitStringReverse(node, ctx)
        case Op.STR_LEN => visitStringLength(node, ctx)
        case Op.STR_AT => visitStringAt(node, ctx)
        case Op.SUBSTR => visitSubstring(node, ctx)
        case Op.INDEX_OF => visitStringIndexOf(node, ctx)
        case Op.SPLIT => visitStringSplit(node, ctx)
        case Op.STARTS_WITH => visitStringStartsWith(node, ctx)
        case Op.ENDS_WITH => visitStringEndsWith(node, ctx)
        case Op.CONTAINS => visitStringContains(node, ctx)
        case Op.STR_TO_INT => visitStringToInt(node, ctx)
        case Op.STR_FROM_INT => visitStringFromInt(node, ctx)
        case Op.ARRAY_MK => visitNewArray(node, ctx)
        case Op.ARRAY_AT => visitArrayAt(node, ctx)
        case Op.ARRAY_UPDATE => visitArrayUpdate(node, ctx)
        case Op.ARRAY_CONTAINS => visitArrayContains(node, ctx)
        case Op.ARRAY_FORALL_TRUE => visitArrayForallTrue(node, ctx)
        case Op.ARRAY_EXISTS_TRUE => visitArrayExistsTrue(node, ctx)

    def visitApply(node: Apply, ctx: C): T = visitExpr(node, ctx)

    def visitIfExpr(node: IfExpr, ctx: C): T = visitExpr(node, ctx)

    // apply op
    def visitEqual(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitNotEqual(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitAdd(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitSub(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitLessEqual(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitLessThan(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitGreaterEqual(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitGreaterThan(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitAnd(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitOr(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitNot(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitCharToCode(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitCharFromCode(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringConcat(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringReverse(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringLength(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringAt(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitSubstring(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringIndexOf(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringSplit(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringStartsWith(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringEndsWith(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringContains(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringToInt(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitStringFromInt(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitNewArray(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitArrayAt(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitArrayUpdate(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitArrayContains(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitArrayForallTrue(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)

    def visitArrayExistsTrue(node: ApplyOp, ctx: C): T = visitExpr(node, ctx)
