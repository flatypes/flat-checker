package flat.checker

object ast:
  trait Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T

  final case class FunDef(ident: Ident, params: Seq[(String, Type)], returnType: Type, body: Stmt) extends Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitFunDef(this, ctx)

  final case class Ident(name: String)

  sealed trait Type:
    def toSort: Sort

  case object AnyType extends Type:
    def toSort: Sort = Sort.Top

  case object NoType extends Type:
    def toSort: Sort = Sort.Bot

  case object IntType extends Type:
    def toSort: Sort = Sort.Int

  case object BoolType extends Type:
    def toSort: Sort = Sort.Bool

  case object StringType extends Type:
    def toSort: Sort = Sort.String

  final case class LiteralType(value: Int | Boolean | String) extends Type:
    def toSort: Sort = value match
      case _: Int => Sort.Int
      case _: Boolean => Sort.Bool
      case _: String => Sort.String

  final case class ArrayType(elem: Type) extends Type:
    def toSort: Sort = Sort.Array(elem.toSort)

  final case class FunType(args: Seq[Type], returns: Type) extends Type:
    def toSort: Sort = Sort.Fun(args.map(_.toSort), returns.toSort)

  final case class RangeType(lb: Bound, ub: Bound) extends Type:
    def toSort: Sort = Sort.Int

  final case class LangType(reExpr: ReExpr) extends Type:
    def toSort: Sort = Sort.String

  given fromSort: Conversion[Sort, Type] = {
    case Sort.Top => AnyType
    case Sort.Bot => NoType
    case Sort.Int => IntType
    case Sort.Bool => BoolType
    case Sort.String => StringType
    case Sort.Array(s) => ArrayType(s)
    case Sort.Fun(ss, s) => FunType(ss.map(fromSort), s)
  }

  sealed trait ReExpr

  case object ReEmpty extends ReExpr

  case object ReAllChar extends ReExpr

  final case class ReChar(value: Char) extends ReExpr

  final case class ReRange(from: Char, to: Char) extends ReExpr

  final case class ReConcat(lhs: ReExpr, rhs: ReExpr) extends ReExpr

  def mkReConcat(parts: Seq[ReExpr]): ReExpr =
    require(parts.nonEmpty)
    parts.reduce(ReConcat(_, _))

  final case class ReUnion(lhs: ReExpr, rhs: ReExpr) extends ReExpr

  def mkReUnion(choices: Seq[ReExpr]): ReExpr =
    require(choices.nonEmpty)
    choices.reduce(ReConcat(_, _))

  final case class ReRepeat(lb: Bound, ub: Bound, base: ReExpr) extends ReExpr

  final case class ReComp(operand: ReExpr) extends ReExpr

  sealed trait Stmt extends Node

  final case class Declare(localName: String, typ: Type) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitDeclare(this, ctx)

  final case class Assign(target: Option[String], value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  object Assign:
    def apply(localName: String, value: Expr): Assign = Assign(Some(localName), value)

  final case class Assert(cond: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Return(value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class IfStmt(cond: Expr, body: Stmt, elseBody: Stmt) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfStmt(this, ctx)

  final case class While(cond: Expr, body: Stmt) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  @deprecated
  final case class PureStmt(value: Expr) extends Stmt:
    override def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = ???

  final case class StmtBlock(body: Seq[Stmt]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitStmtBlock(this, ctx)

  sealed trait Expr extends Node, Locational

  final case class Literal(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLiteral(this, ctx)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

  final case class LocalRef(localName: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLocalRef(this, ctx)

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

  final case class ApplyOp(op: Op, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApplyOp(this, ctx)

  def apply(op: Op, args: Expr*): Expr = ApplyOp(op, args.toSeq)

  final case class IfExpr(cond: Expr, body: Expr, elseBody: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExpr(this, ctx)

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

    def visitDeclare(node: Declare, ctx: C): T = visitStmt(node, ctx)

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
