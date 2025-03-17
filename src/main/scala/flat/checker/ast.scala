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

  case object UnitType extends Type:
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

  val charType: Type = LangType(ReLang.allChar)

  def literalType(value: Int | Boolean | String): Type = value match
    case i: Int => IntervalType(i)
    case b: Boolean => TernaryType(b)
    case s: String => LangType(s)

  final case class TupleType(elems: Seq[Type]) extends Type:
    def toSort: Sort = Sort.Tuple(elems.map(_.toSort))

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
    case Sort.Tuple(ss) => TupleType(ss.map(fromSort))
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

  final case class Literal(value: Int | Boolean | String | Unit) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLiteral(this, ctx)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

  final case class LocalRef(id: Int) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLocalRef(this, ctx)

  final case class TupleExpr(elems: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitTupleExpr(this, ctx)

  final case class IfExpr(cond: Expr, body: Expr, elseBody: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExpr(this, ctx)

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

  enum Op:
    // Boolean
    case AND
    case OR
    case NOT
    // comparison0
    case EQ
    case LT
    // Integer
    case ADD
    case SUB
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

  final case class ApplyOp(op: Op, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = op match
      case Op.AND => visitor.visitAnd(this, ctx)
      case Op.OR => visitor.visitOr(this, ctx)
      case Op.NOT => visitor.visitNot(this, ctx)
      case Op.EQ => visitor.visitEqual(this, ctx)
      case Op.LT => visitor.visitLessThan(this, ctx)
      case Op.ADD => visitor.visitAdd(this, ctx)
      case Op.SUB => visitor.visitSub(this, ctx)
      case Op.CHAR_TO_CODE => visitor.visitCharToCode(this, ctx)
      case Op.CHAR_FROM_CODE => visitor.visitCharFromCode(this, ctx)
      case Op.CONCAT => visitor.visitStringConcat(this, ctx)
      case Op.REVERSE => visitor.visitStringReverse(this, ctx)
      case Op.STR_LEN => visitor.visitStringLength(this, ctx)
      case Op.STR_AT => visitor.visitStringAt(this, ctx)
      case Op.SUBSTR => visitor.visitSubstring(this, ctx)
      case Op.INDEX_OF => visitor.visitStringIndexOf(this, ctx)
      case Op.SPLIT => visitor.visitStringSplit(this, ctx)
      case Op.STARTS_WITH => visitor.visitStringStartsWith(this, ctx)
      case Op.ENDS_WITH => visitor.visitStringEndsWith(this, ctx)
      case Op.CONTAINS => visitor.visitStringContains(this, ctx)
      case Op.STR_TO_INT => visitor.visitStringToInt(this, ctx)
      case Op.STR_FROM_INT => visitor.visitStringFromInt(this, ctx)
      case Op.ARRAY_MK => visitor.visitNewArray(this, ctx)
      case Op.ARRAY_AT => visitor.visitArrayAt(this, ctx)
      case Op.ARRAY_UPDATE => visitor.visitArrayUpdate(this, ctx)
      case Op.ARRAY_CONTAINS => visitor.visitArrayContains(this, ctx)
      case Op.ARRAY_FORALL_TRUE => visitor.visitArrayForallTrue(this, ctx)
      case Op.ARRAY_EXISTS_TRUE => visitor.visitArrayExistsTrue(this, ctx)

  def apply(op: Op, args: Expr*): Expr = ApplyOp(op, args.toSeq)

  val NoExpr: Expr = GlobalRef("")

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

    def visitTupleExpr(node: TupleExpr, ctx: C): T = visitExpr(node, ctx)

    def visitApply(node: Apply, ctx: C): T = visitExpr(node, ctx)

    def visitApplyOp(node: ApplyOp, ctx: C): T =
      throw UnsupportedOperationException(s"visit apply ${node.op}")

    def visitIfExpr(node: IfExpr, ctx: C): T = visitExpr(node, ctx)

    // apply op
    def visitAnd(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitOr(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitNot(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitEqual(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitLessThan(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitAdd(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitSub(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitCharToCode(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitCharFromCode(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringConcat(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringReverse(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringLength(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringAt(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitSubstring(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringIndexOf(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringSplit(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringStartsWith(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringEndsWith(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringContains(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringToInt(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitStringFromInt(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitNewArray(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitArrayAt(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitArrayUpdate(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitArrayContains(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitArrayForallTrue(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)

    def visitArrayExistsTrue(node: ApplyOp, ctx: C): T = visitApplyOp(node, ctx)
