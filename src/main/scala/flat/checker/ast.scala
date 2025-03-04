package flat.checker

object ast:
  trait Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T

  final case class Program(body: Seq[FunDef])

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

  case object CharType extends Type:
    def toSort: Sort = Sort.Char

  case object StringType extends Type:
    def toSort: Sort = Sort.String

  final case class LiteralType(value: Int | Boolean | Char | String) extends Type:
    def toSort: Sort = value match
      case _: Int => Sort.Int
      case _: Boolean => Sort.Bool
      case _: Char => Sort.Char
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
    case Sort.Char => CharType
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

  final case class Literal(value: Int | Boolean | Char | String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLiteral(this, ctx)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

  final case class LocalRef(localName: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitLocalRef(this, ctx)

  final case class Apply(fun: Op | Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

  def apply(fun: Op | Expr, args: Expr*): Expr = Apply(fun, args.toSeq)

  final case class IfExpr(cond: Expr, body: Expr, elseBody: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExpr(this, ctx)

  val NoExpr: Expr = GlobalRef("")

  trait NodeVisitor[C, T]:
    def visitDefault(node: Node, ctx: C): T = throw UnsupportedOperationException()

    // def
    def visitFunDef(node: FunDef, ctx: C): T = visitDefault(node, ctx)

    // stmt
    def visitDeclare(node: Declare, ctx: C): T = visitDefault(node, ctx)

    def visitAssign(node: Assign, ctx: C): T = visitDefault(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitDefault(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitDefault(node, ctx)

    def visitIfStmt(node: IfStmt, ctx: C): T = visitDefault(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitDefault(node, ctx)

    def visitStmtBlock(node: StmtBlock, ctx: C): T = visitDefault(node, ctx)

    // expr
    def visitLiteral(node: Literal, ctx: C): T = visitDefault(node, ctx)

    def visitGlobalRef(node: GlobalRef, ctx: C): T = visitDefault(node, ctx)

    def visitLocalRef(node: LocalRef, ctx: C): T = visitDefault(node, ctx)

    def visitApply(node: Apply, ctx: C): T = visitDefault(node, ctx)

    def visitIfExpr(node: IfExpr, ctx: C): T = visitDefault(node, ctx)

  //    def visit(node: Assign, ctx: C): T = visitDefault(node, ctx)

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
    @deprecated
    case ITE
    // Char
    case IS_DIGIT
    case CHAR_TO_CODE
    case CHAR_FROM_CODE
    @deprecated case CHAR_TO_STR
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

    def accept[I, O](visitor: OpVisitor[I, O], input: I): O = this match
      case Op.EQ => visitor.visitEqual(input)
      case Op.NE => visitor.visitNotEqual(input)
      case Op.ADD => visitor.visitAdd(input)
      case Op.SUB => visitor.visitSub(input)
      case Op.LE => visitor.visitLessEqual(input)
      case Op.LT => visitor.visitLessThan(input)
      case Op.GE => visitor.visitGreaterEqual(input)
      case Op.GT => visitor.visitGreaterThan(input)
      case Op.AND => visitor.visitAnd(input)
      case Op.OR => visitor.visitOr(input)
      case Op.NOT => visitor.visitNot(input)
      case Op.ITE => ???
      case Op.IS_DIGIT => ???
      case Op.CHAR_TO_CODE => visitor.visitCharToCode(input)
      case Op.CHAR_FROM_CODE => visitor.visitCharFromCode(input)
      case Op.CHAR_TO_STR => ???
      case Op.CONCAT => visitor.visitStringConcat(input)
      case Op.REVERSE => visitor.visitStringReverse(input)
      case Op.STR_LEN => visitor.visitStringLength(input)
      case Op.STR_AT => visitor.visitStringAt(input)
      case Op.SUBSTR => visitor.visitSubstring(input)
      case Op.INDEX_OF => visitor.visitStringIndexOf(input)
      case Op.SPLIT => visitor.visitStringSplit(input)
      case Op.STARTS_WITH => visitor.visitStringStartsWith(input)
      case Op.ENDS_WITH => visitor.visitStringEndsWith(input)
      case Op.CONTAINS => visitor.visitStringContains(input)
      case Op.STR_TO_INT => visitor.visitStringToInt(input)
      case Op.STR_FROM_INT => visitor.visitStringFromInt(input)
      case Op.ARRAY_MK => visitor.visitNewArray(input)
      case Op.ARRAY_AT => visitor.visitArrayAt(input)
      case Op.ARRAY_UPDATE => visitor.visitArrayUpdate(input)
      case Op.ARRAY_CONTAINS => visitor.visitArrayContains(input)
      case Op.ARRAY_FORALL_TRUE => visitor.visitArrayForallTrue(input)
      case Op.ARRAY_EXISTS_TRUE => visitor.visitArrayExistsTrue(input)


  trait OpVisitor[I, O]:
    def visitEqual(input: I): O

    def visitNotEqual(input: I): O

    def visitAdd(input: I): O

    def visitSub(input: I): O

    def visitLessEqual(input: I): O

    def visitLessThan(input: I): O

    def visitGreaterEqual(input: I): O

    def visitGreaterThan(input: I): O

    def visitAnd(input: I): O

    def visitOr(input: I): O

    def visitNot(input: I): O

    def visitCharToCode(input: I): O

    def visitCharFromCode(input: I): O

    def visitStringConcat(input: I): O

    def visitStringReverse(input: I): O

    def visitStringLength(input: I): O

    def visitStringAt(input: I): O

    def visitSubstring(input: I): O

    def visitStringIndexOf(input: I): O

    def visitStringSplit(input: I): O

    def visitStringStartsWith(input: I): O

    def visitStringEndsWith(input: I): O

    def visitStringContains(input: I): O

    def visitStringToInt(input: I): O

    def visitStringFromInt(input: I): O

    def visitNewArray(input: I): O

    def visitArrayAt(input: I): O

    def visitArrayUpdate(input: I): O

    def visitArrayContains(input: I): O

    def visitArrayForallTrue(input: I): O

    def visitArrayExistsTrue(input: I): O
