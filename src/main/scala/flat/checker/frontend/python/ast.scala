package flat.checker.frontend.python

import flat.checker.Locational

object ast:
  final case class Module(body: Seq[Stmt])

  sealed trait Node extends Locational:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T

  sealed trait Stmt extends Node

  // global
  final case class ImportFrom(module: String, names: Seq[Alias]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitImportFrom(this, ctx)

  final case class FunctionDef(name: String, args: Seq[Arg], body: Seq[Stmt], returns: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitFunctionDef(this, ctx)

  final case class TypeAlias(name: Expr, value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitTypeAlias(this, ctx)

  final case class Alias(name: String, asName: Option[String]) extends Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAlias(this, ctx)

  final case class Arg(arg: String, annotation: Expr) extends Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitArg(this, ctx)

  // local
  final case class Assign(target: Expr, value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  final case class AnnAssign(target: Expr, annotation: Expr, value: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAnnAssign(this, ctx)

  final case class Assert(test: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Pass() extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitPass(this, ctx)

  final case class If(test: Expr, body: Seq[Stmt], orElse: Seq[Stmt]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIf(this, ctx)

  final case class While(test: Expr, body: Seq[Stmt]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  final case class Return(value: Option[Expr]) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class ExprStmt(expr: Expr) extends Stmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitExprStmt(this, ctx)

  sealed trait Expr extends Node

  final case class Constant(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitConstant(this, ctx)

  final case class JoinedStr(values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitJoinedStr(this, ctx)

  final case class ListExpr(values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitListExpr(this, ctx)

  final case class TupleExpr(values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitTupleExpr(this, ctx)

  final case class Name(id: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitName(this, ctx)

  final case class UnaryOp(op: OpUnary, operand: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitUnaryOp(this, ctx)

  final case class BinOp(left: Expr, op: OpBin, right: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitBinOp(this, ctx)

  final case class BoolOp(op: OpBool, values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitBoolOp(this, ctx)

  final case class Compare(left: Expr, ops: Seq[OpCompare], comparators: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitCompare(this, ctx)

  final case class Call(func: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitCall(this, ctx)

  final case class IfExp(test: Expr, body: Expr, orElse: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExp(this, ctx)

  final case class Attribute(value: Expr, attr: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAttribute(this, ctx)

  final case class Subscript(value: Expr, slice: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitSubscript(this, ctx)

  // can appear only in Subscript
  final case class Slice(lower: Option[Expr], upper: Option[Expr], step: Option[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitSlice(this, ctx)

  trait Op:
    def name: String

    def repr: String

    override def toString: String = repr

  enum OpUnary extends Op:
    case UAdd
    case USub
    case Not

    def name: String =
      this match
        case UAdd => "__pos__"
        case USub => "__neg__"
        case Not => "__not__"

    def repr: String =
      this match
        case UAdd => "unary +"
        case USub => "unary -"
        case Not => "not"

  enum OpBin extends Op:
    case Add
    case Sub

    def name: String =
      this match
        case Add => "__add__"
        case Sub => "__sub__"

    def repr: String =
      this match
        case Add => "+"
        case Sub => "-"

  enum OpBool extends Op:
    case And
    case Or

    def name: String =
      this match
        case And => "__and__"
        case Or => "__or__"

    def repr: String =
      this match
        case And => "and"
        case Or => "or"

  enum OpCompare extends Op:
    case Eq
    case NotEq
    case Lt
    case LtE
    case Gt
    case GtE
    case In
    case NotIn

    def name: String =
      this match
        case Eq => "__eq__"
        case NotEq => "__ne__"
        case Lt => "__lt__"
        case LtE => "__le__"
        case Gt => "__gt__"
        case GtE => "__ge__"
        case In | NotIn => "__contains__"

    def repr: String =
      this match
        case Eq => "=="
        case NotEq => "!="
        case Lt => "<"
        case LtE => "<="
        case Gt => ">"
        case GtE => ">="
        case In => "in"
        case NotIn => "not in"

  val builtInFuncs: Set[String] =
    Set("all", "any", "bool", "chr", "int", "len", "max", "min", "ord", "reversed", "str")

  trait NodeVisitor[C, T]:
    def visitDefault(node: Node, ctx: C): T = throw UnsupportedOperationException()

    // stmt
    def visitImportFrom(node: ImportFrom, ctx: C): T = visitDefault(node, ctx)

    def visitFunctionDef(node: FunctionDef, ctx: C): T = visitDefault(node, ctx)

    def visitTypeAlias(node: TypeAlias, ctx: C): T = visitDefault(node, ctx)

    def visitAlias(node: Alias, ctx: C): T = visitDefault(node, ctx)

    def visitArg(node: Arg, ctx: C): T = visitDefault(node, ctx)

    def visitAssign(node: Assign, ctx: C): T = visitDefault(node, ctx)

    def visitAnnAssign(node: AnnAssign, ctx: C): T = visitDefault(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitDefault(node, ctx)

    def visitPass(node: Pass, ctx: C): T = visitDefault(node, ctx)

    def visitIf(node: If, ctx: C): T = visitDefault(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitDefault(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitDefault(node, ctx)

    def visitExprStmt(node: ExprStmt, ctx: C): T = visitDefault(node, ctx)

    // expr
    def visitConstant(node: Constant, ctx: C): T = visitDefault(node, ctx)

    def visitJoinedStr(node: JoinedStr, ctx: C): T = visitDefault(node, ctx)

    def visitListExpr(node: ListExpr, ctx: C): T = visitDefault(node, ctx)

    def visitTupleExpr(node: TupleExpr, ctx: C): T = visitDefault(node, ctx)

    def visitName(node: Name, ctx: C): T = visitDefault(node, ctx)

    def visitUnaryOp(node: UnaryOp, ctx: C): T = visitDefault(node, ctx)

    def visitBinOp(node: BinOp, ctx: C): T = visitDefault(node, ctx)

    def visitBoolOp(node: BoolOp, ctx: C): T = visitDefault(node, ctx)

    def visitCompare(node: Compare, ctx: C): T = visitDefault(node, ctx)

    def visitCall(node: Call, ctx: C): T = visitDefault(node, ctx)

    def visitIfExp(node: IfExp, ctx: C): T = visitDefault(node, ctx)

    def visitAttribute(node: Attribute, ctx: C): T = visitDefault(node, ctx)

    def visitSubscript(node: Subscript, ctx: C): T = visitDefault(node, ctx)

    def visitSlice(node: Slice, ctx: C): T = visitDefault(node, ctx)
