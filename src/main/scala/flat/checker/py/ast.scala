package flat.checker.py

import flat.checker.Locational
import flat.checker.backend.core.Ident

object ast:
  sealed trait Node extends Locational:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T

  sealed trait TopStmt extends Node

  final case class TypeAlias(ident: Ident, value: Expr) extends TopStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitTypeAlias(this, ctx)

  final case class FunctionDef(ident: Ident, args: Seq[Arg], body: Seq[LocalStmt],
                               returns: Option[Expr]) extends TopStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitFunctionDef(this, ctx)

  final case class Arg(ident: Ident, annotation: Expr) extends Node:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitArg(this, ctx)

  sealed trait LocalStmt extends Node

  final case class Assign(target: Expr, value: Expr) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  final case class AnnAssign(ident: Ident, annotation: Expr, init: Option[Expr]) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAnnAssign(this, ctx)

  final case class Assert(test: Expr) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Pass() extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitPass(this, ctx)

  final case class If(test: Expr, body: Seq[LocalStmt], orElse: Seq[LocalStmt]) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIf(this, ctx)

  final case class While(test: Expr, body: Seq[LocalStmt]) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  final case class Break() extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitBreak(this, ctx)

  final case class Return(value: Option[Expr]) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class ExprStmt(expr: Expr) extends LocalStmt:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitExprStmt(this, ctx)

  sealed trait Expr extends Node

  final case class Constant(value: Int | Boolean | String | Null) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitConstant(this, ctx)

  final case class ListExpr(values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitListExpr(this, ctx)

  final case class TupleExpr(values: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitTupleExpr(this, ctx)

  final case class Name(id: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitName(this, ctx)

    def asIdent: Ident = Ident(id).copyLocation(this)

  final case class Call(func: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitCall(this, ctx)

  final case class IfExp(test: Expr, body: Expr, orElse: Expr) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitIfExp(this, ctx)

  final case class Attribute(value: Expr, attr: String) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitAttribute(this, ctx)

  final case class Subscript(value: Expr, index: Expr | Slice) extends Expr:
    def accept[C, T](visitor: NodeVisitor[C, T], ctx: C): T = visitor.visitSubscript(this, ctx)

  final case class Slice(lower: Option[Expr], upper: Option[Expr]) extends Locational

  trait NodeVisitor[C, T]:
    def visitDefault(node: Node, ctx: C): T =
      throw UnsupportedOperationException("visit " + node.getClass.getName)

    // top-level stmt
    def visitFunctionDef(node: FunctionDef, ctx: C): T = visitDefault(node, ctx)

    def visitTypeAlias(node: TypeAlias, ctx: C): T = visitDefault(node, ctx)

    def visitArg(node: Arg, ctx: C): T = visitDefault(node, ctx)

    // stmt
    def visitAssign(node: Assign, ctx: C): T = visitDefault(node, ctx)

    def visitAnnAssign(node: AnnAssign, ctx: C): T = visitDefault(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitDefault(node, ctx)

    def visitPass(node: Pass, ctx: C): T = visitDefault(node, ctx)

    def visitIf(node: If, ctx: C): T = visitDefault(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitDefault(node, ctx)

    def visitBreak(node: Break, ctx: C): T = visitDefault(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitDefault(node, ctx)

    def visitExprStmt(node: ExprStmt, ctx: C): T = visitDefault(node, ctx)

    // expr
    def visitConstant(node: Constant, ctx: C): T = visitDefault(node, ctx)

    def visitListExpr(node: ListExpr, ctx: C): T = visitDefault(node, ctx)

    def visitTupleExpr(node: TupleExpr, ctx: C): T = visitDefault(node, ctx)

    def visitName(node: Name, ctx: C): T = visitDefault(node, ctx)

    def visitAttribute(node: Attribute, ctx: C): T = visitDefault(node, ctx)

    def visitSubscript(node: Subscript, ctx: C): T = visitDefault(node, ctx)

    def visitCall(node: Call, ctx: C): T = visitDefault(node, ctx)

    def visitIfExp(node: IfExp, ctx: C): T = visitDefault(node, ctx)
