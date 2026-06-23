package flat.flan

import org.eclipse.lsp4j.Range

trait Tree:
  type Type
  type Lang
  type Expr

  final case class Program(defs: List[TopDef])(val uri: String)

  sealed trait TopDef

  final case class TypeDef(ident: Ident, typ: Type) extends TopDef

  final case class LangDef(ident: Ident, lang: Lang) extends TopDef

  final case class ConstDef(ident: Ident, value: Expr) extends TopDef

  final case class MethodDef(ident: Ident, params: List[Param], returnType: Type,
                             specs: List[MethodSpec], body: Stmt) extends TopDef

  final case class Param(ident: Ident, typ: Type)

  sealed trait MethodSpec

  final case class RequireSpec(cond: Expr)(val range: Range) extends MethodSpec

  final case class EnsureSpec(cond: Expr)(val range: Range) extends MethodSpec

  trait Stmt:
    val range: Range

  final case class Assume(cond: Expr)(val range: Range) extends Stmt

  final case class Assert(cond: Expr)(val range: Range) extends Stmt

  final case class If(cond: Expr, thenBody: Stmt, elseBody: Stmt)(val range: Range) extends Stmt

  final case class While(cond: Expr, invariants: List[Expr], body: Stmt)(val range: Range) extends Stmt

  final case class For(ident: Ident, iterable: Expr, body: Stmt)(val range: Range) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  final case class StmtList(stmts: List[Stmt])(val range: Range) extends Stmt

  final case class Ident(value: String)(val range: Range)

  type Literal = Null | Boolean | BigInt | Char | String
