package flat.checker.flan

import org.eclipse.lsp4j.{Position, Range}

object untpd:
  final case class Module(imports: List[Import], body: List[TopDef])

  object Module:
    val empty: Module = Module(Nil, Nil)

  final case class Import(module: Ident, items: List[Ident])

  final case class Ident(name: String)(val range: Range)

  // Top-Level Definitions
  sealed trait TopDef:
    val ident: Ident

  final case class TypeDef(ident: Ident, value: Type) extends TopDef

  final case class ValDef(ident: Ident, typ: Option[Type], value: Expr) extends TopDef

  final case class LangDef(ident: Ident, value: PExpr) extends TopDef

  final case class FunDef(ident: Ident, params: List[Param], returnParams: List[Param],
                          requires: List[Expr], ensures: List[Expr], body: List[Stmt])
                         (val endRange: Range) extends TopDef

  final case class Param(ident: Ident, typ: Type)

  // Types
  sealed trait Type:
    val range: Range

  final case class TypeRef(name: String)(val range: Range) extends Type

  final case class GenericType(constr: Ident, args: List[Type])(val range: Range) extends Type

  final case class TupleType(elemTypes: List[Type])(val range: Range) extends Type:
    def arity: Int = elemTypes.length

  final case class FunType(paramTypes: List[Type], returnType: Type)(val range: Range) extends Type

  final case class OptType(baseType: Type)(val range: Range) extends Type

  // Expressions
  sealed trait Expr:
    val range: Range

  final case class IntConst(value: BigInt)(val range: Range) extends Expr

  final case class BoolConst(value: Boolean)(val range: Range) extends Expr

  final case class CharConst(value: Char)(val range: Range) extends Expr

  final case class StrConst(value: String)(val range: Range) extends Expr

  final case class NullConst()(val range: Range) extends Expr

  final case class TermRef(name: String)(val range: Range) extends Expr

  final case class ListExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class SetExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class MapExpr(items: List[(Expr, Expr)])(val range: Range) extends Expr

  final case class TupleExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class UnaryExpr(op: Ident, expr: Expr) extends Expr:
    override val range: Range = Range(op.range.getStart, expr.range.getEnd)

  final case class BinaryExpr(left: Expr, op: Ident, right: Expr) extends Expr:
    override val range: Range = Range(left.range.getStart, right.range.getEnd)

  final case class ChainedExpr(left: Expr, cmps: List[(Ident, Expr)]) extends Expr:
    override val range: Range = Range(left.range.getStart, cmps.last._2.range.getEnd)

  final case class MemberAccess(receiver: Expr, member: Ident) extends Expr:
    override val range: Range = Range(receiver.range.getStart, member.range.getEnd)

  final case class IndexAccess(receiver: Expr, index: Expr)(endPos: Position) extends Expr:
    override val range: Range = Range(receiver.range.getStart, endPos)

  final case class Slice(receiver: Expr, start: Option[Expr], end: Option[Expr])(endPos: Position) extends Expr:
    override val range: Range = Range(receiver.range.getStart, endPos)

  final case class Apply(fun: Expr, args: List[Expr])(endPos: Position) extends Expr:
    override val range: Range = Range(fun.range.getStart, endPos)

  final case class Size(receiver: Expr)(val range: Range) extends Expr

  final case class Ite(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr:
    override val range: Range = Range(cond.range.getStart, elseExpr.range.getEnd)

  // Formal Languages
  sealed trait PExpr

  final case class PChar(value: Char) extends PExpr

  final case class PStr(value: String) extends PExpr

  final case class PRef(name: String)(val range: Range) extends PExpr

  final case class PStar(lang: PExpr) extends PExpr

  final case class PPlus(lang: PExpr) extends PExpr

  final case class POpt(lang: PExpr) extends PExpr

  final case class PRep(lang: PExpr, times: BigInt | IntRange) extends PExpr

  final case class IntRange(from: BigInt, to: Option[BigInt])(val range: Range)

  final case class PConcat(left: PExpr, right: PExpr) extends PExpr

  final case class PUnion(left: PExpr, right: PExpr) extends PExpr

  final case class PCharSet(negated: Boolean, items: List[Char | CharRange]) extends PExpr

  final case class CharRange(from: Char, to: Char)(val range: Range)

  case object PAllChar extends PExpr

  // Statements
  trait Stmt

  final case class Pass() extends Stmt

  final case class ExprStmt(expr: Expr) extends Stmt

  final case class Assign(left: LExpr, right: Expr | Nondet.type) extends Stmt

  case object Nondet

  final case class AugAssign(left: Ident, op: Ident, right: Expr) extends Stmt

  final case class Assume(expr: Expr) extends Stmt

  final case class Assert(expr: Expr) extends Stmt

  final case class Abort(expr: Expr) extends Stmt

  final case class If(guard: Expr, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt

  final case class Return(value: Option[Expr])(val range: Range) extends Stmt

  final case class While(guard: Expr, invariants: List[Expr], body: List[Stmt]) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  final case class For(ident: Ident, iter: Expr, invariants: List[Expr], body: List[Stmt]) extends Stmt

  // Left-values
  sealed trait LExpr:
    val range: Range

  final case class LVarRef(name: String)(val range: Range) extends LExpr

  final case class LValDecl(ident: Ident, typ: Option[Type])(startPos: Position) extends LExpr:
    override val range: Range = Range(startPos, if typ.isEmpty then ident.range.getEnd else typ.get.range.getEnd)

  final case class LVarDecl(ident: Ident, typ: Option[Type])(startPos: Position) extends LExpr:
    override val range: Range = Range(startPos, if typ.isEmpty then ident.range.getEnd else typ.get.range.getEnd)

  final case class LTuple(elems: List[LExpr])(val range: Range) extends LExpr

  final case class LList(elems: List[LExpr])(val range: Range) extends LExpr
