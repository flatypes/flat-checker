package flat.checker.flan

import org.eclipse.lsp4j.{Position, Range}

object untpd:
  final case class Module(uri: String, body: List[TopStmt] = Nil)

  final case class Ident(name: String)(val range: Range)

  // Top-level statements
  sealed trait TopStmt:
    val ident: Ident

  final case class Import(module: Ident, items: List[Ident]) extends TopStmt:
    override val ident: Ident = module

  final case class FunDef(ident: Ident, params: List[Param], returnType: Option[Type],
                          requires: List[Expr], ensures: List[Expr], body: List[Stmt])
                         (val endRange: Range) extends TopStmt

  final case class Param(ident: Ident, typ: Option[Type])

  final case class ConstDef(ident: Ident, value: Expr) extends TopStmt

  final case class LangDef(ident: Ident, value: PExpr) extends TopStmt

  final case class TypeAlias(ident: Ident, value: Type) extends TopStmt

  // Local statements
  sealed trait Stmt

  final case class Pass() extends Stmt

  final case class ExprStmt(expr: Expr) extends Stmt

  final case class VarDecl(left: Pat, right: Expr) extends Stmt

  sealed trait Pat:
    val range: Range

  final case class PatVar(ident: Ident, typ: Option[Type]) extends Pat:
    override val range: Range = if typ.isEmpty then ident.range else Range(ident.range.getStart, typ.get.range.getEnd)

  final case class TuplePat(elems: List[Pat])(val range: Range) extends Pat

  final case class Assign(left: Ident, right: Expr) extends Stmt

  // Proof derivatives
  final case class Assume(expr: Expr) extends Stmt

  final case class Assert(expr: Expr) extends Stmt

  final case class Abort(expr: Expr) extends Stmt

  // Control flow
  final case class If(cond: Expr, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt

  final case class While(cond: Expr, specs: List[LoopSpec], body: List[Stmt]) extends Stmt:
    def invariants: List[Expr] = specs.collect { case InvariantSpec(e) => e }

  final case class For(ident: Ident, iter: Expr, specs: List[LoopSpec], body: List[Stmt]) extends Stmt:
    def invariants: List[Expr] = specs.collect { case InvariantSpec(e) => e }

  sealed trait LoopSpec

  final case class InvariantSpec(cond: Expr) extends LoopSpec

  final case class Return(value: Option[Expr])(val range: Range) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  // Expressions
  sealed trait Expr:
    val range: Range

  final case class IntConst(value: BigInt)(val range: Range) extends Expr

  final case class BoolConst(value: Boolean)(val range: Range) extends Expr

  final case class StrConst(value: String)(val range: Range) extends Expr

  final case class NullConst()(val range: Range) extends Expr

  final case class TermName(name: String)(val range: Range) extends Expr

  final case class TupleExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class LambdaExpr(params: List[Param], value: Expr)(val range: Range) extends Expr

  final case class Apply(fun: Expr, args: List[Expr])(endPos: Position) extends Expr:
    override val range: Range = Range(fun.range.getStart, endPos)

  // Operations
  final case class Access(receiver: Expr, member: Ident)(val range: Range) extends Expr

  final case class BinaryExpr(left: Expr, op: Ident, right: Expr) extends Expr:
    override val range: Range = Range(left.range.getStart, right.range.getEnd)

  final case class Ite(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr:
    override val range: Range = Range(cond.range.getStart, elseExpr.range.getEnd)

  final case class ListExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class SetExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class MapExpr(items: List[(Expr, Expr)])(val range: Range) extends Expr

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

  // Types
  sealed trait Type:
    val range: Range

  final case class TypeName(name: String)(val range: Range) extends Type

  final case class TypeApply(constr: Ident, args: List[Type])(val range: Range) extends Type

  final case class TupleType(elemTypes: List[Type])(val range: Range) extends Type:
    def arity: Int = elemTypes.length

  final case class FunType(paramTypes: List[Type], returnType: Type)(val range: Range) extends Type

  final case class OptType(baseType: Type)(val range: Range) extends Type

  final case class ReftType(param: Param, cond: Expr)(val range: Range) extends Type