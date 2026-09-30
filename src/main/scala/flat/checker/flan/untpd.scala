package flat.checker.flan

import org.eclipse.lsp4j.{Position, Range}

object untpd:
  // Module
  final case class Module(imports: List[Import], body: List[TopDef])

  object Module:
    def empty: Module = Module(Nil, Nil)

  final case class Import(module: Name, names: List[Name])

  final case class Name(value: String)(val range: Range)

  // Top-Level Definitions
  sealed trait TopDef:
    val name: Name

  final case class TypeDef(name: Name, body: Type) extends TopDef

  final case class ValDef(name: Name, typ: Option[Type], body: Option[Expr]) extends TopDef

  final case class LangDef(name: Name, body: PExpr) extends TopDef

  final case class FunDef(name: Name, params: List[Param], returns: List[Param],
                          requires: List[Expr], ensures: List[Expr], body: List[Stmt]) extends TopDef

  final case class Param(name: Name, typ: Type)

  // Types
  sealed trait Type:
    val range: Range

  final case class TypeRef(name: String)(val range: Range) extends Type

  final case class GenericType(ctor: Name, args: List[Type])(end: Position) extends Type:
    override val range: Range = Range(ctor.range.getStart, end)

  final case class TupleType(elems: List[Type])(val range: Range) extends Type

  final case class FunType(params: List[Type], returns: Type)(start: Position) extends Type:
    override val range: Range = Range(start, returns.range.getEnd)

  final case class OptType(typ: Type)(end: Position) extends Type:
    override val range: Range = Range(typ.range.getStart, end)

  // Expressions
  trait Expr:
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

  final case class UnaryExpr(op: Name, operand: Expr) extends Expr:
    override val range: Range = Range(op.range.getStart, operand.range.getEnd)

  final case class BinaryExpr(left: Expr, op: Name, right: Expr) extends Expr:
    override val range: Range = Range(left.range.getStart, right.range.getEnd)

  final case class ChainedExpr(left: Expr, comparators: List[(Name, Expr)]) extends Expr:
    require(comparators.nonEmpty, "ChainingExpr must have at least one comparator")
    override val range: Range = Range(left.range.getStart, comparators.last._2.range.getEnd)

  final case class FieldAccess(receiver: Expr, field: Name) extends Expr:
    override val range: Range = Range(receiver.range.getStart, field.range.getEnd)

  final case class IndexAccess(receiver: Expr, index: Expr)(end: Position) extends Expr:
    override val range: Range = Range(receiver.range.getStart, end)

  final case class Apply(fun: Expr, args: List[Expr])(end: Position) extends Expr:
    override val range: Range = Range(fun.range.getStart, end)

  final case class Slice(receiver: Expr, from: Option[Expr], until: Option[Expr])(end: Position) extends Expr:
    override val range: Range = Range(receiver.range.getStart, end)

  final case class Size(receiver: Expr)(val range: Range) extends Expr

  final case class Ite(cond: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
    override val range: Range = Range(cond.range.getStart, elseValue.range.getEnd)

  // Parsing expressions
  sealed trait PExpr

  final case class PChar(value: Char) extends PExpr

  final case class PStr(value: String) extends PExpr

  case object PAllChar extends PExpr

  final case class PCharSet(positive: Boolean, items: List[Char | CharRange]) extends PExpr

  final case class CharRange(from: Char, to: Char)(val range: Range)

  final case class PRef(name: String)(val range: Range) extends PExpr

  final case class PConcat(left: PExpr, right: PExpr) extends PExpr

  final case class PUnion(left: PExpr, right: PExpr) extends PExpr

  final case class PStar(base: PExpr) extends PExpr

  final case class PPlus(base: PExpr) extends PExpr

  final case class POpt(base: PExpr) extends PExpr

  final case class PRep(base: PExpr, times: BigInt | IntRange) extends PExpr

  final case class IntRange(from: BigInt, to: Option[BigInt])(val range: Range)

  // Local statements
  trait Stmt

  final case class Pass() extends Stmt

  final case class ExprStmt(expr: Expr) extends Stmt

  final case class Assign(left: LExpr, right: Expr | Nondet.type) extends Stmt

  case object Nondet

  final case class AugAssign(left: Name, op: Name, right: Expr) extends Stmt

  final case class Assume(cond: Expr) extends Stmt

  final case class Assert(cond: Expr) extends Stmt

  final case class Abort(expr: Expr) extends Stmt

  final case class If(cond: Expr, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt

  final case class Return(value: Option[Expr])(val returnRange: Range) extends Stmt

  final case class While(cond: Expr, invariants: List[Expr], body: List[Stmt]) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  final case class For(name: Name, iter: Expr, invariants: List[Expr], body: List[Stmt]) extends Stmt

  // Left-hand side expressions
  sealed trait LExpr:
    val range: Range

  final case class LVarRef(name: String)(val range: Range) extends LExpr

  final case class LValDecl(name: Name, typ: Option[Type])(start: Position) extends LExpr:
    override val range: Range = Range(start, if typ.isDefined then typ.get.range.getEnd else name.range.getEnd)

  final case class LVarDecl(name: Name, typ: Option[Type])(start: Position) extends LExpr:
    override val range: Range = Range(start, if typ.isDefined then typ.get.range.getEnd else name.range.getEnd)

  final case class LList(elems: List[LExpr])(val range: Range) extends LExpr

  final case class LTuple(elems: List[LExpr])(val range: Range) extends LExpr
