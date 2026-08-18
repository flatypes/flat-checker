package flat.checker.flan

import org.eclipse.lsp4j.Range

object untpd:
  final case class Program(imports: List[Import], body: List[TopDef])

  final case class Import(module: Ident, items: List[Ident])

  // Top-Level Definitions
  sealed trait TopDef:
    val ident: Ident

  final case class TypeDef(ident: Ident, value: Type) extends TopDef

  final case class LangDef(ident: Ident, value: Lang) extends TopDef

  final case class ConstDef(ident: Ident, value: Expr) extends TopDef

  final case class MethodDef(ident: Ident, params: List[Param], returnParams: List[Param],
                             requires: List[Expr], ensures: List[Expr], body: Option[List[Stmt]])
                            (val endRange: Range) extends TopDef

  final case class Ident(name: String)(val range: Range)

  final case class Param(ident: Ident, typ: Type)

  // Statements
  trait Stmt

  final case class Nondet()(val range: Range)

  @deprecated
  final case class VarStmt(ident: Ident, typ: Type, value: Expr | Nondet) extends Stmt

  final case class VarDecl(ident: Ident, typ: Type) extends Stmt

  sealed trait Target:
    val range: Range

  final case class TargetName(name: String)(val range: Range) extends Target

  final case class ValTarget(ident: Ident, typ: Option[Type])(val range: Range) extends Target

  final case class VarTarget(ident: Ident, typ: Option[Type])(val range: Range) extends Target

  final case class TupleTarget(elems: List[Target])(val range: Range) extends Target

  final case class ListTarget(elems: List[Target])(val range: Range) extends Target

  final case class Assign(target: Target, value: Expr) extends Stmt

  final case class ExprStmt(expr: Expr) extends Stmt

  final case class Return(value: Option[Expr])(val range: Range) extends Stmt

  final case class If(guard: Expr | Nondet, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt

  final case class While(guard: Expr | Nondet, invariants: List[Expr], body: List[Stmt]) extends Stmt

  final case class For(ident: Ident, iter: Expr, invariants: List[Expr], body: List[Stmt]) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  final case class Abort(expr: Expr) extends Stmt

  final case class Assume(expr: Expr) extends Stmt

  final case class Assert(expr: Expr) extends Stmt

  // Types
  trait Type

  final case class TypeName(name: String)(val range: Range) extends Type

  final case class GenericType(constr: String, args: List[Type])(val range: Range) extends Type

  final case class TupleType(elemTypes: List[Type]) extends Type:
    def arity: Int = elemTypes.length

  val unitType = TupleType(Nil)

  def mkTupleType(elemTypes: Type*): TupleType = TupleType(elemTypes.toList)

  final case class FunType(paramTypes: List[Type], returnType: Type) extends Type

  final case class NullableType(baseType: Type)(val range: Range) extends Type

  @deprecated
  final case class UnionType(left: Type, right: Type) extends Type

  // Formal Languages
  sealed trait Lang

  final case class LangConst(value: String)(val range: Range) extends Lang

  final case class LangName(name: String)(val range: Range) extends Lang

  final case class RegEx(regEx: flat.checker.domain.StrRE) extends Lang

  final case class LangStar(lang: Lang)(val range: Range) extends Lang

  final case class LangPlus(lang: Lang)(val range: Range) extends Lang

  final case class LangOpt(lang: Lang)(val range: Range) extends Lang

  final case class LangPower(lang: Lang, exp: Int)(val range: Range) extends Lang

  final case class LangLoop(lang: Lang, min: Int, max: Option[Int])(val range: Range) extends Lang

  final case class LangConcat(left: Lang, right: Lang)(val range: Range) extends Lang

  final case class LangUnion(left: Lang, right: Lang)(val range: Range) extends Lang

  // Expressions
  trait Expr:
    val range: Range

  final case class Const(lit: Literal)(val range: Range) extends Expr

  final case class TermName(name: String)(val range: Range) extends Expr

  // Equality
  final case class Eq(left: Expr, right: Expr)(val range: Range) extends Expr

  final case class Ne(left: Expr, right: Expr)(val range: Range) extends Expr

  // Boolean Operators
  final case class And(left: Expr, right: Expr)(val range: Range) extends Expr

  final case class Or(left: Expr, right: Expr)(val range: Range) extends Expr

  final case class Not(cond: Expr)(val range: Range) extends Expr

  final case class Implies(left: Expr, right: Expr)(val range: Range) extends Expr

  final case class Ite(cond: Expr, thenExpr: Expr, elseExpr: Expr)(val range: Range) extends Expr

  // General Operators
  // NOTE: for future
  final case class UnaryExpr(op: Ident, expr: Expr)(val range: Range) extends Expr

  // NOTE: for future
  final case class BinaryExpr(op: Ident, left: Expr, right: Expr)(val range: Range) extends Expr

  // NOTE: for future
  final case class Size(coll: Expr)(val range: Range) extends Expr

  // NOTE: for future
  final case class At(coll: Expr, index: Expr)(val range: Range) extends Expr

  // NOTE: for future
  final case class Slice(coll: Expr, start: Option[Expr], end: Option[Expr])(val range: Range) extends Expr

  final case class Access(receiver: Expr, member: Ident)(val range: Range) extends Expr

  def mkUnary(operand: Expr, op: Ident)(range: Range): Apply =
    val access = Access(operand, op)(range)
    Apply(access, Nil)(range)

  def mkNot(operand: Expr)(range: Range, notRange: Range): Apply =
    mkUnary(operand, Ident("prefix_!")(notRange))(range)

  def mkApply(receiver: Expr, op: Ident, args: Expr*)(range: Range): Apply =
    val access = Access(receiver, op)(Range(receiver.range.getStart, op.range.getEnd))
    Apply(access, args.toList)(range)

  final case class Apply(fun: Expr, args: List[Expr])(val range: Range) extends Expr

  // Constructors
  final case class SeqExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class SetExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class MapExpr(keys: List[Expr], values: List[Expr])(val range: Range) extends Expr

  final case class TupleExpr(elems: List[Expr])(val range: Range) extends Expr

  final case class InLang(str: Expr, lang: Lang)(val range: Range) extends Expr


