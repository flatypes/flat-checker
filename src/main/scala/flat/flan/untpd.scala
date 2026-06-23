package flat.flan

import org.eclipse.lsp4j.Range

object untpd extends Tree:
  sealed trait Type

  final case class TypeName(name: String)(val range: Range) extends Type

  final case class TupleType(elemTypes: List[Type])(val range: Range) extends Type

  final case class TypeApply(typeFun: Type, typeArgs: List[Type])(val range: Range) extends Type

  final case class OptType(typ: Type)(val range: Range) extends Type

  final case class UnionType(left: Type, right: Type)(val range: Range) extends Type

  final case class FunType(paramTypes: List[Type], returnType: Type)(val range: Range) extends Type

  final case class NoType()(val range: Range) extends Type

  type Lang = Clause

  sealed trait Clause:
    val range: Range

  sealed trait CharSet extends Clause

  final case class ConcreteCharSet(values: Set[Char])(val range: Range) extends CharSet

  final case class AllChar()(val range: Range) extends CharSet

  final case class CharRange(min: Char, max: Char)(val range: Range) extends CharSet

  final case class CharClass(isPos: Boolean, items: List[CharSet])(val range: Range) extends CharSet

  final case class LangName(name: String)(val range: Range) extends Clause

  final case class Repeat(part: Clause, quant: Quant)(val range: Range) extends Clause

  final case class Concat(left: Clause, right: Clause)(val range: Range) extends Clause

  final case class Union(left: Clause, right: Clause)(val range: Range) extends Clause

  sealed trait Quant:
    val range: Range

  final case class Star()(val range: Range) extends Quant

  final case class Plus()(val range: Range) extends Quant

  final case class Opt()(val range: Range) extends Quant

  final case class Exactly(count: Int)(val range: Range) extends Quant:
    assert(count >= 0)

  final case class Between(min: Int, max: Option[Int])(val range: Range) extends Quant:
    assert(min >= 0)
    assert(max.isEmpty || max.get >= min)

  sealed trait Expr:
    val range: Range

  final case class Const(literal: Literal)(val range: Range) extends Expr

  final case class TermName(name: String)(val range: Range) extends Expr

  final case class TupleExpr(elems: List[Expr])(val range: Range) extends Expr:
    assert(elems.isEmpty || elems.length >= 2)

  final case class AnnotExpr(expr: Expr, typ: Type)(val range: Range) extends Expr

  final case class MemberAccess(receiver: Expr, member: Ident)(val range: Range) extends Expr

  final case class Apply(fun: Expr, args: List[Expr])(val range: Range) extends Expr

  final case class RelExpr(ops: List[Ident], exprs: List[Expr])(val range: Range) extends Expr:
    assert(ops.length + 1 == exprs.length)

  final case class CondExpr(cond: Expr, thenValue: Expr, elseValue: Expr)(val range: Range) extends Expr

  final case class Lambda(params: List[Param], returnValue: Expr)(val range: Range) extends Expr

  // Statements
  final case class Declare(ident: Ident, typ: Option[Type], value: Option[Expr | UnknownValue])
                          (val range: Range) extends Stmt

  final case class Assign(target: Ident, value: Expr | UnknownValue)(val range: Range) extends Stmt

  final case class UnknownValue()(val range: Range)

  final case class ExprStmt(expr: Expr)(val range: Range) extends Stmt

  final case class Return(value: Option[Expr])(val range: Range) extends Stmt
