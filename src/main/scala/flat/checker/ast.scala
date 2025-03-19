package flat.checker

object ast:
  trait Node

  final case class FunDef(ident: Ident, paramTypes: Seq[ast.Type], returnType: ast.Type,
                          varTypes: Seq[ast.Type], body: Stmt) extends Node

  final case class Ident(name: String) extends Node, Locational

  sealed trait Type extends Node:
    def ignoreHint: Type = this

    def toSort: Sort

  case object AnyType extends Type:
    def toSort: Sort = Sort.Top

  case object NoType extends Type:
    def toSort: Sort = Sort.Bot

  final case class IntervalType(interval: Interval) extends Type:
    def toSort: Sort = Sort.Int

  val intType: Type = IntervalType(Interval.full)

  final case class TernaryType(value: Ternary) extends Type:
    def toSort: Sort = Sort.Bool

  val boolType: Type = TernaryType(Ternary.Maybe)

  final case class LangType(reLang: ReLang) extends Type:
    def toSort: Sort = Sort.String

  val strType: Type = LangType(ReLang.full)

  val charType: Type = LangType(ReLang.allChar)

  def literalType(value: Int | Boolean | String): Type = value match
    case i: Int => IntervalType(i)
    case b: Boolean => TernaryType(b)
    case s: String => LangType(s)

  final case class TupleType(elems: Seq[Type]) extends Type:
    def toSort: Sort = Sort.Tuple(elems.map(_.toSort))

  val unitType: TupleType = TupleType(Seq.empty)

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
    case Sort.String => strType
    case Sort.Tuple(ss) => TupleType(ss.map(fromSort))
    case Sort.Array(s) => ArrayType(s)
    case Sort.Fun(ss, s) => FunType(ss.map(fromSort), s)
  }

  sealed trait Stmt extends Node:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T

  final case class Assign(id: Int, value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  final case class Assert(cond: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Return(value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class IfStmt(cond: Expr, body: Stmt, elseBody: Stmt) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitIfStmt(this, ctx)

  final case class While(cond: Expr, body: Stmt, invariants: Seq[Invariant] = Seq.empty) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  final case class Invariant(id: Int, typ: Type) extends Locational

  final case class StmtBlock(body: Seq[Stmt]) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitStmtBlock(this, ctx)

  trait StmtVisitor[C, T]:
    def visitStmt(node: Stmt, ctx: C): T =
      throw UnsupportedOperationException("visit " + node.getClass.getCanonicalName)

    def visitAssign(node: Assign, ctx: C): T = visitStmt(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitStmt(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitStmt(node, ctx)

    def visitIfStmt(node: IfStmt, ctx: C): T = visitStmt(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitStmt(node, ctx)

    def visitStmtBlock(node: StmtBlock, ctx: C): T = visitStmt(node, ctx)

  sealed trait Expr extends Node, Locational:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T

  final case class Const(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitConst(this, ctx)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

  final case class Var(id: Int) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitVar(this, ctx)

  final case class TupleExpr(elems: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitTupleExpr(this, ctx)

  def mkUnit: Expr = TupleExpr(Seq.empty)

  // builtin functions/operations
  final case class And(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitAnd(this, ctx)

  def mkAnd(conjuncts: Expr*): Expr = conjuncts.toList match
    case Nil => Const(true)
    case es => es.reduceRight(And.apply)

  final case class Or(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitOr(this, ctx)

  def mkOr(disjuncts: Expr*): Expr = disjuncts.toList match
    case Nil => Const(false)
    case es => es.reduceRight(Or.apply)

  final case class Not(operand: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitNot(this, ctx)

  final case class Ite(test: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitIte(this, ctx)

  final case class Cmp(op: CmpOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCmp(this, ctx)

  enum CmpOp:
    case EQ
    case NE
    case LE
    case LT
    case GE
    case GT

    def apply(left: Expr, right: Expr): Cmp = Cmp(this, left, right)

  final case class Arith(op: ArithOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitArith(this, ctx)

  enum ArithOp:
    case ADD
    case SUB

    def apply(left: Expr, right: Expr): Arith = Arith(this, left, right)

  final case class StrConcat(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrConcat(this, ctx)

  final case class StrLen(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrLen(this, ctx)

  final case class StrAt(str: Expr, index: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrAt(this, ctx)

  final case class StrSlice(str: Expr, fromIndex: Expr, untilIndex: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrSlice(this, ctx)

  final case class StrStartsWith(str: Expr, prefix: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrStartsWith(this, ctx)

  final case class StrEndsWith(str: Expr, suffix: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrEndsWith(this, ctx)

  final case class StrContains(str: Expr, infix: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrContains(this, ctx)

  final case class StrFind(str: Expr, target: Expr, fromIndex: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrFind(this, ctx)

  final case class StrSplit(str: Expr, sep: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrSplit(this, ctx)

  final case class StrRev(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrRev(this, ctx)

  final case class CharToCode(char: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCharToCode(this, ctx)

  final case class CharFromCode(code: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCharFromCode(this, ctx)

  final case class StrToInt(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrToInt(this, ctx)

  final case class StrFromInt(int: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrFromInt(this, ctx)

  final case class ArraySelect(array: Expr, index: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitArraySelect(this, ctx)

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

  val NoExpr: Expr = GlobalRef("")

  trait ExprVisitor[C, T]:
    def visitExpr(node: Expr, ctx: C): T =
      throw UnsupportedOperationException("visit " + node.getClass.getCanonicalName)

    def visitConst(node: Const, ctx: C): T = visitExpr(node, ctx)

    def visitGlobalRef(node: GlobalRef, ctx: C): T = visitExpr(node, ctx)

    def visitVar(node: Var, ctx: C): T = visitExpr(node, ctx)

    def visitTupleExpr(node: TupleExpr, ctx: C): T = visitExpr(node, ctx)

    def visitAnd(node: And, ctx: C): T = visitExpr(node, ctx)

    def visitOr(node: Or, ctx: C): T = visitExpr(node, ctx)

    def visitNot(node: Not, ctx: C): T = visitExpr(node, ctx)

    def visitIte(node: Ite, ctx: C): T = visitExpr(node, ctx)

    def visitCmp(node: Cmp, ctx: C): T = visitExpr(node, ctx)

    def visitArith(node: Arith, ctx: C): T = visitExpr(node, ctx)

    def visitStrConcat(node: StrConcat, ctx: C): T = visitExpr(node, ctx)

    def visitStrLen(node: StrLen, ctx: C): T = visitExpr(node, ctx)

    def visitStrAt(node: StrAt, ctx: C): T = visitExpr(node, ctx)

    def visitStrStartsWith(node: StrStartsWith, ctx: C): T = visitExpr(node, ctx)

    def visitStrEndsWith(node: StrEndsWith, ctx: C): T = visitExpr(node, ctx)

    def visitStrContains(node: StrContains, ctx: C): T = visitExpr(node, ctx)

    def visitStrSlice(node: StrSlice, ctx: C): T = visitExpr(node, ctx)

    def visitStrFind(node: StrFind, ctx: C): T = visitExpr(node, ctx)

    def visitStrSplit(node: StrSplit, ctx: C): T = visitExpr(node, ctx)

    def visitStrRev(node: StrRev, ctx: C): T = visitExpr(node, ctx)

    def visitCharToCode(node: CharToCode, ctx: C): T = visitExpr(node, ctx)

    def visitCharFromCode(node: CharFromCode, ctx: C): T = visitExpr(node, ctx)

    def visitStrToInt(node: StrToInt, ctx: C): T = visitExpr(node, ctx)

    def visitStrFromInt(node: StrFromInt, ctx: C): T = visitExpr(node, ctx)

    def visitArraySelect(node: ArraySelect, ctx: C): T = visitExpr(node, ctx)

    def visitApply(node: Apply, ctx: C): T = visitExpr(node, ctx)
