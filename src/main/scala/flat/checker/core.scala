package flat.checker

import flat.checker.*
import flat.regex.RegEx
import flat.util.renderSubscript
import flat.{Location, Locational, Ops}
import org.apache.commons.text.StringEscapeUtils.escapeJava

import scala.collection.immutable.Iterable
import scala.collection.mutable.ListBuffer

object core:
  trait Node

  final case class Program(vars: Map[String, Type], body: List[Stmt])

  final case class Ident(name: String) extends Node, Locational

  sealed trait Type extends Node:
    def ignoreHint: Type = this

    def toSort: Sort

  case object AnyType extends Type:
    def toSort: Sort = Sort.Top

  case object NoType extends Type:
    def toSort: Sort = Sort.Bot

  case object IntType extends Type:
    def toSort: Sort = Sort.Int

  case object BoolType extends Type:
    def toSort: Sort = Sort.Bool

  final case class LangType(re: RegEx) extends Type:
    def toSort: Sort = Sort.String

  val strType: Type = LangType(RegEx.all)

  val charType: Type = LangType(RegEx.allChar)

  def literalType(value: String): Type = LangType(RegEx.fromString(value))

  final case class TupleType(elems: Seq[Type]) extends Type:
    def toSort: Sort = Sort.Tuple(elems.map(_.toSort))

  val unitType: TupleType = TupleType(Seq.empty)

  final case class ArrayType(elem: Type) extends Type:
    def toSort: Sort = Sort.Array(elem.toSort)

  final case class FunType(args: Seq[Type], returns: Type) extends Type:
    def toSort: Sort = Sort.Fun(args.map(_.toSort), returns.toSort)

  given fromSort: Conversion[Sort, Type] = {
    case Sort.Top => AnyType
    case Sort.Bot => NoType
    case Sort.Int => IntType
    case Sort.Bool => BoolType
    case Sort.String => strType
    case Sort.Tuple(ss) => TupleType(ss.map(fromSort))
    case Sort.Array(s) => ArrayType(s)
    case Sort.Fun(ss, s) => FunType(ss.map(fromSort), s)
  }

  sealed trait Stmt extends Node:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T

  final case class Assign(id: String, value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitAssign(this, ctx)

  final case class Assert(cond: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitAssert(this, ctx)

  final case class Return() extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitReturn(this, ctx)

  final case class IfStmt(cond: Expr, body: List[Stmt], elseBody: List[Stmt]) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitIfStmt(this, ctx)

  final case class While(cond: Expr, body: List[Stmt], invariants: List[Expr]) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitWhile(this, ctx)

  final case class Invariant(id: Int, typ: Type) extends Locational

  final case class Break() extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitBreak(this, ctx)

  final case class ShowType(value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T], ctx: C): T = visitor.visitShowType(this, ctx)

  trait StmtVisitor[C, T]:
    def visitStmt(node: Stmt, ctx: C): T =
      throw UnsupportedOperationException("visit " + node.getClass.getCanonicalName)

    def visitAssign(node: Assign, ctx: C): T = visitStmt(node, ctx)

    def visitAssert(node: Assert, ctx: C): T = visitStmt(node, ctx)

    def visitReturn(node: Return, ctx: C): T = visitStmt(node, ctx)

    def visitIfStmt(node: IfStmt, ctx: C): T = visitStmt(node, ctx)

    def visitWhile(node: While, ctx: C): T = visitStmt(node, ctx)

    def visitBreak(node: Break, ctx: C): T = visitStmt(node, ctx)

    def visitShowType(node: ShowType, ctx: C): T = visitStmt(node, ctx)

  sealed trait Expr extends Node, Locational, Product:
    def walk(f: Expr => Unit): Unit =
      f(this)
      productIterator.foreach {
        case e: Expr => e.walk(f)
        case l: List[?] =>
          l.foreach {
            case e: Expr => e.walk(f)
            case _ =>
          }
        case _ =>
      }

    def walkAndCollect[T](pf: PartialFunction[Expr, T]): List[T] =
      val results = ListBuffer.empty[T]
      walk { e =>
        pf.lift.apply(e) match
          case Some(result) => results += result
          case None =>
      }
      results.toList

    def collect[T](pf: PartialFunction[Expr, T]): List[T] =
      val results = ListBuffer.empty[T]
      pf.lift.apply(this) match
        case Some(r) => results += r
        case None =>
          productIterator.foreach {
            case e: Expr => results ++= e.collect(pf)
            case l: List[?] =>
              l.foreach {
                case e: Expr => results ++= e.collect(pf)
                case _ =>
              }
            case _ =>
          }
      results.toList

    def collectFirst[T](pf: PartialFunction[Expr, T]): Option[T] = collect(pf).headOption

    def collectVars: Set[String] = collect { case Var(x) => x }.toSet

    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T

    def transform(pf: PartialFunction[Expr, Expr]): Expr

    def subst(mappings: Map[String, Expr]): Expr =
      val e = transform {
        case Var(x) if mappings.contains(x) => mappings(x)
      }
      e.optLoc = optLoc
      e

    def fillLocation(newLoc: Location): this.type =
      walk(_.setLocation(newLoc))
      this

  final case class Const(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitConst(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String = value match
      case s: String => "\"" + escapeJava(s) + "\""
      case _ => value.toString

  given Conversion[Int | Boolean | String, Const] = Const.apply

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitGlobalRef(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String = name

  final case class Var(name: String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitVar(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String =
      if name.contains('@') then
        val Array(x, ver) = name.split('@')
        x + renderSubscript(ver.toInt)
      else name

  final case class TupleExpr(elems: List[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitTupleExpr(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => TupleExpr(elems.map(_.transform(pf)))

    override def toString: String = "(" + elems.mkString(", ") + ")"

  def mkUnit: Expr = TupleExpr(Nil)

  final case class TypeTest(value: Expr, typ: Type) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitTypeTest(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => TypeTest(value.transform(pf), typ)

    override def toString: String = typ match
      case LangType(r) => s"$value : $r"
      case _ => s"$value : $typ"

  // builtin functions/operations
  final case class And(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitAnd(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => And(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left && $right)"

  def mkAnd(conjuncts: Iterable[Expr]): Expr =
    if conjuncts.isEmpty then true else conjuncts.reduce(And.apply)

  def mkAnd(conjuncts: Expr*): Expr = mkAnd(conjuncts.toList)

  final case class Or(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitOr(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Or(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left || $right)"

  def mkOr(disjuncts: Iterable[Expr]): Expr =
    if disjuncts.isEmpty then false else disjuncts.reduce(Or.apply)

  def mkOr(disjuncts: Expr*): Expr = mkOr(disjuncts.toList)

  final case class Not(operand: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitNot(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Not(operand.transform(pf))

    override def toString: String = s"(!$operand)"

  final case class Ite(test: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitIte(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Ite(test.transform(pf), thenValue.transform(pf), elseValue.transform(pf))

    override def toString: String = s"(if $test then $thenValue else $elseValue)"

  export flat.Ops.CmpOp

  final case class Cmp(op: CmpOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCmp(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Cmp(op, left.transform(pf), right.transform(pf))

    override def toString: String = s"($left $op $right)"

  extension (op: CmpOp)
    def apply(left: Expr, right: Expr): Cmp = Cmp(op, left, right)

  final case class Negate(value: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitNegate(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Negate(value.transform(pf))

    override def toString: String = s"-$value"

  final case class Arith(op: ArithOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitArith(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Arith(op, left.transform(pf), right.transform(pf))

    override def toString: String = s"($left $op $right)"

  enum ArithOp:
    case ADD
    case SUB

    def unary_! : ArithOp = this match
      case ADD => SUB
      case SUB => ADD

    def apply(left: Expr, right: Expr): Arith = Arith(this, left, right)

    override def toString: String = this match
      case ADD => "+"
      case SUB => "-"

  def mkAdd(expr: Expr, value: Int): Expr = value match
    case 0 => expr
    case n if n > 0 => Arith(ArithOp.ADD, expr, n)
    case n => Arith(ArithOp.SUB, expr, n)

  inline def mkSub(expr: Expr, value: Int): Expr = mkAdd(expr, -value)

  final case class Concat(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrConcat(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Concat(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left ++ $right)"

  final case class Length(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrLen(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Length(str.transform(pf))

    override def toString: String = s"|$str|"

  final case class CharAt(str: Expr, index: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrAt(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => CharAt(str.transform(pf), index.transform(pf))

    override def toString: String = s"$str[$index]"

  final case class Substr(str: Expr, startIndex: Expr, endIndex: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrSlice(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Substr(str.transform(pf), startIndex.transform(pf), endIndex.transform(pf))

    override def toString: String = endIndex match
      case Length(s) if s == str => s"$str[$startIndex:]"
      case _ => s"$str[$startIndex:$endIndex]"

  final case class PrefixOf(prefix: Expr, str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrStartsWith(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => PrefixOf(prefix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.startsWith($prefix)"

  final case class SuffixOf(suffix: Expr, str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrEndsWith(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => SuffixOf(suffix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.endsWith($suffix)"

  final case class InfixOf(infix: Expr, str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrContains(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => InfixOf(infix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.contains($infix)"

  final case class Find(str: Expr, pat: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrFind(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Find(str.transform(pf), pat.transform(pf))

    override def toString: String = s"$str.find($pat)"

  final case class Split(str: Expr, sep: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrSplit(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Split(str.transform(pf), sep.transform(pf))

    override def toString: String = s"$str.split($sep)"

  final case class Reverse(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrRev(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Reverse(str.transform(pf))

    override def toString: String = s"$str.rev"

  final case class StrToCode(char: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCharToCode(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrToCode(char.transform(pf))

    override def toString: String = s"$char.toCode"

  final case class StrFromCode(code: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitCharFromCode(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrFromCode(code.transform(pf))

    override def toString: String = s"$code.toChar"

  final case class StrToInt(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrToInt(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrToInt(str.transform(pf))

    override def toString: String = s"$str.toInt"

  final case class StrFromInt(int: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitStrFromInt(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrFromInt(int.transform(pf))

    override def toString: String = s"$int.toStr"

  final case class ArraySelect(array: Expr, index: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitArraySelect(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => ArraySelect(array.transform(pf), index.transform(pf))

    override def toString: String = s"$array[$index]"

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T], ctx: C): T = visitor.visitApply(this, ctx)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Apply(fun.transform(pf), args.map(_.transform(pf)))

  val NoExpr: Expr = GlobalRef("")

  trait ExprVisitor[C, T]:
    def visitExpr(node: Expr, ctx: C): T =
      throw UnsupportedOperationException("visit " + node.getClass.getCanonicalName)

    def visitConst(node: Const, ctx: C): T = visitExpr(node, ctx)

    def visitGlobalRef(node: GlobalRef, ctx: C): T = visitExpr(node, ctx)

    def visitVar(node: Var, ctx: C): T = visitExpr(node, ctx)

    def visitTupleExpr(node: TupleExpr, ctx: C): T = visitExpr(node, ctx)

    def visitTypeTest(node: TypeTest, ctx: C): T = visitExpr(node, ctx)

    def visitAnd(node: And, ctx: C): T = visitExpr(node, ctx)

    def visitOr(node: Or, ctx: C): T = visitExpr(node, ctx)

    def visitNot(node: Not, ctx: C): T = visitExpr(node, ctx)

    def visitIte(node: Ite, ctx: C): T = visitExpr(node, ctx)

    def visitCmp(node: Cmp, ctx: C): T = visitExpr(node, ctx)

    def visitNegate(node: Negate, ctx: C): T = visitExpr(node, ctx)

    def visitArith(node: Arith, ctx: C): T = visitExpr(node, ctx)

    def visitStrConcat(node: Concat, ctx: C): T = visitExpr(node, ctx)

    def visitStrLen(node: Length, ctx: C): T = visitExpr(node, ctx)

    def visitStrAt(node: CharAt, ctx: C): T = visitExpr(node, ctx)

    def visitStrStartsWith(node: PrefixOf, ctx: C): T = visitExpr(node, ctx)

    def visitStrEndsWith(node: SuffixOf, ctx: C): T = visitExpr(node, ctx)

    def visitStrContains(node: InfixOf, ctx: C): T = visitExpr(node, ctx)

    def visitStrSlice(node: Substr, ctx: C): T = visitExpr(node, ctx)

    def visitStrFind(node: Find, ctx: C): T = visitExpr(node, ctx)

    def visitStrSplit(node: Split, ctx: C): T = visitExpr(node, ctx)

    def visitStrRev(node: Reverse, ctx: C): T = visitExpr(node, ctx)

    def visitCharToCode(node: StrToCode, ctx: C): T = visitExpr(node, ctx)

    def visitCharFromCode(node: StrFromCode, ctx: C): T = visitExpr(node, ctx)

    def visitStrToInt(node: StrToInt, ctx: C): T = visitExpr(node, ctx)

    def visitStrFromInt(node: StrFromInt, ctx: C): T = visitExpr(node, ctx)

    def visitArraySelect(node: ArraySelect, ctx: C): T = visitExpr(node, ctx)

    def visitApply(node: Apply, ctx: C): T = visitExpr(node, ctx)
