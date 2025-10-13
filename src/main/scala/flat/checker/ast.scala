package flat.checker

import flat.checker.*
import flat.regex.RegEx
import flat.util.renderSubscript
import flat.{Location, Locational, Ops}
import org.apache.commons.text.StringEscapeUtils.escapeJava

import scala.collection.immutable.Iterable
import scala.collection.mutable.ListBuffer

object ast:
  trait Node

  final case class Module(body: List[FunDef])

  final case class FunDef(ident: Ident, params: List[VarDef], returns: Type, locals: List[VarDef], body: List[Stmt])

  final case class VarDef(ident: Ident, typ: Type)

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
    def toSort: Sort = Sort.I

  case object BoolType extends Type:
    def toSort: Sort = Sort.B

  final case class LangType(re: RegEx) extends Type:
    def toSort: Sort = Sort.S

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
    case Sort.I => IntType
    case Sort.B => BoolType
    case Sort.S => strType
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
    def sort: Sort

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
    def sort: Sort = value match
      case _: Int => Sort.I
      case _: Boolean => Sort.B
      case _: String => Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String = value match
      case s: String => "\"" + escapeJava(s) + "\""
      case _ => value.toString

  given Conversion[Int | Boolean | String, Const] = Const.apply

  final case class GlobalRef(name: String) extends Expr:
    def sort: Sort = Sort.Bot

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String = name

  final case class Var(name: String) extends Expr:
    private var theSort = Sort.Bot

    def sort: Sort = theSort

    def withSort(s: Sort): this.type =
      require(theSort == Sort.Bot)
      theSort = s
      this

    def transform(pf: PartialFunction[Expr, Expr]): Expr = pf.lift.apply(this) match
      case Some(e) => e
      case None => this

    override def toString: String =
      if name.contains('@') then
        val Array(x, ver) = name.split('@')
        x + renderSubscript(ver.toInt)
      else name

  final case class TupleExpr(elems: List[Expr]) extends Expr:
    def sort: Sort = Sort.Tuple(elems.map(_.sort))

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => TupleExpr(elems.map(_.transform(pf)))

    override def toString: String = "(" + elems.mkString(", ") + ")"

  def mkUnit: Expr = TupleExpr(Nil)

  final case class TypeTest(value: Expr, typ: Type) extends Expr:
    def sort: Sort = Sort.B

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => TypeTest(value.transform(pf), typ)

    override def toString: String = typ match
      case LangType(r) => s"$value : $r"
      case _ => s"$value : $typ"

  // Boolean operations
  final case class And(left: Expr, right: Expr) extends Expr:
    def sort: Sort = Sort.B

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => And(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left && $right)"

  def mkAnd(conjuncts: Iterable[Expr]): Expr =
    if conjuncts.isEmpty then true else conjuncts.reduce(And.apply)

  def mkAnd(conjuncts: Expr*): Expr = mkAnd(conjuncts.toList)

  final case class Or(left: Expr, right: Expr) extends Expr:
    def sort: Sort = Sort.B

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Or(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left || $right)"

  def mkOr(disjuncts: Iterable[Expr]): Expr =
    if disjuncts.isEmpty then false else disjuncts.reduce(Or.apply)

  def mkOr(disjuncts: Expr*): Expr = mkOr(disjuncts.toList)

  final case class Not(operand: Expr) extends Expr:
    def sort: Sort = Sort.B

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Not(operand.transform(pf))

    override def toString: String = s"(!$operand)"

  final case class Ite(test: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
    def sort: Sort = thenValue.sort

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Ite(test.transform(pf), thenValue.transform(pf), elseValue.transform(pf))

    override def toString: String = s"(if $test then $thenValue else $elseValue)"

  export flat.Ops.CmpOp

  final case class Cmp(op: CmpOp, left: Expr, right: Expr) extends Expr:
    def sort: Sort = Sort.B

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Cmp(op, left.transform(pf), right.transform(pf))

    override def toString: String = s"($left $op $right)"

  extension (op: CmpOp)
    def apply(left: Expr, right: Expr): Cmp = Cmp(op, left, right)

  // Arithmetic operations
  final case class Negate(value: Expr) extends Expr:
    def sort: Sort = Sort.I

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Negate(value.transform(pf))

    override def toString: String = s"-$value"

  final case class Arith(op: ArithOp, left: Expr, right: Expr) extends Expr:
    def sort: Sort = Sort.I

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

  // String Operations

  final case class Concat(left: Expr, right: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Concat(left.transform(pf), right.transform(pf))

    override def toString: String = s"($left ++ $right)"

  final case class Length(str: Expr) extends Expr:
    def sort: Sort = Sort.I

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Length(str.transform(pf))

    override def toString: String = s"|$str|"

  final case class CharAt(str: Expr, index: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => CharAt(str.transform(pf), index.transform(pf))

    override def toString: String = s"$str[$index]"

  final case class Substr(str: Expr, startIndex: Expr, endIndex: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Substr(str.transform(pf), startIndex.transform(pf), endIndex.transform(pf))

    override def toString: String = endIndex match
      case Length(s) if s == str => s"$str[$startIndex:]"
      case _ => s"$str[$startIndex:$endIndex]"

  /** String Testing Operations. */
  sealed trait StrTest extends Expr:
    def sort: Sort = Sort.B

  final case class PrefixOf(prefix: Expr, str: Expr) extends StrTest:
    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => PrefixOf(prefix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.startsWith($prefix)"

  final case class SuffixOf(suffix: Expr, str: Expr) extends StrTest:
    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => SuffixOf(suffix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.endsWith($suffix)"

  final case class InfixOf(infix: Expr, str: Expr) extends StrTest:
    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => InfixOf(infix.transform(pf), str.transform(pf))

    override def toString: String = s"$str.contains($infix)"

  final case class Find(str: Expr, pat: Expr) extends Expr:
    def sort: Sort = Sort.I

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Find(str.transform(pf), pat.transform(pf))

    override def toString: String = s"$str.find($pat)"

  final case class Split(str: Expr, sep: Expr) extends Expr:
    def sort: Sort = Sort.Array(Sort.S)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Split(str.transform(pf), sep.transform(pf))

    override def toString: String = s"$str.split($sep)"

  final case class Reverse(str: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Reverse(str.transform(pf))

    override def toString: String = s"$str.rev"

  final case class StrToCode(char: Expr) extends Expr:
    def sort: Sort = Sort.I

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrToCode(char.transform(pf))

    override def toString: String = s"$char.toCode"

  final case class StrFromCode(code: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrFromCode(code.transform(pf))

    override def toString: String = s"$code.toChar"

  final case class StrToInt(str: Expr) extends Expr:
    def sort: Sort = Sort.I

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrToInt(str.transform(pf))

    override def toString: String = s"$str.toInt"

  final case class StrFromInt(int: Expr) extends Expr:
    def sort: Sort = Sort.S

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => StrFromInt(int.transform(pf))

    override def toString: String = s"$int.toStr"

  final case class ArraySelect(array: Expr, index: Expr) extends Expr:
    def sort: Sort = array.sort match
      case Sort.Array(s) => s
      case _ => assert(false)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => ArraySelect(array.transform(pf), index.transform(pf))

    override def toString: String = s"$array[$index]"

  final case class Apply(fun: Expr, args: Seq[Expr]) extends Expr:
    def sort: Sort = fun.sort match
      case Sort.Fun(_, s) => s
      case _ => assert(false)

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => Apply(fun.transform(pf), args.map(_.transform(pf)))

  val NoExpr: Expr = GlobalRef("")
