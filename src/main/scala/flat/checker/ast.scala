package flat.checker

import flat.regex.RegEx
import flat.util.renderSubscript
import flat.{Location, Locational, Ops}
import org.apache.commons.text.StringEscapeUtils.escapeJava

import scala.collection.immutable.Iterable
import scala.collection.mutable.ListBuffer

object ast:
  final case class Module(body: List[FunDef])

  final case class FunDef(name: String, params: List[VarDef], returns: VarDef, locals: List[VarDef], body: Stmt):
    lazy val lCtx: Map[String, Type] =
      val paramCtx = Map.from(for param <- params yield param.name -> param.typ)
      val returnCtx = Map(returns.name -> returns.typ)
      val localCtx = Map.from(for param <- locals yield param.name -> param.typ)
      paramCtx ++ returnCtx ++ localCtx

  final case class VarDef(name: String, typ: Type)

  /** We adopt a refinement type system. */
  sealed trait Type:
    /** The base type is a ''sort'', which has no logical constraint. */
    def base: Sort

    /** The logical constraint that poses to this type. The self value is bound to `_`. */
    def constraint: Option[Expr]

  /** Sort: a type that is not constrained. */
  sealed trait Sort extends Type:
    def base: Sort = this

    def constraint: Option[Expr] = None

  case object TopType extends Sort

  case object NoType extends Sort

  case object IntSort extends Sort

  case object BoolSort extends Sort

  case object StrSort extends Sort

  case object UnitSort extends Sort

  final case class TupleSort(elems: List[Sort]) extends Sort

  final case class ArraySort(elem: Sort) extends Sort

  final case class FunSort(args: List[Sort], returns: Sort) extends Sort

  extension (lower: Sort)
    infix def subsortOf(upper: Sort): Boolean =
      (lower, upper) match
        case (_, TopType) | (NoType, _) => true
        case (FunSort(xs, x), FunSort(ys, y)) => (ys zip xs).forall(_ subsortOf _) && (x subsortOf y)
        case _ => lower == upper

  /** Refines a `typ` with a given `cond`. */
  class RefinedType(val typ: Type, val cond: Expr) extends Type:
    def base: Sort = typ.base

    def constraint: Option[Expr] = typ.constraint match
      case Some(b) => Some(And(b, cond))
      case None => Some(cond)

  /** Regular Language Type */
  final case class LangType(re: RegEx) extends RefinedType(StrSort, StrIn(Var("_"), re))

  val charType: Type = LangType(RegEx.allChar)

  /** Creates a literal type whose sole inhabitant is the given literal `value`. */
  def literalType(value: Int | Boolean | String): Type = value match
    case n: Int => RefinedType(IntSort, Ops.CmpOp.EQ(Var("_"), n))
    case b: Boolean => RefinedType(BoolSort, Ops.CmpOp.EQ(Var("_"), b))
    case s: String => LangType(RegEx.fromString(s))

  final case class TupleType(elems: List[Type]) extends Type:
    def base: Sort = TupleSort(elems.map(_.base))

    def constraint: Option[Expr] = Some(TypeTest(Var("_"), this))

  final case class ArrayType(elem: Type) extends Type:
    def base: Sort = ArraySort(elem.base)

    def constraint: Option[Expr] = throw UnsupportedOperationException()

  final case class FunType(args: List[Type], returns: Type) extends Type:
    def base: Sort = FunSort(args.map(_.base), returns.base)

    def constraint: Option[Expr] = throw UnsupportedOperationException()

  /** Statement. */
  sealed trait Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T

    protected def children: List[Stmt]

    def traverse(pf: PartialFunction[Stmt, Unit]): Unit =
      pf.applyOrElse(this, _ => {})
      children.foreach(_.traverse(pf))

    def collect[T](pf: PartialFunction[Stmt, T]): List[T] =
      pf.lift.apply(this) match
        case Some(x) => List(x)
        case None => children.flatMap(_.collect(pf))

    def collectFirst[T](pf: PartialFunction[Stmt, T]): Option[T] =
      pf.lift.apply(this).orElse:
        children.collectFirst(Function.unlift(_.collectFirst(pf)))

    def toBlock: List[Stmt] = List(this)

  final case class Skip() extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitSkip(this)

    protected def children: List[Stmt] = Nil

    override def toBlock: List[Stmt] = Nil

  final case class SeqStmt(first: Stmt, second: Stmt) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitSeqStmt(this)

    protected def children: List[Stmt] = List(first, second)

    override def toBlock: List[Stmt] = first.toBlock ++ second.toBlock

  def mkStmtList(body: List[Stmt]): Stmt = body match
    case Nil => Skip()
    case ss => ss.reduceRight(SeqStmt(_, _))

  final case class Assign(id: String, value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitAssign(this)

    protected def children: List[Stmt] = Nil

  final case class Assert(cond: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitAssert(this)

    protected def children: List[Stmt] = Nil

  final case class Assume(cond: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitAssume(this)

    protected def children: List[Stmt] = Nil

  final case class ShowType(value: Expr) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitShowType(this)

    protected def children: List[Stmt] = Nil

  final case class IfStmt(cond: Expr, thenBody: Stmt, elseBody: Stmt) extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitIfStmt(this)

    protected def children: List[Stmt] = List(thenBody, elseBody)

  final case class While(cond: Expr, body: Stmt) extends Stmt:
    var invariants: ListBuffer[Expr] = ListBuffer.empty

    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitWhile(this)

    protected def children: List[Stmt] = List(body)

  final case class Break() extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitBreak(this)

    protected def children: List[Stmt] = Nil

  final case class Return() extends Stmt:
    def accept[C, T](visitor: StmtVisitor[C, T])(using ctx: C): T = visitor.visitReturn(this)

    protected def children: List[Stmt] = Nil

  trait StmtVisitor[C, T]:
    def visitSkip(node: Skip)(using ctx: C): T

    def visitSeqStmt(node: SeqStmt)(using ctx: C): T

    def visitAssign(node: Assign)(using ctx: C): T

    def visitAssert(node: Assert)(using ctx: C): T

    def visitAssume(node: Assume)(using ctx: C): T

    def visitShowType(node: ShowType)(using ctx: C): T

    def visitIfStmt(node: IfStmt)(using ctx: C): T

    def visitWhile(node: While)(using ctx: C): T

    def visitBreak(node: Break)(using ctx: C): T

    def visitReturn(node: Return)(using ctx: C): T

  /** Expression. */
  sealed trait Expr extends Locational, Product:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T

    protected def children: List[Expr]

    protected def update(newChildren: List[Expr]): Expr

    def traverse(pf: PartialFunction[Expr, Unit]): Unit =
      pf.applyOrElse(this, _ => {})
      children.foreach(_.traverse(pf))

    def collect[T](pf: PartialFunction[Expr, T]): List[T] =
      pf.lift.apply(this) match
        case Some(x) => List(x)
        case None => children.flatMap(_.collect(pf))

    def collectFirst[T](pf: PartialFunction[Expr, T]): Option[T] =
      pf.lift.apply(this).orElse:
        children.collectFirst(Function.unlift(_.collectFirst(pf)))

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      pf.lift.apply(this) match
        case Some(e) => e
        case None => update(children.map(_.transform(pf)))

    def collectVars: Set[String] = collect { case Var(x) => x }.toSet

    def subst(m: Map[String, Expr]): Expr = transform { case Var(x) if m.contains(x) => m(x) }

    def sort: Sort

    def fillLocation(newLoc: Location): this.type =
      traverse(_.setLocation(newLoc))
      this

  final case class Const(value: Int | Boolean | String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitConst(this)

    protected def children: List[Expr] = Nil

    protected def update(newChildren: List[Expr]): Expr = this

    def sort: Sort = value match
      case _: Int => IntSort
      case _: Boolean => BoolSort
      case _: String => StrSort

    override def toString: String = value match
      case s: String => "\"" + escapeJava(s) + "\""
      case _ => value.toString

  given Conversion[Int | Boolean | String, Const] = Const(_)

  final case class GlobalRef(name: String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitGlobalRef(this)

    protected def children: List[Expr] = Nil

    protected def update(newChildren: List[Expr]): Expr = this

    def sort: Sort = NoType

    override def toString: String = name

  final case class Var(name: String) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitVar(this)

    protected def children: List[Expr] = Nil

    protected def update(newChildren: List[Expr]): Expr = this

    private var theSort: Sort = NoType

    def sort: Sort = theSort

    def withSort(s: Sort): this.type =
      require(theSort == NoType)
      theSort = s
      this

    override def toString: String =
      if name.contains('@') then
        val Array(x, ver) = name.split('@')
        x + renderSubscript(ver.toInt)
      else name

  final case class TupleExpr(elems: List[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitTupleExpr(this)

    protected def children: List[Expr] = elems

    protected def update(newChildren: List[Expr]): Expr = copy(elems = newChildren)

    def sort: Sort = TupleSort(elems.map(_.sort))

    override def toString: String = "(" + elems.mkString(", ") + ")"

  def mkUnit: Expr = TupleExpr(Nil)

  final case class TypeTest(value: Expr, typ: Type) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitTypeTest(this)

    protected def children: List[Expr] = List(value)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(value = e)
      case _ => assert(false)

    def sort: Sort = BoolSort

    override def toString: String = typ match
      case LangType(r) => s"$value : $r"
      case _ => s"$value : $typ"

  // Boolean operations
  final case class And(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitAnd(this)

    protected def children: List[Expr] = List(left, right)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e1, e2) => copy(left = e1, right = e2)
      case _ => assert(false)

    def sort: Sort = BoolSort

    override def toString: String = s"($left && $right)"

  def mkAnd(conjuncts: Iterable[Expr]): Expr =
    if conjuncts.isEmpty then true else conjuncts.reduce(And.apply)

  def mkAnd(conjuncts: Expr*): Expr = mkAnd(conjuncts.toList)

  final case class Or(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitOr(this)

    protected def children: List[Expr] = List(left, right)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e1, e2) => copy(left = e1, right = e2)
      case _ => assert(false)

    def sort: Sort = BoolSort

    override def toString: String = s"($left || $right)"

  def mkOr(disjuncts: Iterable[Expr]): Expr =
    if disjuncts.isEmpty then false else disjuncts.reduce(Or.apply)

  def mkOr(disjuncts: Expr*): Expr = mkOr(disjuncts.toList)

  final case class Not(value: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitNot(this)

    protected def children: List[Expr] = List(value)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(value = e)
      case _ => assert(false)

    def sort: Sort = BoolSort

    override def toString: String = s"(!$value)"

  final case class Ite(cond: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitIte(this)

    protected def children: List[Expr] = List(cond, thenValue, elseValue)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(eb, e1, e2) => copy(cond = eb, thenValue = e1, elseValue = e2)
      case _ => assert(false)

    def sort: Sort = thenValue.sort

    override def toString: String = s"(if $cond then $thenValue else $elseValue)"

  export flat.Ops.CmpOp

  final case class Cmp(op: CmpOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitCmp(this)

    protected def children: List[Expr] = List(left, right)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e1, e2) => copy(left = e1, right = e2)
      case _ => assert(false)

    def sort: Sort = BoolSort

    override def toString: String = s"($left $op $right)"

  extension (op: CmpOp)
    def apply(left: Expr, right: Expr): Cmp = Cmp(op, left, right)

  // Arithmetic operations
  final case class Negate(value: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitNegate(this)

    protected def children: List[Expr] = List(value)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(value = e)
      case _ => assert(false)

    def sort: Sort = IntSort

    override def toString: String = s"-$value"

  final case class Arith(op: ArithOp, left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitArith(this)

    protected def children: List[Expr] = List(left, right)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e1, e2) => copy(left = e1, right = e2)
      case _ => assert(false)

    def sort: Sort = IntSort

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

  import ArithOp.*

  def mkAdd(expr: Expr, value: Int): Expr =
    val (base, k) = expr match
      case Const(n: Int) => (None, n + value)
      case Arith(ADD, e, Const(n: Int)) => (Some(e), n + value)
      case Arith(SUB, e, Const(n: Int)) => (Some(e), -n + value)
      case e => (Some(e), value)
    base match
      case Some(e) =>
        if k == 0 then e
        else if k > 0 then Arith(ADD, e, k)
        else Arith(SUB, e, -k)
      case None => Const(k)

  // String Operations

  final case class Concat(left: Expr, right: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitConcat(this)

    protected def children: List[Expr] = List(left, right)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e1, e2) => copy(left = e1, right = e2)
      case _ => assert(false)

    def sort: Sort = StrSort

    override def toString: String = s"($left ++ $right)"

  final case class Length(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitLength(this)

    protected def children: List[Expr] = List(str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(str = e)
      case _ => assert(false)

    def sort: Sort = IntSort

    override def toString: String = s"|$str|"

  final case class CharAt(str: Expr, idx: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitCharAt(this)

    protected def children: List[Expr] = List(str, idx)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e, ei) => copy(str = e, idx = ei)
      case _ => assert(false)

    def sort: Sort = StrSort

    override def toString: String = s"$str[$idx]"

  final case class Substr(str: Expr, startIdx: Expr, endIdx: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitSubstr(this)

    protected def children: List[Expr] = List(str, startIdx, endIdx)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e, ei, ej) => copy(str = e, startIdx = ei, endIdx = ej)
      case _ => assert(false)

    def sort: Sort = StrSort

    override def toString: String = endIdx match
      case Length(s) if s == str => s"$str[$startIdx:]"
      case _ => s"$str[$startIdx:$endIdx]"

  /** String Testing Operations. */
  sealed trait StrTest extends Expr:
    def sort: Sort = BoolSort

  final case class PrefixOf(prefix: Expr, str: Expr) extends StrTest:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitPrefixOf(this)

    protected def children: List[Expr] = List(prefix, str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(et, e) => copy(prefix = et, str = e)
      case _ => assert(false)

    override def toString: String = s"$str.startsWith($prefix)"

  final case class SuffixOf(suffix: Expr, str: Expr) extends StrTest:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitSuffixOf(this)

    protected def children: List[Expr] = List(suffix, str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(et, e) => copy(suffix = et, str = e)
      case _ => assert(false)

    override def toString: String = s"$str.endsWith($suffix)"

  final case class InfixOf(infix: Expr, str: Expr) extends StrTest:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitInfixOf(this)

    protected def children: List[Expr] = List(infix, str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(et, e) => copy(infix = et, str = e)
      case _ => assert(false)

    override def toString: String = s"$str.contains($infix)"

  final case class Find(str: Expr, pat: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitFind(this)

    protected def children: List[Expr] = List(str, pat)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e, et) => copy(str = e, pat = et)
      case _ => assert(false)

    def sort: Sort = IntSort

    override def toString: String = s"$str.find($pat)"

  final case class Split(str: Expr, sep: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitSplit(this)

    protected def children: List[Expr] = List(str, sep)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e, et) => copy(str = e, sep = et)
      case _ => assert(false)

    def sort: Sort = ArraySort(StrSort)

    override def toString: String = s"$str.split($sep)"

  final case class Reverse(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitReverse(this)

    protected def children: List[Expr] = List(str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(str = e)
      case _ => assert(false)

    def sort: Sort = StrSort

    override def toString: String = s"$str.rev"

  final case class StrToCode(chr: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitStrToCode(this)

    protected def children: List[Expr] = List(chr)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(chr = e)
      case _ => assert(false)

    def sort: Sort = IntSort

  final case class StrFromCode(code: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitStrFromCode(this)

    protected def children: List[Expr] = List(code)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(code = e)
      case _ => assert(false)

    def sort: Sort = StrSort

  final case class StrToInt(str: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitStrToInt(this)

    protected def children: List[Expr] = List(str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(str = e)
      case _ => assert(false)

    def sort: Sort = IntSort

  final case class StrFromInt(int: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitStrFromInt(this)

    protected def children: List[Expr] = List(int)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(int = e)
      case _ => assert(false)

    def sort: Sort = StrSort

  final case class StrIn(str: Expr, re: RegEx) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitStrIn(this)

    protected def children: List[Expr] = List(str)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e) => copy(str = e)
      case _ => assert(false)

    def sort: Sort = BoolSort

  final case class ArrSelect(arr: Expr, idx: Expr) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitArrSelect(this)

    protected def children: List[Expr] = List(arr, idx)

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case List(e, ei) => copy(arr = e, idx = ei)
      case _ => assert(false)

    def sort: Sort = arr.sort match
      case ArraySort(s) => s
      case _ => assert(false)

  final case class Apply(fun: Expr, args: List[Expr]) extends Expr:
    def accept[C, T](visitor: ExprVisitor[C, T])(using ctx: C): T = visitor.visitApply(this)

    protected def children: List[Expr] = fun :: args

    protected def update(newChildren: List[Expr]): Expr = newChildren match
      case ef :: es => copy(fun = ef, args = es)
      case _ => assert(false)

    def sort: Sort = fun.sort match
      case FunSort(_, s) => s
      case _ => assert(false)

  val NoExpr: Expr = GlobalRef("")

  trait ExprVisitor[C, T]:
    def visitConst(node: Const)(using ctx: C): T

    def visitGlobalRef(node: GlobalRef)(using ctx: C): T

    def visitVar(node: Var)(using ctx: C): T

    def visitTupleExpr(node: TupleExpr)(using ctx: C): T

    def visitTypeTest(node: TypeTest)(using ctx: C): T

    def visitAnd(node: And)(using ctx: C): T

    def visitOr(node: Or)(using ctx: C): T

    def visitNot(node: Not)(using ctx: C): T

    def visitIte(node: Ite)(using ctx: C): T

    def visitCmp(node: Cmp)(using ctx: C): T

    def visitNegate(node: Negate)(using ctx: C): T

    def visitArith(node: Arith)(using ctx: C): T

    def visitConcat(node: Concat)(using ctx: C): T

    def visitLength(node: Length)(using ctx: C): T

    def visitCharAt(node: CharAt)(using ctx: C): T

    def visitSubstr(node: Substr)(using ctx: C): T

    def visitPrefixOf(node: PrefixOf)(using ctx: C): T

    def visitSuffixOf(node: SuffixOf)(using ctx: C): T

    def visitInfixOf(node: InfixOf)(using ctx: C): T

    def visitFind(node: Find)(using ctx: C): T

    def visitSplit(node: Split)(using ctx: C): T

    def visitReverse(node: Reverse)(using ctx: C): T

    def visitStrToCode(node: StrToCode)(using ctx: C): T

    def visitStrFromCode(node: StrFromCode)(using ctx: C): T

    def visitStrToInt(node: StrToInt)(using ctx: C): T

    def visitStrFromInt(node: StrFromInt)(using ctx: C): T

    def visitStrIn(node: StrIn)(using ctx: C): T

    def visitArrSelect(node: ArrSelect)(using ctx: C): T

    def visitApply(node: Apply)(using ctx: C): T