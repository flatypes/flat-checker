package flat.checker.flan

import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.domain.{CharSet, StrRE}
import org.eclipse.lsp4j.{Position, Range}

object tpd:
  final case class Program(body: List[MethodDef])

  final case class MethodDef(name: String, params: List[VarDecl], returns: List[VarDecl],
                             requires: List[Expr], ensures: List[Expr],
                             locals: List[VarDecl], body: Option[List[Stmt]])
                            (val endRange: Range)

  final case class VarDecl(name: String, typ: Type)

  // Statements
  sealed trait Stmt

  final case class Assign(variable: String, value: Option[Expr]) extends Stmt

  def Assign(variable: String, value: Expr): Assign = Assign(variable, Some(value))

  @deprecated
  final case class Havoc(lhs: String) extends Stmt

  final case class ExprStmt(expr: Expr) extends Stmt

  final case class If(cond: Expr, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt

  final case class While(cond: Expr, inv: Expr, body: List[Stmt]) extends Stmt

  final case class For(name: String, iter: Expr, inv: Expr, body: List[Stmt]) extends Stmt

  final case class Break()(val range: Range) extends Stmt

  final case class Continue()(val range: Range) extends Stmt

  final case class Return()(val range: Range) extends Stmt

  final case class Assume(cond: Expr) extends Stmt

  final case class Assert(cond: Expr) extends Stmt

  final case class Abort()(val range: Range) extends Stmt

  // Types
  sealed trait Type

  case object IntType extends Type

  case object BoolType extends Type

  case object CharType extends Type

  final case class ListType(elemType: Type) extends Type

  val strType: Type = ListType(CharType)

  final case class SetType(elemType: Type) extends Type

  final case class DictType(keyType: Type, valType: Type) extends Type

  final case class RefinedType(base: Type, reft: Expr) extends Type:
    var name: String = ""

  final case class TupleType(elemTypes: List[Type]) extends Type

  def mkTupleType(elemTypes: Type*): TupleType = TupleType(elemTypes.toList)

  final case class FunType(paramTypes: List[Type], returnType: Type) extends Type:
    def arity: Int = paramTypes.length

  case object NullType extends Type

  final case class NullableType(valType: Type) extends Type

  case object NoType extends Type

  @deprecated
  final case class NormType(sort: Type, reft: Option[Expr])

  // Expressions
  sealed trait Expr extends Product:
    val range: Range

    def subtrees: List[Expr] =
      productIterator.toList.flatMap:
        case e: Expr => List(e)
        case (e: Expr) :: es => e :: es.map(_.asInstanceOf[Expr])
        case Some(e: Expr) => List(e)
        case _ => Nil

    def collect[T](pf: PartialFunction[Expr, T]): List[T] =
      if pf.isDefinedAt(this) then List(pf(this)) else subtrees.flatMap(_.collect(pf))

    def collectFirst[T](pf: PartialFunction[Expr, T]): Option[T] =
      if pf.isDefinedAt(this) then Some(pf(this)) else subtrees.collectFirst(Function.unlift(_.collectFirst(pf)))

    def rebuild(subtrees: List[Expr]): Expr

    def rebuild(f: Expr => Expr): Expr = rebuild(subtrees.map(f))

    def transform(pf: PartialFunction[Expr, Expr]): Expr =
      if pf.isDefinedAt(this) then pf(this)
      else rebuild(subtrees.map(_.transform(pf)))

    def subst(m: Map[String, Expr]): Expr = Subst.substExpr(this, m, Set.empty)

  val noRange: Range = Range(Position(0, 0), Position(0, 0))

  given Conversion[Range => Expr, Expr] = f => f(noRange)

  final case class NullLit()(val range: Range) extends Expr:
    override def rebuild(subtrees: List[Expr]): NullLit = this

  final case class IntLit(value: BigInt)(val range: Range) extends Expr:
    override def rebuild(subtrees: List[Expr]): IntLit = this

  final case class BoolLit(value: Boolean)(val range: Range) extends Expr:
    override def rebuild(subtrees: List[Expr]): BoolLit = this

  final case class CharLit(value: Char)(val range: Range) extends Expr:
    override def rebuild(subtrees: List[Expr]): CharLit = this

  final case class StrLit(value: String)(val range: Range) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrLit = this

  final case class Var(name: String)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Var = this

  final case class MethodRef(name: String)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MethodRef = this

  final case class Apply(fun: Expr, args: List[Expr])(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Apply = subtrees match
      case ef :: es => Apply(ef, es)(range)
      case _ => throw IllegalArgumentException("Apply must have at least 1 subtree")

  def mkApply(fun: Expr, args: Expr*): Apply = Apply(fun, args.toList)()

  final case class Lambda(params: List[VarDecl], body: Expr)(val range: Range = noRange) extends Expr:
    def paramNames: List[String] = params.map(_.name)

    override def rebuild(subtrees: List[Expr]): Lambda = subtrees match
      case List(b) => Lambda(params, b)(range)
      case _ => throw IllegalArgumentException("Lambda must have exactly 1 subtree")

  final case class Eq(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Eq = subtrees match
      case List(e1, e2) => Eq(e1, e2)(range)
      case _ => throw IllegalArgumentException("Eq must have exactly 2 subtrees")

  final case class Ne(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Ne = subtrees match
      case List(e1, e2) => Ne(e1, e2)(range)
      case _ => throw IllegalArgumentException("Ne must have exactly 2 subtrees")

  final case class Ite(cond: Expr, thenValue: Expr, elseValue: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Ite = subtrees match
      case List(e, e1, e2) => Ite(e, e1, e2)(range)
      case _ => throw IllegalArgumentException("Ite must have exactly 3 subtrees")

  // Boolean operations
  final case class And(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): And = subtrees match
      case List(e1, e2) => And(e1, e2)(range)
      case _ => throw IllegalArgumentException("And must have exactly 2 subtrees")

  def mkAnd(exprs: List[Expr]): Expr =
    if exprs.isEmpty then BoolLit(true) else exprs.reduce(And(_, _)())

  final case class Or(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Or = subtrees match
      case List(e1, e2) => Or(e1, e2)(range)
      case _ => throw IllegalArgumentException("Or must have exactly 2 subtrees")

  def mkOr(exprs: List[Expr]): Expr =
    if exprs.isEmpty then BoolLit(false) else exprs.reduce(Or(_, _)())

  final case class Not(cond: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Not = subtrees match
      case List(e) => Not(e)(range)
      case _ => throw IllegalArgumentException("Not must have exactly 1 subtree")

  final case class Implies(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Implies = subtrees match
      case List(e1, e2) => Implies(e1, e2)(range)
      case _ => throw IllegalArgumentException("Implies must have exactly 2 subtrees")

  // Int operations
  final case class Negate(int: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Negate = subtrees match
      case List(e) => Negate(e)(range)
      case _ => throw IllegalArgumentException("Negate must have exactly 1 subtree")

  final case class Add(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Add = subtrees match
      case List(e1, e2) => Add(e1, e2)(range)
      case _ => throw IllegalArgumentException("Add must have exactly 2 subtrees")

  final case class Sub(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Sub = subtrees match
      case List(e1, e2) => Sub(e1, e2)(range)
      case _ => throw IllegalArgumentException("Sub must have exactly 2 subtrees")

  final case class Mul(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Mul = subtrees match
      case List(e1, e2) => Mul(e1, e2)(range)
      case _ => throw IllegalArgumentException("Mul must have exactly 2 subtrees")

  final case class Div(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Div = subtrees match
      case List(e1, e2) => Div(e1, e2)(range)
      case _ => throw IllegalArgumentException("Div must have exactly 2 subtrees")

  final case class Mod(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Mod = subtrees match
      case List(e1, e2) => Mod(e1, e2)(range)
      case _ => throw IllegalArgumentException("Mod must have exactly 2 subtrees")

  final case class Le(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Le = subtrees match
      case List(e1, e2) => Le(e1, e2)(range)
      case _ => throw IllegalArgumentException("Le must have exactly 2 subtrees")

  final case class Lt(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Lt = subtrees match
      case List(e1, e2) => Lt(e1, e2)(range)
      case _ => throw IllegalArgumentException("Lt must have exactly 2 subtrees")

  final case class BitAnd(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitAnd = subtrees match
      case List(e1, e2) => BitAnd(e1, e2)(range)
      case _ => throw IllegalArgumentException("BitAnd must have exactly 2 subtrees")

  final case class BitOr(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitOr = subtrees match
      case List(e1, e2) => BitOr(e1, e2)(range)
      case _ => throw IllegalArgumentException("BitOr must have exactly 2 subtrees")

  final case class BitXor(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitXor = subtrees match
      case List(e1, e2) => BitXor(e1, e2)(range)
      case _ => throw IllegalArgumentException("BitXor must have exactly 2 subtrees")

  final case class BitNot(int: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitNot = subtrees match
      case List(e) => BitNot(e)(range)
      case _ => throw IllegalArgumentException("BitNot must have exactly 1 subtree")

  final case class BitShL(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitShL = subtrees match
      case List(e1, e2) => BitShL(e1, e2)(range)
      case _ => throw IllegalArgumentException("BitShL must have exactly 2 subtrees")

  final case class BitShR(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): BitShR = subtrees match
      case List(e1, e2) => BitShR(e1, e2)(range)
      case _ => throw IllegalArgumentException("BitShR must have exactly 2 subtrees")

  // Char operations
  final case class CharIn(chr: Expr, a: CharSet)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): CharIn = subtrees match
      case List(e) => CharIn(e, a)(range)
      case _ => throw IllegalArgumentException("CharIn must have exactly 1 subtree")

  final case class CharToInt(chr: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): CharToInt = subtrees match
      case List(e) => CharToInt(e)(range)
      case _ => throw IllegalArgumentException("CharToInt must have exactly 1 subtree")

  final case class CharFromInt(int: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): CharFromInt = subtrees match
      case List(e) => CharFromInt(e)(range)
      case _ => throw IllegalArgumentException("CharFromInt must have exactly 1 subtree")

  final case class CharToString(chr: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): CharToString = subtrees match
      case List(e) => CharToString(e)(range)
      case _ => throw IllegalArgumentException("CharToString must have exactly 1 subtree")

  // Tuple operations
  final case class TupleExpr(elems: List[Expr])(val range: Range = noRange) extends Expr:
    require(elems.length != 1, "TupleExpr must have at least 2 elements or none")

    override def rebuild(subtrees: List[Expr]): TupleExpr = TupleExpr(subtrees)(range)

  def mkTuple(elems: Expr*): TupleExpr = TupleExpr(elems.toList)()

  final case class TupleSelect(index: Int, tup: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): TupleSelect = subtrees match
      case List(t) => TupleSelect(index, t)(range)
      case _ => throw IllegalArgumentException("TupleSelect must have exactly 1 subtree")

  // Seq operations
  final case class SeqLit(elems: List[Expr])(val elemSort: Type, val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqLit = SeqLit(subtrees)(elemSort, range)

  final case class SeqLength(seq: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqLength = subtrees match
      case List(s) => SeqLength(s)(range)
      case _ => throw IllegalArgumentException("SeqLength must have exactly 1 subtree")

  final case class ListAt(seq: Expr, idx: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): ListAt = subtrees match
      case List(s, i) => ListAt(s, i)(range)
      case _ => throw IllegalArgumentException("SeqSelect must have exactly 2 subtrees")

  final case class SeqUpdate(seq: Expr, idx: Expr, elem: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqUpdate = subtrees match
      case List(e, ei, ex) => SeqUpdate(e, ei, ex)(range)
      case _ => throw IllegalArgumentException("SeqUpdate must have exactly 3 subtrees")

  final case class SeqSlice(seq: Expr, start: Expr, end: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqSlice = subtrees match
      case List(e, ei, ej) => SeqSlice(e, ei, ej)(range)
      case _ => throw IllegalArgumentException("SeqSlice must have 3 subtrees")

  object SeqSlice:
    def apply(seq: Expr, start: Expr)(range: Range): SeqSlice = SeqSlice(seq, start, SeqLength(seq))(range)

  final case class SeqConcat(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqConcat = subtrees match
      case List(e1, e2) => SeqConcat(e1, e2)(range)
      case _ => throw IllegalArgumentException("SeqConcat must have exactly 2 subtrees")

  final case class SeqReverse(seq: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqReverse = subtrees match
      case List(e) => SeqReverse(e)(range)
      case _ => throw IllegalArgumentException("SeqReverse must have exactly 1 subtree")

  final case class SeqIndexOf(seq: Expr, sub: Expr, start: Expr = IntLit(0))
                             (val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqIndexOf = subtrees match
      case List(e, et) => SeqIndexOf(e, et)(range)
      case List(e, et, ei) => SeqIndexOf(e, et, ei)(range)
      case _ => throw IllegalArgumentException("SeqIndexOf must have 2 or 3 subtrees")

  final case class ListContains(seq: Expr, elem: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): ListContains = subtrees match
      case List(e, et) => ListContains(e, et)(range)
      case _ => throw IllegalArgumentException("ListContains must have exactly 2 subtrees")

  final case class ListContainsSlice(seq: Expr, sub: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): ListContainsSlice = subtrees match
      case List(e, et) => ListContainsSlice(e, et)(range)
      case _ => throw IllegalArgumentException("ListContainsSlice must have exactly 2 subtrees")

  final case class SeqStartsWith(seq: Expr, prefix: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqStartsWith = subtrees match
      case List(e, et) => SeqStartsWith(e, et)(range)
      case _ => throw IllegalArgumentException("SeqStartsWith must have exactly 2 subtrees")

  final case class SeqEndsWith(seq: Expr, suffix: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqEndsWith = subtrees match
      case List(e, et) => SeqEndsWith(e, et)(range)
      case _ => throw IllegalArgumentException("SeqEndsWith must have exactly 2 subtrees")

  final case class SeqCount(seq: Expr, sub: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqCount = subtrees match
      case List(e, et) => SeqCount(e, et)(range)
      case _ => throw IllegalArgumentException("SeqCount must have exactly 2 subtrees")

  final case class SeqForall(seq: Expr, pred: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SeqForall = subtrees match
      case List(e, ep) => SeqForall(e, ep)(range)
      case _ => throw IllegalArgumentException("SeqForall must have exactly 2 subtrees")

  // String-specific operations
  final case class StrReplace(str: Expr, target: Expr, replacement: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrReplace = subtrees match
      case List(e, et, er) => StrReplace(e, et, er)(range)
      case _ => throw IllegalArgumentException("StrReplace must have exactly 3 subtrees")

  final case class StrSplit(str: Expr, sep: Expr, max: Option[Expr] = None)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrSplit = subtrees match
      case List(e, et) => StrSplit(e, et)(range)
      case List(e, et, em) => StrSplit(e, et, Some(em))(range)
      case _ => throw IllegalArgumentException("StringSplit must have exactly 2 subtrees")

  final case class StrJoin(sep: Expr, strs: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrJoin = subtrees match
      case List(e1, e2) => StrJoin(e1, e2)(range)
      case _ => throw IllegalArgumentException("StrJoin must have exactly 2 subtrees")

  final case class StrTrim(str: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrTrim = subtrees match
      case List(e) => StrTrim(e)(range)
      case _ => throw IllegalArgumentException("StringTrim must have exactly 1 subtree")

  final case class StrToLower(str: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrToLower = subtrees match
      case List(e) => StrToLower(e)(range)
      case _ => throw IllegalArgumentException("StringToLower must have exactly 1 subtree")

  final case class StrToUpper(str: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrToUpper = subtrees match
      case List(e) => StrToUpper(e)(range)
      case _ => throw IllegalArgumentException("StringToUpper must have exactly 1 subtree")

  final case class StrToInt(str: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrToInt = subtrees match
      case List(e) => StrToInt(e)(range)
      case _ => throw IllegalArgumentException("StringToInt must have exactly 1 subtree")

  final case class StrFromInt(int: Expr, fmt: NumStrFormat = NumStrFormat())
                             (val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrFromInt = subtrees match
      case List(e) => StrFromInt(e, fmt)(range)
      case _ => throw IllegalArgumentException("StringFromInt must have exactly 1 subtree")

  final case class StrIsAscii(str: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StrIsAscii = subtrees match
      case List(e) => StrIsAscii(e)(range)
      case _ => throw IllegalArgumentException("StrIsAscii must have exactly 1 subtree")

  // Set Operations
  final case class SetLit(elems: List[Expr])(val elemSort: Type, val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetLit = SetLit(subtrees)(elemSort, range)

  def singletonSet(elem: Expr, elemSort: Type): SetLit = SetLit(List(elem))(elemSort, elem.range)

  final case class SetSize(set: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetSize = subtrees match
      case List(e) => SetSize(e)(range)
      case _ => throw IllegalArgumentException("SetSize must have exactly 1 subtree")

  final case class SetContains(set: Expr, elem: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetContains = subtrees match
      case List(e, ex) => SetContains(e, ex)(range)
      case _ => throw IllegalArgumentException("SetContains must have exactly 2 subtrees")

  final case class Subset(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): Subset = subtrees match
      case List(e1, e2) => Subset(e1, e2)(range)
      case _ => throw IllegalArgumentException("Subset must have exactly 2 subtrees")

  final case class SetUnion(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetUnion = subtrees match
      case List(e1, e2) => SetUnion(e1, e2)(range)
      case _ => throw IllegalArgumentException("SetUnion must have exactly 2 subtrees")

  final case class SetInter(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetInter = subtrees match
      case List(e1, e2) => SetInter(e1, e2)(range)
      case _ => throw IllegalArgumentException("SetInter must have exactly 2 subtrees")

  final case class SetDiff(left: Expr, right: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetDiff = subtrees match
      case List(e1, e2) => SetDiff(e1, e2)(range)
      case _ => throw IllegalArgumentException("SetDiff must have exactly 2 subtrees")

  final case class SetForall(set: Expr, pred: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): SetForall = subtrees match
      case List(e, ep) => SetForall(e, ep)(range)
      case _ => throw IllegalArgumentException("SetForall must have exactly 2 subtrees")

  // Map operations
  final case class MapLit(keys: List[Expr], vals: List[Expr])
                         (val keySort: Type, val valSort: Type, val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapLit =
      require(subtrees.length % 2 == 0, "MapLit must have an even number of subtrees")
      val (keys, vals) = subtrees.splitAt(subtrees.length / 2)
      MapLit(keys, vals)(keySort, valSort, range)

  final case class MapKeys(map: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapKeys = subtrees match
      case List(e) => MapKeys(e)(range)
      case _ => throw IllegalArgumentException("MapKeys must have exactly 1 subtree")

  final case class MapValues(map: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapValues = subtrees match
      case List(e) => MapValues(e)(range)
      case _ => throw IllegalArgumentException("MapValues must have exactly 1 subtree")

  final case class MapItems(map: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapItems = subtrees match
      case List(e) => MapItems(e)(range)
      case _ => throw IllegalArgumentException("MapItems must have exactly 1 subtree")

  final case class MapSize(map: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapSize = subtrees match
      case List(e) => MapSize(e)(range)
      case _ => throw IllegalArgumentException("MapSize must have exactly 1 subtree")

  final case class MapContains(map: Expr, key: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapContains = subtrees match
      case List(e, ek) => MapContains(e, ek)(range)
      case _ => throw IllegalArgumentException("MapContains must have exactly 2 subtrees")

  final case class MapSelect(map: Expr, key: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapSelect = subtrees match
      case List(e, ek) => MapSelect(e, ek)(range)
      case _ => throw IllegalArgumentException("MapSelect must have exactly 2 subtrees")

  final case class MapUpdate(map: Expr, key: Expr, value: Expr)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): MapUpdate = subtrees match
      case List(e, ek, ev) => MapUpdate(e, ek, ev)(range)
      case _ => throw IllegalArgumentException("MapUpdate must have exactly 3 subtrees")

  // Domain membership
  final case class StringInLang(str: Expr, regEx: StrRE)(val range: Range = noRange) extends Expr:
    override def rebuild(subtrees: List[Expr]): StringInLang = subtrees match
      case List(e) => StringInLang(e, regEx)(range)
      case _ => throw IllegalArgumentException("StringInRegEx must have exactly 1 subtree")

  // Error handling
  case object NoExpr extends Expr:
    val range: Range = noRange

    override def rebuild(subtrees: List[Expr]): NoExpr.type = this