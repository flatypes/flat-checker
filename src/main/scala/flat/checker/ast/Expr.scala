package flat.checker.ast

import flat.regex.Domain
import flat.{Location, Locational, Ops}

import java.util.concurrent.atomic.AtomicInteger

sealed trait Expr extends Locational, Node, Sorted:
  def collectVars: Set[String] = collect { case Var(x) => x }.toSet

  def subst(m: Map[String, Expr]): Expr = transform { case Var(x) if m.contains(x) => m(x) }

  def fillLocation(newLoc: Location): this.type =
    traverse(_.asInstanceOf[Expr].setLocation(newLoc))
    this

final case class Const(value: Int | Boolean | Char | String) extends Expr:
  override def sort(using ctx: SortingContext): Sort = value match
    case _: Int => IntSort
    case _: Boolean => BoolSort
    case _: Char => CharSort
    case _: String => StringSort

final case class Var(name: String) extends Expr:
  override def sort(using ctx: SortingContext): Sort = ctx(name)

private var freshVarCounter = new AtomicInteger

object FreshVar:
  def create(): Var =
    val n = freshVarCounter.getAndIncrement()
    Var(s"$n")

  def unapply(expr: Expr): Option[String] = expr match
    case Var(x) if x.forall(_.isDigit) => Some(x)
    case _ => None

final case class Global(decl: Decl) extends Expr:
  override def sort(using ctx: SortingContext): Sort = decl.sort

final case class Lambda(params: List[Decl], body: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = FunSort(params.map(_.sort), body.sort)

final case class Apply(fun: Expr, args: List[Expr]) extends Expr:
  override def sort(using ctx: SortingContext): Sort =
    fun.sort match
      case FunSort(_, s) => s
      case _ => assert(false)

final case class RefinedBy(value: Expr, domain: Domain) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

// Boolean operations
final case class And(conjuncts: List[Expr]) extends Expr:
  require(conjuncts.length >= 2, "And must have at least two conjuncts")

  override def sort(using ctx: SortingContext): Sort = BoolSort

object And:
  def apply(value1: Expr, value2: Expr, values: Expr*): And = And(value1 :: value2 :: values.toList)

def mkAnd(conjuncts: List[Expr]): Expr = conjuncts match
  case Nil => Const(true)
  case c :: Nil => c
  case cs => And(cs)

def mkAnd(conjuncts: Expr*): Expr = mkAnd(conjuncts.toList)

final case class Or(disjuncts: List[Expr]) extends Expr:
  require(disjuncts.length >= 2, "Or must have at least two disjuncts")

  override def sort(using ctx: SortingContext): Sort = BoolSort

object Or:
  def apply(value1: Expr, value2: Expr, values: Expr*): Or = Or(value1 :: value2 :: values.toList)

def mkOr(disjuncts: List[Expr]): Expr = disjuncts match
  case Nil => Const(false)
  case d :: Nil => d
  case ds => Or(ds)

def mkOr(disjuncts: Expr*): Expr = mkOr(disjuncts.toList)

final case class Not(cond: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

  override def toString: String = s"(!$cond)"

def mkImplies(premise: Expr, conclusion: Expr): Expr = Ite(premise, conclusion, Const(true))

final case class Implies(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class Ite(cond: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = thenValue.sort

final case class Forall(params: List[Decl], body: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class Exists(params: List[Decl], body: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

// Relational Operations
export flat.Ops.CmpOp

final case class RelExpr(op: CmpOp, left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

extension (op: CmpOp)
  def apply(left: Expr, right: Expr): RelExpr = RelExpr(op, left, right)

// Arithmetic Operations
final case class Negate(value: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class Add(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class Sub(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

def mkAdd(expr: Expr, value: Int): Expr =
  val (base, k) = expr match
    case Const(n: Int) => (None, n + value)
    case Add(e, Const(n: Int)) => (Some(e), n + value)
    case Sub(e, Const(n: Int)) => (Some(e), -n + value)
    case e => (Some(e), value)
  base match
    case Some(e) =>
      if k == 0 then e
      else if k > 0 then Add(e, Const(k))
      else Sub(e, Const(-k))
    case None => Const(k)

final case class Mul(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

// Bitwise Operations
final case class BitAnd(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class BitOr(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class BitXor(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class BitShL(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class BitShR(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

// Char Operations
final case class CharToCode(chr: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class CharFromCode(code: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = CharSort

final case class CharToLower(chr: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = CharSort

final case class CharToUpper(chr: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = CharSort

final case class CharToString(chr: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

// String Operations
final case class StringLength(str: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class StringConcat(left: Expr, right: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class StringReverse(str: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class CharAt(str: Expr, idx: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class Substring(str: Expr, startIdx: Expr, endIdx: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class StringContains(str: Expr, infix: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class StringStartsWith(str: Expr, prefix: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class StringEndsWith(str: Expr, suffix: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class StringIndexOf(str: Expr, pat: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class StringSplit(str: Expr, sep: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = SeqSort(StringSort)

final case class StringToInt(str: Expr, base: Int) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class StringFromInt(int: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class StringToSet(str: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = SetSort(StringSort)

final case class StrFormat(fmt: Expr, arg: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = StringSort

final case class StrIs(str: Expr, kind: String, predicate: Char => Boolean) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class StringForall(str: Expr, predicate: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

// Tuple Operations
final case class TupleOf(elems: List[Expr]) extends Expr:
  val arity: Int = elems.length

  require(arity != 1)

  override def sort(using ctx: SortingContext): Sort = TupleSort(elems.map(_.sort))

def mkUnit: Expr = TupleOf(Nil)

final case class TupleSelect(tup: Expr, index: Int) extends Expr:
  override def sort(using ctx: SortingContext): Sort = tup.sort match
    case TupleSort(ss) => ss(index)
    case _ => assert(false)

// Seq Operations

final case class SeqOf(elems: List[Expr]) extends Expr:
  override def sort(using ctx: SortingContext): Sort = elems match
    case Nil => SeqSort(NoSort)
    case e :: _ => SeqSort(e.sort)

sealed trait ListOp extends Expr:
  val seq: Expr

final case class SeqLength(seq: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class SeqConcat(leftSeq: Expr, rightSeq: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = leftSeq.sort

def mkSeqAppend(seq: Expr, elem: Expr): Expr = SeqConcat(seq, SeqOf(List(elem)))

final case class SeqReverse(seq: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = seq.sort

final case class SeqGet(seq: Expr, idx: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = seq.sort match
    case SeqSort(s) => s
    case _ => assert(false)

def mkListHead(lst: Expr): Expr = SeqGet(lst, Const(0))

def mkListLast(lst: Expr): Expr = SeqGet(lst, Sub(SeqLength(lst), Const(1)))

final case class SeqSlice(seq: Expr, startIdx: Expr, endIdx: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = seq.sort

def mkListTail(lst: Expr): Expr = SeqSlice(lst, Const(1), SeqLength(lst))

def mkListFront(lst: Expr): Expr = SeqSlice(lst, Const(0), Sub(SeqLength(lst), Const(1)))

final case class SeqContains(seq: Expr, elem: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class SeqStartsWith(seq: Expr, prefix: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class SeqEndsWith(seq: Expr, suffix: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class SeqIndexOf(seq: Expr, elem: Expr, from: Expr, until: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class SeqCount(seq: Expr, elem: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class SeqMap(seq: Expr, fun: Expr) extends ListOp:
  override def sort(using ctx: SortingContext): Sort = fun.sort match
    case FunSort(_, s) => SeqSort(s)
    case _ => assert(false)

// Set Operations
final case class SetOf(elems: List[Expr]) extends Expr:
  override def sort(using ctx: SortingContext): Sort = elems match
    case Nil => SetSort(NoSort)
    case e :: _ => SetSort(e.sort)

final case class SetSize(set: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class SetContains(set: Expr, elem: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class Subset(leftSet: Expr, rightSet: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class SetUnion(leftSet: Expr, rightSet: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = leftSet.sort

final case class SetIntersect(leftSet: Expr, rightSet: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = leftSet.sort

final case class SetMinus(leftSet: Expr, rightSet: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = leftSet.sort

// Map Operations
final case class MapOf(items: List[Expr]) extends Expr:
  override def sort(using ctx: SortingContext): Sort = items match
    case Nil => MapSort(NoSort, NoSort)
    case item :: _ => item.sort match
      case TupleSort(List(k, v)) => MapSort(k, v)
      case _ => assert(false)

  def keys: List[Expr] = items.map:
    case TupleOf(List(k, _)) => k
    case tup => TupleSelect(tup, 0)

final case class MapSize(map: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = IntSort

final case class MapContains(map: Expr, key: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = BoolSort

final case class MapGet(map: Expr, key: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = map.sort match
    case MapSort(_, s) => s
    case _ => assert(false)

final case class MapPut(map: Expr, key: Expr, value: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = map.sort

final case class MapRemove(map: Expr, key: Expr) extends Expr:
  override def sort(using ctx: SortingContext): Sort = map.sort

// Placeholder expression for default value in operations
final case class DefaultExpr(sort: Sort) extends Expr:
  override def sort(using ctx: SortingContext): Sort = sort

val NoExpr: Expr = TupleOf(Nil)