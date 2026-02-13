package flat.checker.ast

import flat.regex.Domain
import flat.{Location, Locational, Ops}

import scala.collection.immutable.Iterable

sealed trait Expr extends Locational, Node:
  def collectVars: Set[String] = collect { case Var(x) => x }.toSet

  def subst(m: Map[String, Expr]): Expr = transform { case Var(x) if m.contains(x) => m(x) }

  def sort: Sort

  def fillLocation(newLoc: Location): this.type =
    traverse(_.asInstanceOf[Expr].setLocation(newLoc))
    this

final case class Const(value: Int | Boolean | Char | String) extends Expr:
  val sort: Sort = value match
    case _: Int => IntSort
    case _: Boolean => BoolSort
    case _: Char => CharSort
    case _: String => StringSort

final case class Local(decl: Decl) extends Expr:
  val sort: Sort = decl.sort

final case class LocalWithVersion(decl: Decl, version: Int) extends Expr:
  val sort: Sort = decl.sort

final case class Var(name: String)(val sort: Sort) extends Expr

final case class Global(decl: Decl) extends Expr:
  val sort: Sort = decl.sort

final case class Lambda(params: List[Decl], body: Expr) extends Expr:
  val sort: Sort = FunSort(params.map(_.sort), body.sort)

final case class Apply(fun: Expr, args: List[Expr]) extends Expr:
  val sort: Sort =
    fun.sort match
      case FunSort(_, s) => s
      case _ => assert(false)

final case class RefinedBy(value: Expr, domain: Domain) extends Expr:
  val sort: Sort = BoolSort

final case class Ite(cond: Expr, thenValue: Expr, elseValue: Expr) extends Expr:
  val sort: Sort = thenValue.sort

// Boolean operations
final case class And(left: Expr, right: Expr) extends Expr:
  val sort: Sort = BoolSort

def mkAnd(conjuncts: Iterable[Expr]): Expr =
  if conjuncts.isEmpty then Const(true) else conjuncts.reduce(And.apply)

def mkAnd(conjuncts: Expr*): Expr = mkAnd(conjuncts.toList)

final case class Or(left: Expr, right: Expr) extends Expr:
  val sort: Sort = BoolSort

def mkOr(disjuncts: Iterable[Expr]): Expr =
  if disjuncts.isEmpty then Const(false) else disjuncts.reduce(Or.apply)

def mkOr(disjuncts: Expr*): Expr = mkOr(disjuncts.toList)

final case class Not(value: Expr) extends Expr:
  val sort: Sort = BoolSort

  override def toString: String = s"(!$value)"

def mkImplies(premise: Expr, conclusion: Expr): Expr = Ite(premise, conclusion, Const(true))

final case class Forall(params: List[Decl], body: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class Exists(params: List[Decl], body: Expr) extends Expr:
  val sort: Sort = BoolSort

export flat.Ops.CmpOp

final case class Cmp(op: CmpOp, left: Expr, right: Expr) extends Expr:
  val sort: Sort = BoolSort

extension (op: CmpOp)
  def apply(left: Expr, right: Expr): Cmp = Cmp(op, left, right)

// Arithmetic operations
final case class Negate(value: Expr) extends Expr:
  val sort: Sort = IntSort

final case class Arith(op: ArithOp, left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

enum ArithOp:
  case ADD
  case SUB
  case MUL

  def unary_! : ArithOp = this match
    case ADD => SUB
    case SUB => ADD
    case MUL => throw IllegalArgumentException("no ! operation for MUL")

  def apply(left: Expr, right: Expr): Arith = Arith(this, left, right)

  override def toString: String = this match
    case ADD => "+"
    case SUB => "-"
    case MUL => "*"

import flat.checker.ast.ArithOp.*

def mkAdd(expr: Expr, value: Int): Expr =
  val (base, k) = expr match
    case Const(n: Int) => (None, n + value)
    case Arith(ADD, e, Const(n: Int)) => (Some(e), n + value)
    case Arith(SUB, e, Const(n: Int)) => (Some(e), -n + value)
    case e => (Some(e), value)
  base match
    case Some(e) =>
      if k == 0 then e
      else if k > 0 then Arith(ADD, e, Const(k))
      else Arith(SUB, e, Const(-k))
    case None => Const(k)

// Bitwise Operations
final case class BitAnd(left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

final case class BitOr(left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

final case class BitXor(left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

final case class BitShL(left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

final case class BitShR(left: Expr, right: Expr) extends Expr:
  val sort: Sort = IntSort

// Char Operations
final case class CharToCode(chr: Expr) extends Expr:
  val sort: Sort = IntSort

final case class CharFromCode(code: Expr) extends Expr:
  val sort: Sort = CharSort

final case class CharToLower(chr: Expr) extends Expr:
  val sort: Sort = CharSort

final case class CharToUpper(chr: Expr) extends Expr:
  val sort: Sort = CharSort

final case class CharToString(chr: Expr) extends Expr:
  val sort: Sort = StringSort

// String Operations
final case class StringLength(str: Expr) extends Expr:
  val sort: Sort = IntSort

final case class StringConcat(left: Expr, right: Expr) extends Expr:
  val sort: Sort = StringSort

final case class StringReverse(str: Expr) extends Expr:
  val sort: Sort = StringSort

final case class CharAt(str: Expr, idx: Expr) extends Expr:
  val sort: Sort = StringSort

final case class Substring(str: Expr, startIdx: Expr, endIdx: Expr) extends Expr:
  val sort: Sort = StringSort

final case class StringContains(str: Expr, infix: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class StringStartsWith(str: Expr, prefix: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class StringEndsWith(str: Expr, suffix: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class StringIndexOf(str: Expr, pat: Expr) extends Expr:
  val sort: Sort = IntSort

final case class StringSplit(str: Expr, sep: Expr) extends Expr:
  val sort: Sort = SeqSort(StringSort)

final case class StringToInt(str: Expr, base: Expr) extends Expr:
  val sort: Sort = IntSort

final case class StringFromInt(int: Expr) extends Expr:
  val sort: Sort = StringSort

final case class StringToSet(str: Expr) extends Expr:
  val sort: Sort = SetSort(StringSort)

final case class StrFormat(fmt: Expr, arg: Expr) extends Expr:
  val sort: Sort = StringSort

final case class StrIs(str: Expr, kind: String, predicate: Char => Boolean) extends Expr:
  val sort: Sort = BoolSort

// Tuple Operations
final case class TupleOf(elems: List[Expr]) extends Expr:
  val arity: Int = elems.length

  require(arity != 1)

  val sort: Sort = TupleSort(elems.map(_.sort))

def mkUnit: Expr = TupleOf(Nil)

final case class TupleSelect(tup: Expr, index: Int) extends Expr:
  val sort: Sort = tup.sort match
    case TupleSort(ss) => ss(index)
    case _ => assert(false)

// Seq Operations

final case class SeqOf(elems: List[Expr]) extends Expr:
  val sort: Sort = elems match
    case Nil => SeqSort(NoSort)
    case e :: _ => SeqSort(e.sort)

sealed trait ListOp extends Expr:
  val seq: Expr

final case class SeqLength(seq: Expr) extends ListOp:
  val sort: Sort = IntSort

final case class SeqConcat(leftSeq: Expr, rightSeq: Expr) extends Expr:
  val sort: Sort = leftSeq.sort

def mkSeqAppend(seq: Expr, elem: Expr): Expr = SeqConcat(seq, SeqOf(List(elem)))

final case class SeqReverse(seq: Expr) extends Expr:
  val sort: Sort = seq.sort

final case class SeqGet(seq: Expr, idx: Expr) extends ListOp:
  val sort: Sort = seq.sort match
    case SeqSort(s) => s
    case _ => assert(false)

def mkListHead(lst: Expr): Expr = SeqGet(lst, Const(0))

def mkListLast(lst: Expr): Expr = SeqGet(lst, SUB(SeqLength(lst), Const(1)))

final case class SeqSlice(seq: Expr, startIdx: Expr, endIdx: Expr) extends ListOp:
  val sort: Sort = seq.sort

def mkListTail(lst: Expr): Expr = SeqSlice(lst, Const(1), SeqLength(lst))

def mkListFront(lst: Expr): Expr = SeqSlice(lst, Const(0), SUB(SeqLength(lst), Const(1)))

final case class SeqContains(seq: Expr, elem: Expr) extends ListOp:
  val sort: Sort = BoolSort

final case class SeqStartsWith(seq: Expr, prefix: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class SeqEndsWith(seq: Expr, suffix: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class SeqIndexOf(seq: Expr, elem: Expr, from: Expr, until: Expr) extends ListOp:
  val sort: Sort = IntSort

final case class SeqCount(seq: Expr, elem: Expr) extends ListOp:
  val sort: Sort = IntSort

final case class SeqMap(seq: Expr, fun: Expr) extends ListOp:
  def sort: Sort = fun.sort match
    case FunSort(_, s) => SeqSort(s)
    case _ => assert(false)

// Set Operations
final case class SetOf(elems: List[Expr]) extends Expr:
  val sort: Sort = elems match
    case Nil => SetSort(NoSort)
    case e :: _ => SetSort(e.sort)

final case class SetSize(set: Expr) extends Expr:
  val sort: Sort = IntSort

final case class SetContains(set: Expr, elem: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class Subset(leftSet: Expr, rightSet: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class SetUnion(leftSet: Expr, rightSet: Expr) extends Expr:
  val sort: Sort = leftSet.sort

final case class SetIntersect(leftSet: Expr, rightSet: Expr) extends Expr:
  val sort: Sort = leftSet.sort

final case class SetMinus(leftSet: Expr, rightSet: Expr) extends Expr:
  val sort: Sort = leftSet.sort

// Map Operations
final case class MapOf(items: List[Expr]) extends Expr:
  val sort: Sort = items match
    case Nil => MapSort(NoSort, NoSort)
    case item :: _ => item.sort match
      case TupleSort(List(k, v)) => MapSort(k, v)
      case _ => assert(false)

  def keys: List[Expr] = items.map:
    case TupleOf(List(k, _)) => k
    case tup => TupleSelect(tup, 0)

final case class MapSize(map: Expr) extends Expr:
  val sort: Sort = IntSort

final case class MapContains(map: Expr, key: Expr) extends Expr:
  val sort: Sort = BoolSort

final case class MapGet(map: Expr, key: Expr) extends Expr:
  val sort: Sort = map.sort match
    case MapSort(_, s) => s
    case _ => assert(false)

final case class MapPut(map: Expr, key: Expr, value: Expr) extends Expr:
  val sort: Sort = map.sort

final case class MapRemove(map: Expr, key: Expr) extends Expr:
  val sort: Sort = map.sort

val NoExpr: Expr = TupleOf(Nil)