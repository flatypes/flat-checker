package flat.checker.py

import flat.checker.ast as ir
import flat.regex.*

enum Type:
  case TopType
  case NoType
  case IntType
  case BoolType
  case StringType
  case FunType(args: List[Type], ret: Type)
  case TupleType(args: List[Type])
  case ListType(elem: Type)
  case SetType(elem: Type)
  case MapType(key: Type, value: Type)
  case RefinedType(baseType: Type, domain: Domain)

  def show: String = this match
    case TopType => "Any"
    case NoType => "?"
    case IntType => "int"
    case BoolType => "bool"
    case StringType => "str"
    case FunType(args, ret) => s"Callable[[${args.map(_.show).mkString(", ")}], ${ret.show}]"
    case TupleType(args) => s"tuple[${args.map(_.show).mkString(", ")}]"
    case ListType(elem) => s"list[${elem.show}]"
    case SetType(elem) => s"set[${elem.show}]"
    case MapType(key, value) => s"dict[${key.show}, ${value.show}]"
    case RefinedType(baseType, domain) => s"{x: ${baseType.show} | $domain}"

  def split: (Type, Option[Domain]) = this match
    case TupleType(ts) =>
      val (bases, refinements) = ts.map(_.split).unzip
      val refinement =
        if refinements.forall(_.isEmpty) then None
        else Some(ProductDomain(refinements.map(_.getOrElse(TopDomain))))
      (TupleType(bases), refinement)
    case ListType(elem) =>
      val (base, refinement) = elem.split
      (ListType(base), refinement.map(SeqDomain(_)))
    case SetType(elem) =>
      val (base, refinement) = elem.split
      (SetType(base), refinement.map(SetDomain(_)))
    case RefinedType(base, domain) => (base, Some(domain))
    case _ => (this, None)

  def base: Type = split._1

  def <=(that: Type): Boolean = (this, that) match
    case (NoType, _) | (_, NoType) => true
    case (_, TopType) => true
    case (FunType(xs, x), FunType(ys, y)) => (ys zip xs).forall(_ <= _) && (x <= y)
    case _ => this == that

  def toSort: ir.Sort = this match
    case TopType => ir.TopSort
    case NoType => ir.NoSort
    case IntType => ir.IntSort
    case BoolType => ir.BoolSort
    case StringType => ir.StringSort
    case FunType(args, ret) => ir.FunSort(args.map(_.toSort), ret.toSort)
    case TupleType(args) => ir.TupleSort(args.map(_.toSort))
    case ListType(elem) => ir.SeqSort(elem.toSort)
    case SetType(elem) => ir.SetSort(elem.toSort)
    case MapType(key, value) => ir.MapSort(key.toSort, value.toSort)
    case RefinedType(baseType, _) => baseType.toSort

object Type:
  val UnitType: Type = TupleType(Nil)
