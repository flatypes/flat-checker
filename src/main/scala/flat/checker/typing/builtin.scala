package flat.checker.typing

import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.domain.{CharSet, RegEx, given}
import flat.checker.flan.sorting.toSort
import flat.checker.flan.tpd.{BoolType as bool, IntType as int, StrType as str, *}
import org.eclipse.lsp4j.Range

final case class Member(funType: FunType,
                        op: PartialFunction[List[Expr], Expr],
                        pre: PartialFunction[List[(Expr, Range)], List[Stmt]] = PartialFunction.empty):
  def impl(receiver: Expr, args: List[Expr]): Expr =
    if op.isDefinedAt(receiver :: args) then op(receiver :: args)
    else
      throw new IllegalArgumentException(s"Invalid member access: $receiver.${args.mkString(", ")}")

  def side(receiver: Expr, args: List[(Expr, Range)]): List[Stmt] =
    if pre.isDefinedAt((receiver, noRange) :: args) then pre((receiver, noRange) :: args) else Nil

object builtin:
  def accessMember(typ: Type, name: String): List[Member] = typ.toSort match
    case `int` => accessIntMember(name)
    case `bool` => accessBoolMember(name)
    case `str` => accessStrMember(name)
    case ListType(t) => accessListMember(t, name)
    case SetType(t) => accessSetMember(t, name)
    case MapType(tk, tv) => accessMapMember(tk, tv, name)
    case TupleType(ts) => accessTupleMember(ts, name)
    case _ => Nil

  extension (unit: Unit)
    private def ->(returnType: Type): FunType = FunType(Nil, returnType)

  extension (sort: Type)
    private def ->(returnType: Type): FunType = FunType(List(sort), returnType)

  extension (twoParams: (Type, Type))
    private def ->(returnType: Type): FunType = FunType(List(twoParams._1, twoParams._2), returnType)

  private def accessIntMember(name: String): List[Member] = name match
    // arithmetic operators
    case "prefix_-" => List(Member(() -> int, { case List(n) => Negate(n) }))
    case "+" => List(Member(int -> int, { case List(n1, n2) => Add(n1, n2) }))
    case "-" => List(Member(int -> int, { case List(n1, n2) => Sub(n1, n2) }))
    case "*" => List(Member(int -> int, { case List(n1, n2) => Mul(n1, n2) }))
    case "/" => List(Member(int -> int, { case List(n1, n2) => Div(n1, n2) }))
    case "%" => List(Member(int -> int, { case List(n1, n2) => Mod(n1, n2) }))
    case "**" => List(Member(int -> int, { case List(n1, n2) => Pow(n1, n2) }))
    // relational operators
    case "<=" => List(Member(int -> bool, { case List(n1, n2) => Le(n1, n2) }))
    case "<" => List(Member(int -> bool, { case List(n1, n2) => Lt(n1, n2) }))
    case ">=" => List(Member(int -> bool, { case List(n1, n2) => Le(n2, n1) }))
    case ">" => List(Member(int -> bool, { case List(n1, n2) => Lt(n2, n1) }))
    // bitwise operators
    case "prefix_~" => List(Member(() -> int, { case List(n) => BitNot(n) }))
    case "&" => List(Member(int -> int, { case List(n1, n2) => BitAnd(n1, n2) }))
    case "|" => List(Member(int -> int, { case List(n1, n2) => BitOr(n1, n2) }))
    case "^" => List(Member(int -> int, { case List(n1, n2) => BitXor(n1, n2) }))
    case "<<" => List(Member(int -> int, { case List(n1, n2) => BitShL(n1, n2) }))
    case ">>" => List(Member(int -> int, { case List(n1, n2) => BitShR(n1, n2) }))
    // conversion
    case "toChar" => List(Member(() -> str, { case List(n) => CodeToChar(n) }))
    case "toStr" => List(Member(() -> str, { case List(n) => IntFormat(n) }))
    case _ => Nil

  private def accessBoolMember(name: String): List[Member] = name match
    case "and" => List(Member(bool -> bool, { case List(b1, b2) => And(b1, b2) }))
    case "or" => List(Member(bool -> bool, { case List(b1, b2) => Or(b1, b2) }))
    case "not" | "prefix_not" => List(Member(() -> bool, { case List(b) => Not(b) }))
    case "==>" => List(Member(bool -> bool, { case List(b1, b2) => Implies(b1, b2) }))
    case _ => Nil

  private def accessStrMember(name: String): List[Member] = name match
    // access
    case "len" | "size" => List(Member(() -> int, { case List(s) => StrLength(s) }))
    case "charAt" | "select" => List(Member(int -> str, { case List(s, i) => CharAt(s, i) }))
    case "substr" | "slice" => List(
      Member(int -> str, { case List(s, i) => Substr(s, i, StrLength(s)) }),
      Member((int, int) -> str, { case List(s, i, j) => Substr(s, i, j) }))
    // construction
    case "+" => List(Member(str -> str, { case List(s1, s2) => StrConcat(s1, s2) }))
    case "reverse" => List(Member(() -> str, { case List(s) => StrReverse(s) }))
    case "join" => List(Member(ListType(str) -> str, { case List(s, es) => StrJoin(s, es) }))
    // test
    case "startsWith" => List(Member(str -> bool, { case List(s, t) => StrStartsWith(s, t) }))
    case "endsWith" => List(Member(str -> bool, { case List(s, t) => StrEndsWith(s, t) }))
    case "isAscii" => List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.ascii).star) }))
    case "isAsciiLower" =>
      List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.asciiLower).star) }))
    case "isAsciiUpper" =>
      List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.asciiUpper).star) }))
    case "isAsciiLetter" =>
      List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.asciiLetter).star) }))
    case "isAsciiDigit" =>
      List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.asciiDecimal).star) }))
    case "isAsciiSpace" =>
      List(Member(() -> bool, { case List(s) => StrIn(s, RegEx.symbolSet(CharSet.asciiSpace).star) }))
    // search
    case "contains" => List(Member(str -> bool, { case List(s, t) => StrContains(s, t) }))
    case "indexOf" => List(
      Member(str -> int, { case List(s, t) => StrIndexOf(s, t) }),
      Member((str, int) -> int, { case List(s, t, i) => StrIndexOf(s, t, i) }))
    case "count" => List(Member(str -> int, { case List(s, t) => StrCount(s, t) }))
    case "split" => List(
      Member(str -> ListType(str), { case List(s, t) => StrSplit(s, t) }),
      Member((str, int) -> ListType(str), { case List(s, t, k) => StrSplit(s, t, Some(k)) }))
    case "replace" => List(Member((str, str) -> str, { case List(s, t1, t2) => StrReplace(s, t1, t2) }))
    // conversion
    case "trim" => List(Member(() -> str, { case List(s) => StrTrim(s) }))
    case "toLower" => List(Member(() -> str, { case List(s) => StrToLower(s) }))
    case "toUpper" => List(Member(() -> str, { case List(s) => StrToUpper(s) }))
    case "toInt" => List(Member(() -> int, { case List(s) => StrToInt(s) }))
    case "toCode" => List(Member(() -> int, { case List(s) => CharToCode(s) }))
    // String higher-order
    case "forall" => List(Member((str -> bool) -> bool, { case List(s, f) => StrForall(s, f) }))
    case "map" => List(Member((str -> str) -> str, { case List(s, f) => StrMap(s, f) }))
    case _ => Nil

  private def accessListMember(t: Type, name: String): List[Member] = name match
    case "contains" => List(Member(t -> bool, { case List(s, x) => ListContains(s, x) }))
    case "len" | "size" => List(Member(() -> int, { case List(s) => ListLength(s) }))
    case "select" => List(Member(int -> t, { case List(s, i) => ListSelect(s, i) }))
    case "update" => List(Member((int, t) -> ListType(t), { case List(s, i, x) => ListUpdate(s, i, x) }))
    case "slice" => List(
      Member(int -> ListType(t), { case List(s, i) => ListSlice(s, i, ListLength(s)) }),
      Member((int, int) -> ListType(t), { case List(s, i, j) => ListSlice(s, i, j) }))
    case "+" => List(Member(ListType(t) -> ListType(t), { case List(s1, s2) => ListConcat(s1, s2) }))
    case "indexOf" => List(
      Member(t -> int, { case List(s, x) => ListIndexOf(s, x) }),
      Member((t, int) -> int, { case List(s, x, i) => ListIndexOf(s, x, i) }))

    // sugars
    case "head" => List(Member(() -> t, { case List(s) => ListSelect(s, IntConst(0)) }))
    case "tail" => List(
      Member(() -> ListType(t), { case List(s) => ListSlice(s, IntConst(1), ListLength(s)) }))
    case "init" => List(
      Member(() -> ListType(t), { case List(s) => ListSlice(s, IntConst(0), Sub(ListLength(s), IntConst(1))) }))
    case "last" => List(Member(() -> t, { case List(s) => ListSelect(s, Sub(ListLength(s), IntConst(1))) }))

    // higher-order functions
    case "forall" => List(Member((t -> bool) -> bool, { case List(s, p) => ListForall(s, p) }))
    case "exists" => List(Member((t -> bool) -> bool, { case List(s, p) => ListExists(s, p) }))
    case "map" => List(Member((t -> t) -> ListType(t), { case List(s, f) => ListMap(s, f) }))
    case "filter" => List(Member((t -> bool) -> ListType(t), { case List(s, p) => ListFilter(s, p) }))
    case _ => Nil

  private def accessSetMember(t: Type, name: String): List[Member] = name match
    case "size" => List(Member(() -> int, { case List(s) => SetSize(s) }))
    case "contains" => List(Member(t -> bool, { case List(s, x) => SetContains(s, x) }))
    case "subsetOf" => List(Member(SetType(t) -> bool, { case List(e1, e2) => Subset(e1, e2) }))
    case "|" => List(Member(SetType(t) -> SetType(t), { case List(e1, e2) => SetUnion(e1, e2) }))
    case "&" => List(Member(SetType(t) -> SetType(t), { case List(e1, e2) => SetInter(e1, e2) }))
    case "+" => List(
      Member(t -> SetType(t), { case List(s, x) => SetUnion(s, singletonSet(x, t)) }),
      Member(SetType(t) -> SetType(t), { case List(s1, s2) => SetUnion(s1, s2) }))
    case "-" => List(
      Member(t -> SetType(t), { case List(s, x) => SetDiff(s, singletonSet(x, t)) }),
      Member(SetType(t) -> SetType(t), { case List(s1, s2) => SetDiff(s1, s2) }))
    case _ => Nil

  private def accessMapMember(k: Type, v: Type, name: String): List[Member] = name match
    case "keys" => List(Member(() -> SetType(k), { case List(m) => MapKeys(m) }))
    case "values" => List(Member(() -> ListType(v), { case List(m) => MapValues(m) }))
    case "items" => List(Member(() -> ListType(mkTupleType(k, v)), { case List(m) => MapItems(m) }))
    case "size" => List(Member(() -> int, { case List(m) => MapSize(m) }))
    case "contains" => List(Member(k -> bool, { case List(m, x) => MapContains(m, x) }))
    case "select" => List(Member(k -> v, { case List(m, x) => MapSelect(m, x) }))
    case "update" => List(Member((k, v) -> MapType(k, v), { case List(m, k, v) => MapUpdate(m, k, v) }))
    case _ => Nil

  private def accessTupleMember(elemTypes: List[Type], name: String): List[Member] = name match
    case _ if name.startsWith("_") =>
      val selectors = elemTypes.indices.toList.map(i => "_" + (i + 1))
      selectors.indexOf(name) match
        case -1 => Nil
        case i => List(Member(() -> elemTypes(i), { case List(t) => TupleSelect(i, t) }))
    case _ => Nil
