package flat.checker.typing

import flat.checker.domain.CharREOps.NumStrFormat
import flat.checker.domain.CharSet
import flat.checker.flan.sorting.toSort
import flat.checker.flan.tpd.{BoolType as bool, CharType as char, IntType as int, StrType as str, *}
import org.eclipse.lsp4j.Range

final case class Member(funType: FunType, op: PartialFunction[List[Expr], Range => Expr]):
  def impl(receiver: Expr, args: List[Expr], span: Range): Expr =
    if op.isDefinedAt(receiver :: args) then op(receiver :: args)(span)
    else
      throw new IllegalArgumentException(s"Invalid member access: $receiver.${args.mkString(", ")}")

object builtin:
  def accessMember(typ: Type, name: String): List[Member] = typ.toSort match
    case `int` => accessIntMember(name)
    case `bool` => accessBoolMember(name)
    case `char` => accessCharMember(name)
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
    case "//" => List(Member(int -> int, { case List(n1, n2) => Div(n1, n2) }))
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
    case "__str__" | "toStr" => List(Member(() -> str, { case List(n) => IntFormat(n) }))
    case _ => Nil

  private def accessBoolMember(name: String): List[Member] = name match
    case "and" => List(Member(bool -> bool, { case List(b1, b2) => And(b1, b2) }))
    case "or" => List(Member(bool -> bool, { case List(b1, b2) => Or(b1, b2) }))
    case "not" | "prefix_not" => List(Member(() -> bool, { case List(b) => Not(b) }))
    case "==>" => List(Member(bool -> bool, { case List(b1, b2) => Implies(b1, b2) }))
    case _ => Nil

  def f(): Unit =
    val s = "hello"
    s.contains('h') // This will call the accessStrMember method with name "contains"

  private def accessCharMember(name: String): List[Member] = name match
    case "toCode" => List(Member(() -> int, { case List(c) => CharToCode(c) }))
    case "__str__" | "toStr" => List(Member(() -> str, { case List(c) => CharToStr(c) }))

    // Classification
    case "isascii" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.ascii) }))
    case "isalpha" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.asciiLetter) }))
    case "isdigit" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.asciiDecimal) }))
    case "islower" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.asciiLower) }))
    case "isupper" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.asciiUpper) }))
    case "isspace" => List(Member(() -> bool, { case List(c) => CharIn(c, CharSet.asciiSpace) }))
    case _ => Nil

  private def accessStrMember(name: String): List[Member] = name match
    case "contains" => List(
      Member(char -> bool, { case List(s, c) => StrContains(s, CharToStr(c)) }),
      Member(str -> bool, { case List(s, t) => StrContains(s, t) }))
    case "__len__" | "length" => List(Member(() -> int, { case List(s) => StrLen(s) }))
    case "__getitem__" | "charAt" => List(Member(int -> char, { case List(s, i) => CharAt(s, i) }))
    case "slice" | "substring" => List(
      Member(int -> str, { case List(s, i) => Substr(s, i, StrLen(s)) }),
      Member((int, int) -> str, { case List(s, i, j) => Substr(s, i, j) }))
    case "__add__" | "+" | "concat" => List(
      Member(char -> str, { case List(s, c) => StrConcat(s, CharToStr(c)) }),
      Member(str -> str, { case List(s1, s2) => StrConcat(s1, s2) }))
    case "startswith" | "startsWith" => List(Member(str -> bool, { case List(s, t) => StrStartsWith(s, t) }))
    case "endswith" | "endsWith" => List(Member(str -> bool, { case List(s, t) => StrEndsWith(s, t) }))
    case "find" | "indexOf" => List(
      Member(char -> int, { case List(s, c) => StrIndexOf(s, CharToStr(c)) }),
      Member((char, int) -> int, { case List(s, c, i) => StrIndexOf(s, CharToStr(c), i) }),
      Member(str -> int, { case List(s, t) => StrIndexOf(s, t) }),
      Member((str, int) -> int, { case List(s, t, i) => StrIndexOf(s, t, i) }))
    case "replace" => List(
      Member((char, char) -> str, { case List(s, c1, c2) => StrReplace(s, CharToStr(c1), CharToStr(c2)) }),
      Member((str, str) -> str, { case List(s, t1, t2) => StrReplace(s, t1, t2) }))
    case "__int__" => List(Member(() -> int, { case List(s) => StrToInt(s) }))

    // String extensions
    case "reverse" => List(Member(() -> str, { case List(s) => StrReverse(s) }))
    case "strip" | "trim" => List(Member(() -> str, { case List(s) => StrTrim(s) }))
    case "split" => List(
      Member(char -> ListType(str), { case List(s, c) => StrSplit(s, CharToStr(c)) }),
      Member(str -> ListType(str), { case List(s, t) => StrSplit(s, t) }))
    case "join" => List(Member(ListType(str) -> str, { case List(s, ts) => StrJoin(s, ts) }))
    case "count" => List(
      Member(char -> int, { case List(s, c) => StrCount(s, CharToStr(c)) }),
      Member(str -> int, { case List(s, t) => StrCount(s, t) }))

    // String higher-order
    case "forall" => List(Member((char -> bool) -> bool, { case List(s, f) => StrForall(s, f) }))
    case "map" => List(Member((char -> str) -> str, { case List(s, f) => StrMap(s, f) }))

    // String classification
    case "isascii" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.ascii) }))
    case "isalpha" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.asciiLetter) }))
    case "isdigit" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.asciiDecimal) }))
    case "islower" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.asciiLower) }))
    case "isupper" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.asciiUpper) }))
    case "isspace" => List(Member(() -> bool, { case List(s) => mkStrIs(s, CharSet.asciiSpace) }))

    // Case manipulation
    case "lower" | "toLower" => List(Member(() -> str, { case List(s) => StrToLower(s) }))
    case "upper" | "toUpper" => List(Member(() -> str, { case List(s) => StrToUpper(s) }))
    case _ => Nil

  private def mkStrIs(s: Expr, cs: CharSet): Range => Expr =
    StrForall(s, LambdaExpr(List("c" -> char), CharIn(Var("c"), cs)))

  private def accessListMember(t: Type, name: String): List[Member] = name match
    case "contains" => List(Member(t -> bool, { case List(s, x) => ListContains(s, x) }))
    case "__len__" | "size" => List(Member(() -> int, { case List(s) => ListLen(s) }))
    case "__getitem__" | "select" => List(Member(int -> t, { case List(s, i) => ElemAt(s, i) }))
    case "update" => List(Member((int, t) -> ListType(t), { case List(s, i, x) => ListUpdate(s, i, x) }))
    case "slice" => List(
      Member(int -> ListType(t), { case List(s, i) => ListSlice(s, i, ListLen(s)) }),
      Member((int, int) -> ListType(t), { case List(s, i, j) => ListSlice(s, i, j) }))
    case "__add__" | "+" => List(
      Member(t -> ListType(t), { case List(s, x) => ListConcat(s, ListExpr(List(x))(t.toSort, x.range)) }),
      Member(ListType(t) -> ListType(t), { case List(s1, s2) => ListConcat(s1, s2) }))
    case "indexOf" => List(
      Member(t -> int, { case List(s, x) => ListIndexOf(s, x) }),
      Member((t, int) -> int, { case List(s, x, i) => ListIndexOf(s, x, i) }))

    // sugars
    case "head" => List(Member(() -> t, { case List(s) => ElemAt(s, IntConst(0)) }))
    case "tail" => List(
      Member(() -> ListType(t), { case List(s) => ListSlice(s, IntConst(1), ListLen(s)) }))
    case "init" => List(
      Member(() -> ListType(t), { case List(s) => ListSlice(s, IntConst(0), Sub(ListLen(s), IntConst(1))) }))
    case "last" => List(Member(() -> t, { case List(s) => ElemAt(s, Sub(ListLen(s), IntConst(1))) }))

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
    case "__getitem__" | "select" => List(Member(k -> v, { case List(m, x) => MapSelect(m, x) }))
    case "update" => List(Member((k, v) -> MapType(k, v), { case List(m, k, v) => MapUpdate(m, k, v) }))
    case _ => Nil

  private def accessTupleMember(elemTypes: List[Type], name: String): List[Member] = name match
    case _ if name.startsWith("_") =>
      val selectors = elemTypes.indices.toList.map(i => "_" + (i + 1))
      selectors.indexOf(name) match
        case -1 => Nil
        case i => List(Member(() -> elemTypes(i), { case List(t) => TupleSelect(i, t) }))
    case _ => Nil
