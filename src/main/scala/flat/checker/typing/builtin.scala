package flat.checker.typing

import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.flan.tpd.*
import flat.checker.flan.{BoolSort as bool, CharSort as char, IntSort as int, stringSort as str, *}
import org.eclipse.lsp4j.Range

final case class Member(funSort: FunSort, builder: PartialFunction[List[Expr], Range => Expr]):
  def apply(receiver: Expr, args: List[Expr], range: Range): Expr =
    val argList = receiver :: args
    assert(builder.isDefinedAt(argList), s"Builder not defined for arguments: $argList")
    builder(argList)(range)

object builtin:
  def accessMember(typ: Sort, name: String): List[Member] = typ match
    case `bool` => accessBoolMember(name)
    case `int` => accessIntMember(name)
    case `char` => accessCharMember(name)
    case SeqSort(t) => accessSeqMember(t, name)
    case SetSort(t) => accessSetMember(t, name)
    case MapSort(tk, tv) => accessMapMember(tk, tv, name)
    case TupleSort(ts) => accessTupleMember(ts, name)
    case _ => Nil

  extension (unit: Unit)
    private def ->(returnSort: Sort): FunSort = FunSort(Nil, returnSort)

  extension (sort: Sort)
    private def ->(returnSort: Sort): FunSort = FunSort(List(sort), returnSort)

  extension (twoParams: (Sort, Sort))
    private def ->(returnSort: Sort): FunSort = FunSort(List(twoParams._1, twoParams._2), returnSort)

  private def accessBoolMember(name: String): List[Member] = name match
    case "prefix_!" => List(Member(() -> bool, { case List(b) => Not(b) }))
    case "&&" => List(Member(bool -> bool, { case List(b1, b2) => And(b1, b2) }))
    case "||" => List(Member(bool -> bool, { case List(b1, b2) => Or(b1, b2) }))
    case "==>" => List(Member(bool -> bool, { case List(b1, b2) => Implies(b1, b2) }))
    case _ => Nil

  private def accessIntMember(name: String): List[Member] = name match
    // unary operators
    case "prefix_-" => List(Member(() -> int, { case List(n) => Negate(n) }))
    case "prefix_~" => List(Member(() -> int, { case List(n) => BitNot(n) }))
    // arithmetic operators
    case "+" => List(Member(int -> int, { case List(n1, n2) => Add(n1, n2) }))
    case "-" => List(Member(int -> int, { case List(n1, n2) => Sub(n1, n2) }))
    case "*" => List(Member(int -> int, { case List(n1, n2) => Mul(n1, n2) }))
    // relational operators
    case "<=" => List(Member(int -> bool, { case List(n1, n2) => Le(n1, n2) }))
    case "<" => List(Member(int -> bool, { case List(n1, n2) => Lt(n1, n2) }))
    case ">=" => List(Member(int -> bool, { case List(n1, n2) => Le(n2, n1) }))
    case ">" => List(Member(int -> bool, { case List(n1, n2) => Lt(n2, n1) }))
    // bitwise operators
    case "&" => List(Member(int -> int, { case List(n1, n2) => BitAnd(n1, n2) }))
    case "|" => List(Member(int -> int, { case List(n1, n2) => BitOr(n1, n2) }))
    case "^" => List(Member(int -> int, { case List(n1, n2) => BitXor(n1, n2) }))
    case "<<" => List(Member(int -> int, { case List(n1, n2) => BitShL(n1, n2) }))
    case ">>" => List(Member(int -> int, { case List(n1, n2) => BitShR(n1, n2) }))
    // conversion
    case "toChar" => List(Member(() -> char, { case List(n) => CharFromInt(n) }))
    case "toString" => List(Member(() -> str, { case List(n) => StrFromInt(n) }))
    case _ => Nil

  private def accessCharMember(name: String): List[Member] = name match
    // conversion
    case "toInt" => List(Member(() -> int, { case List(c) => CharToInt(c) }))
    case "toString" => List(Member(() -> str, { case List(c) => CharToString(c) }))
    case _ => Nil

  private def accessTupleMember(elemSorts: List[Sort], name: String): List[Member] = name match
    case _ if name.startsWith("_") =>
      val selectors = elemSorts.indices.toList.map("_" + _)
      selectors.indexOf(name) match
        case -1 => Nil
        case i => List(Member(() -> elemSorts(i), { case List(t) => TupleSelect(i, t) }))
    case _ => Nil

  private def accessSeqMember(t: Sort, name: String): List[Member] = name match
    case "length" | "size" => List(Member(() -> int, { case List(s) => SeqLength(s) }))
    case "select" => List(Member(int -> t, { case List(s, i) => SeqSelect(s, i) }))
    case "update" => List(Member((int, t) -> SeqSort(t), { case List(s, i, x) => SeqUpdate(s, i, x) }))
    case "slice" => List(
      Member(int -> SeqSort(t), { case List(s, i) => SeqSlice(s, i) }),
      Member((int, int) -> SeqSort(t), { case List(s, i, j) => SeqSlice(s, i, j) }))
    case "+" => List(
      Member(t -> SeqSort(t), { case List(s, x) => SeqConcat(s, unitSeq(x, t)) }),
      Member(SeqSort(t) -> SeqSort(t), { case List(s1, s2) => SeqConcat(s1, s2) }))
    case "reverse" => List(Member(() -> SeqSort(t), { case List(s) => SeqReverse(s) }))
    case "indexOf" => List(
      Member(t -> int, { case List(e, ex) => SeqIndexOf(e, unitSeq(ex, t)) }),
      Member((t, int) -> int, { case List(e, ex, ei) => SeqIndexOf(e, unitSeq(ex, t), ei) }),
      Member(SeqSort(t) -> int, { case List(e, et) => SeqIndexOf(e, et) }),
      Member((SeqSort(t), int) -> int, { case List(e, et, ei) => SeqIndexOf(e, et, ei) }))
    case "contains" => List(
      Member(t -> bool, { case List(e, ex) => SeqContains(e, unitSeq(ex, t)) }),
      Member(SeqSort(t) -> bool, { case List(e, et) => SeqContains(e, et) }))
    case "startsWith" => List(Member(SeqSort(t) -> bool, { case List(s, s1) => SeqStartsWith(s, s1) }))
    case "endsWith" => List(Member(SeqSort(t) -> bool, { case List(s, s1) => SeqEndsWith(s, s1) }))
    case "count" => List(
      Member(t -> int, { case List(e, ex) => SeqCount(e, unitSeq(ex, t)) }),
      Member(SeqSort(t) -> int, { case List(e, et) => SeqCount(e, et) }))
    case _ => if t == char then accessStringSpecificMember(name) else Nil

  private def unitSeq(elem: Expr, elemSort: Sort): Expr = elemSort match
    case `char` => CharToString(elem)(elem.range)
    case _ => SeqLit(List(elem))(elemSort, elem.range)

  private def accessStringSpecificMember(name: String): List[Member] = name match
    case "charAt" => List(Member(int -> char, { case List(s, i) => SeqSelect(s, i) }))
    case "substring" => List(
      Member(int -> str, { case List(s, i) => SeqSlice(s, i) }),
      Member((int, int) -> str, { case List(s, i, j) => SeqSlice(s, i, j) }))
    case "replace" => List(
      Member((char, str) -> str, { case List(s, c, s1) => StrReplace(s, CharToString(c)(c.range), s1) }),
      Member((str, str) -> str, { case List(s, t1, t2) => StrReplace(s, t1, t2) }))
    case "split" => List(Member(str -> SeqSort(str), { case List(s, t) => StringSplit(s, t) }))
    case "trim" => List(Member(() -> str, { case List(s) => StringTrim(s) }))
    case "toLower" => List(Member(() -> str, { case List(s) => StringToLower(s) }))
    case "toUpper" => List(Member(() -> str, { case List(s) => StringToUpper(s) }))
    case "toInt" => List(Member(() -> int, { case List(s) => StringToInt(s) }))
    case _ => Nil

  private def accessSetMember(t: Sort, name: String): List[Member] = name match
    case "size" => List(Member(() -> int, { case List(s) => SetSize(s) }))
    case "contains" => List(Member(t -> bool, { case List(s, x) => SetContains(s, x) }))
    case "subsetOf" => List(Member(SetSort(t) -> bool, { case List(e1, e2) => Subset(e1, e2) }))
    case "|" => List(Member(SetSort(t) -> SetSort(t), { case List(e1, e2) => SetUnion(e1, e2) }))
    case "&" => List(Member(SetSort(t) -> SetSort(t), { case List(e1, e2) => SetInter(e1, e2) }))
    case "+" => List(
      Member(t -> SetSort(t), { case List(s, x) => SetUnion(s, singletonSet(x, t)) }),
      Member(SetSort(t) -> SetSort(t), { case List(s1, s2) => SetUnion(s1, s2) }))
    case "-" => List(
      Member(t -> SetSort(t), { case List(s, x) => SetDiff(s, singletonSet(x, t)) }),
      Member(SetSort(t) -> SetSort(t), { case List(s1, s2) => SetDiff(s1, s2) }))
    case _ => Nil

  private def accessMapMember(k: Sort, v: Sort, name: String): List[Member] = name match
    case "keys" => List(Member(() -> SetSort(k), { case List(m) => MapKeys(m) }))
    case "values" => List(Member(() -> SeqSort(v), { case List(m) => MapValues(m) }))
    case "items" => List(Member(() -> SeqSort(mkTupleSort(k, v)), { case List(m) => MapItems(m) }))
    case "size" => List(Member(() -> int, { case List(m) => MapSize(m) }))
    case "contains" => List(Member(k -> bool, { case List(m, x) => MapContains(m, x) }))
    case "select" => List(Member(k -> v, { case List(m, x) => MapSelect(m, x) }))
    case "update" => List(Member((k, v) -> MapSort(k, v), { case List(m, k, v) => MapUpdate(m, k, v) }))
    case _ => Nil