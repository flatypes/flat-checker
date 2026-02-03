package flat.checker.py

import flat.checker.ast.*
import flat.checker.ast.BitwiseOp.*
import flat.regex.CharExt.*

final case class MemberInfo(required: Seq[Sort], optional: Seq[(Sort, Expr)], returns: Sort,
                            builder: PartialFunction[Seq[Expr], Expr],
                            preCond: Option[PartialFunction[Seq[Expr], Expr]] = None,
                            sideEffect: Option[PartialFunction[Seq[Expr], Expr]] = None):
  def apply(args: Seq[Expr]): Expr =
    require(1 + required.length <= args.length && args.length <= 1 + required.length + optional.length,
      s"arity mismatch: expect ${1 + required.length} (+ ${optional.length}), but found ${args.length}")
    val es = args ++ optional.drop(args.length - required.length - 1).map(_._2.copyLocation(args.last))
    assert(builder.isDefinedAt(es))
    builder.apply(es)

  def applyPre(args: Seq[Expr]): Expr =
    require(1 + required.length <= args.length && args.length <= 1 + required.length + optional.length,
      s"arity mismatch: expect ${1 + required.length} (+ ${optional.length}), but found ${args.length}")
    val es = args ++ optional.drop(args.length - required.length - 1).map(_._2.copyLocation(args.last))
    assert(preCond.get.isDefinedAt(es))
    preCond.get.apply(es)

import flat.Ops.CmpOp.*
import flat.checker.ast.ArithOp.*

val intMemberTable = Map(
  "__pos__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(n) => n }),
  "__neg__" -> MemberInfo(Seq(), Seq(), IntSort, {
    case Seq(Const(k: Int)) => Const(-k)
    case Seq(n) => SUB(Const(0), n)
  }),
  // arithmetic
  "__add__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => ADD(x, y) }),
  "__sub__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => SUB(x, y) }),
  "__mul__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => MUL(x, y) }),
  // comparison
  "__eq__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => NE(x, y) }),
  "__lt__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => LT(x, y) }),
  "__gt__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => GT(x, y) }),
  "__le__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => LE(x, y) }),
  "__ge__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => GE(x, y) }),
  // string
  "__str__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(n) => StrFromInt(n) }),
  "__chr__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(n) => StrFromCode(n) }),
  // bitwise
  "__and__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => AND(x, y) }),
  "__or__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => OR(x, y) }),
  "__xor__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => XOR(x, y) }),
  "__lshift__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => SHL(x, y) }),
  "__rshift__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => SHR(x, y) })
)

val intModuleTable = Map(
  "from_bytes" -> MemberInfo(Seq(TopType, StrSort), Seq(), IntSort, { case Seq(_, bytes, order) => NoExpr })
)

val boolMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(BoolSort), Seq(), BoolSort, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(BoolSort), Seq(), BoolSort, { case Seq(x, y) => NE(x, y) }),
  "__and__" -> MemberInfo(Seq(BoolSort), Seq(), BoolSort, { case Seq(x, y) => And(x, y) }),
  "__or__" -> MemberInfo(Seq(BoolSort), Seq(), BoolSort, { case Seq(x, y) => Or(x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), BoolSort, { case Seq(x) => Not(x) }),
)

val strMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(x, y) => NE(x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), BoolSort, { case Seq(s) => EQ(s, Const("")) }),
  "__add__" -> MemberInfo(Seq(StrSort), Seq(), StrSort, { case Seq(s1, s2) => Concat(s1, s2) }),
  "__mod__" -> MemberInfo(Seq(TopType), Seq(), StrSort, { case Seq(s, x) => StrFormat(s, x) }),
  "__len__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(s) => Length(s) }),
  "__reversed__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(s) => Reverse(s) }),
  "__contains__" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(s, s1) => InfixOf(s1, s) }),
  "__int__" -> MemberInfo(Seq(), Seq(IntSort -> 10), IntSort, { case Seq(s, base) => StrToInt(s, base) },
    preCond = Some({ case Seq(s, Const(n: Int)) => StrIs(s, "isNumber", _.isNumber(n)) })),
  "__ord__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(c) => StrToCode(c) }),
  "__set__" -> MemberInfo(Seq(), Seq(), SetSort(StrSort), { case Seq(s) => StrToSet(s) }),
  "__getitem__" -> MemberInfo(Seq(IntSort), Seq(), StrSort, { case Seq(s, i) => CharAt(s, i) }),
  "__getitem_slice__" -> MemberInfo(Seq(IntSort), Seq(IntSort -> mkUnit), StrSort,
    { case Seq(s, i, j) => Substr(s, i, if j == mkUnit then Length(s) else j) }),
  "find" -> MemberInfo(Seq(StrSort), Seq(IntSort -> Const(0)), IntSort,
    { case Seq(s, t, i) => if i == Const(0) then Find(s, t) else ADD(Find(Substr(s, i, Length(s)), t), i) }),
  "index" -> MemberInfo(Seq(StrSort), Seq(IntSort -> Const(0)), IntSort,
    { case Seq(s, t, i) => if i == Const(0) then Find(s, t) else ADD(Find(Substr(s, i, Length(s)), t), i) },
    preCond = Some({ case Seq(s, t, i) => InfixOf(t, if i == Const(0) then s else Substr(s, i, Length(s))) })),
  "startswith" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(s, s1) => PrefixOf(s1, s) }),
  "endswith" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(s, s1) => SuffixOf(s1, s) }),
  "split" -> MemberInfo(Seq(StrSort), Seq(), ListSort(StrSort), { case Seq(s, s1) => Split(s, s1) }),
  // tests for all characters
  "isascii" -> MemberInfo(Seq(), Seq(), BoolSort, { case Seq(s) => StrIs(s, "ascii", _.isASCII) }),
  "isdigit" -> MemberInfo(Seq(), Seq(), BoolSort, { case Seq(s) => StrIs(s, "digit", _.isDigit) }),
)

def listMemberTable(elemSort: Sort) = Map(
  "__len__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(xs) => ListLen(xs) }),
  "__contains__" -> MemberInfo(Seq(elemSort), Seq(), BoolSort, { case Seq(xs, x) => ListContains(xs, x) }),
  "__getitem__" -> MemberInfo(Seq(IntSort), Seq(), elemSort, { case Seq(xs, i) => ListGet(xs, i) },
    preCond = Some({ case Seq(xs, i) => And(LE(0, i), LT(i, ListLen(xs))) })),
  "__getitem_slice__" -> MemberInfo(Seq(IntSort), Seq(IntSort -> mkUnit), ListSort(elemSort),
    { case Seq(xs, i, j) => ListSlice(xs, i, if j == mkUnit then ListLen(xs) else j) }),
  "index" -> MemberInfo(Seq(StrSort), Seq(IntSort -> Const(0), IntSort -> mkUnit), IntSort,
    { case Seq(xs, t, i, j) => ListIndexOf(xs, t, i, if j == mkUnit then ListLen(xs) else j) }),
  "count" -> MemberInfo(Seq(elemSort), Seq(), IntSort, { case Seq(xs, x) => ListCount(xs, x) }),
  "append" -> MemberInfo(Seq(elemSort), Seq(), UnitSort, { case Seq(_, _) => NoExpr },
    sideEffect = Some({ case Seq(xs, x) => ListAppend(xs, x) })),
  "pop" -> MemberInfo(Seq(), Seq(), elemSort, { case Seq(xs) => mkListLast(xs) },
    sideEffect = Some({ case Seq(xs) => mkListFront(xs) })),
)

def setMemberTable(elemSort: Sort) = Map(
  "issubset" -> MemberInfo(Seq(SetSort(elemSort)), Seq(), BoolSort, { case Seq(s1, s2) => Subset(s1, s2) }),
  "issuperset" -> MemberInfo(Seq(SetSort(elemSort)), Seq(), BoolSort, { case Seq(s1, s2) => Subset(s2, s1) }),
)

def dictMemberTable(keySort: Sort, valueSort: Sort) = Map(
  "__contains__" -> MemberInfo(Seq(keySort), Seq(), BoolSort, { case Seq(d, k) => MapContains(d, k) }),
  "__getitem__" -> MemberInfo(Seq(keySort), Seq(), valueSort, { case Seq(d, k) => MapLookup(d, k) }),
)

def selectMember(receiverSort: Sort, memberName: String): Option[MemberInfo] =
  receiverSort match
    case IntSort => intMemberTable.get(memberName)
    case BoolSort => boolMemberTable.get(memberName)
    case StrSort => strMemberTable.get(memberName)
    case ListSort(s) => listMemberTable(s).get(memberName)
    case SetSort(s) => setMemberTable(s).get(memberName)
    case MapSort(sk, sv) => dictMemberTable(sk, sv).get(memberName)
    case _ => None