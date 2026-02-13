package flat.checker.py

import flat.checker.ast.*
import flat.checker.py.Type.*
import flat.regex.CharExt.*

final case class MemberInfo(required: Seq[Type], optional: Seq[(Type, Expr)], returns: Type,
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
  "__pos__" -> MemberInfo(Seq(), Seq(), IntType, { case Seq(n) => n }),
  "__neg__" -> MemberInfo(Seq(), Seq(), IntType, {
    case Seq(Const(k: Int)) => Const(-k)
    case Seq(n) => SUB(Const(0), n)
  }),
  // arithmetic
  "__add__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => ADD(x, y) }),
  "__sub__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => SUB(x, y) }),
  "__mul__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => MUL(x, y) }),
  // comparison
  "__eq__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => NE(x, y) }),
  "__lt__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => LT(x, y) }),
  "__gt__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => GT(x, y) }),
  "__le__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => LE(x, y) }),
  "__ge__" -> MemberInfo(Seq(IntType), Seq(), BoolType, { case Seq(x, y) => GE(x, y) }),
  // string
  "__str__" -> MemberInfo(Seq(), Seq(), StringType, { case Seq(n) => StringFromInt(n) }),
  "__chr__" -> MemberInfo(Seq(), Seq(), StringType, { case Seq(n) => CharFromCode(n) }),
  // bitwise
  "__and__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => BitAnd(x, y) }),
  "__or__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => BitOr(x, y) }),
  "__xor__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => BitXor(x, y) }),
  "__lshift__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => BitShL(x, y) }),
  "__rshift__" -> MemberInfo(Seq(IntType), Seq(), IntType, { case Seq(x, y) => BitShR(x, y) })
)

val intModuleTable = Map(
  "from_bytes" -> MemberInfo(Seq(TopType, StringType), Seq(), IntType, { case Seq(_, bytes, order) => NoExpr })
)

val boolMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(BoolType), Seq(), BoolType, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(BoolType), Seq(), BoolType, { case Seq(x, y) => NE(x, y) }),
  "__and__" -> MemberInfo(Seq(BoolType), Seq(), BoolType, { case Seq(x, y) => And(x, y) }),
  "__or__" -> MemberInfo(Seq(BoolType), Seq(), BoolType, { case Seq(x, y) => Or(x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), BoolType, { case Seq(x) => Not(x) }),
)

val strMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(StringType), Seq(), BoolType, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(StringType), Seq(), BoolType, { case Seq(x, y) => NE(x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), BoolType, { case Seq(s) => EQ(s, Const("")) }),
  "__add__" -> MemberInfo(Seq(StringType), Seq(), StringType, { case Seq(s1, s2) => StringConcat(s1, s2) }),
  "__mod__" -> MemberInfo(Seq(TopType), Seq(), StringType, { case Seq(s, x) => StrFormat(s, x) }),
  "__len__" -> MemberInfo(Seq(), Seq(), IntType, { case Seq(s) => StringLength(s) }),
  "__reversed__" -> MemberInfo(Seq(), Seq(), StringType, { case Seq(s) => StringReverse(s) }),
  "__contains__" -> MemberInfo(Seq(StringType), Seq(), BoolType, { case Seq(s, s1) => StringContains(s, s1) }),
  "__int__" -> MemberInfo(Seq(), Seq(IntType -> Const(10)), IntType, { case Seq(s, base) => StringToInt(s, base) },
    preCond = Some({ case Seq(s, Const(n: Int)) => StrIs(s, "isNumber", _.isNumber(n)) })),
  "__ord__" -> MemberInfo(Seq(), Seq(), IntType, { case Seq(c) => CharToCode(c) }),
  "__set__" -> MemberInfo(Seq(), Seq(), SetType(StringType), { case Seq(s) => StringToSet(s) }),
  "__getitem__" -> MemberInfo(Seq(IntType), Seq(), StringType, { case Seq(s, i) => CharAt(s, i) }),
  "__getitem_slice__" -> MemberInfo(Seq(IntType), Seq(IntType -> mkUnit), StringType,
    { case Seq(s, i, j) => Substring(s, i, if j == mkUnit then StringLength(s) else j) }),
  "find" -> MemberInfo(Seq(StringType), Seq(IntType -> Const(0)), IntType,
    { case Seq(s, t, i) => if i == Const(0) then StringIndexOf(s, t) else ADD(StringIndexOf(Substring(s, i, StringLength(s)), t), i) }),
  "index" -> MemberInfo(Seq(StringType), Seq(IntType -> Const(0)), IntType,
    { case Seq(s, t, i) => if i == Const(0) then StringIndexOf(s, t) else ADD(StringIndexOf(Substring(s, i, StringLength(s)), t), i) },
    preCond = Some({ case Seq(s, t, i) => StringContains(if i == Const(0) then s else Substring(s, i, StringLength(s)), t) })),
  "startswith" -> MemberInfo(Seq(StringType), Seq(), BoolType, { case Seq(s, s1) => StringStartsWith(s, s1) }),
  "endswith" -> MemberInfo(Seq(StringType), Seq(), BoolType, { case Seq(s, s1) => StringEndsWith(s, s1) }),
  "split" -> MemberInfo(Seq(StringType), Seq(), ListType(StringType), { case Seq(s, s1) => StringSplit(s, s1) }),
  // tests for all characters
  "isascii" -> MemberInfo(Seq(), Seq(), BoolType, { case Seq(s) => StrIs(s, "ascii", _.isASCII) }),
  "isdigit" -> MemberInfo(Seq(), Seq(), BoolType, { case Seq(s) => StrIs(s, "digit", _.isDigit) }),
)

def listMemberTable(elemType: Type) = Map(
  "__len__" -> MemberInfo(Seq(), Seq(), IntType, { case Seq(xs) => SeqLength(xs) }),
  "__add__" -> MemberInfo(Seq(ListType(elemType)), Seq(), ListType(elemType),
    { case Seq(xs, ys) => SeqConcat(xs, ys) }),
  "__contains__" -> MemberInfo(Seq(elemType), Seq(), BoolType, { case Seq(xs, x) => SeqContains(xs, x) }),
  "__getitem__" -> MemberInfo(Seq(IntType), Seq(), elemType, { case Seq(xs, i) => SeqGet(xs, i) },
    preCond = Some({ case Seq(xs, i) => And(LE(Const(0), i), LT(i, SeqLength(xs))) })),
  "__getitem_slice__" -> MemberInfo(Seq(IntType), Seq(IntType -> mkUnit), ListType(elemType),
    { case Seq(xs, i, j) => SeqSlice(xs, i, if j == mkUnit then SeqLength(xs) else j) }),
  "index" -> MemberInfo(Seq(StringType), Seq(IntType -> Const(0), IntType -> mkUnit), IntType,
    { case Seq(xs, t, i, j) => SeqIndexOf(xs, t, i, if j == mkUnit then SeqLength(xs) else j) }),
  "count" -> MemberInfo(Seq(elemType), Seq(), IntType, { case Seq(xs, x) => SeqCount(xs, x) }),
  "append" -> MemberInfo(Seq(elemType), Seq(), UnitType, { case Seq(_, _) => NoExpr },
    sideEffect = Some({ case Seq(xs, x) => mkSeqAppend(xs, x) })),
  "pop" -> MemberInfo(Seq(), Seq(), elemType, { case Seq(xs) => mkListLast(xs) },
    sideEffect = Some({ case Seq(xs) => mkListFront(xs) })),
)

def setMemberTable(elemType: Type) = Map(
  "issubset" -> MemberInfo(Seq(SetType(elemType)), Seq(), BoolType, { case Seq(s1, s2) => Subset(s1, s2) }),
  "issuperset" -> MemberInfo(Seq(SetType(elemType)), Seq(), BoolType, { case Seq(s1, s2) => Subset(s2, s1) }),
)

def dictMemberTable(keyType: Type, valueType: Type) = Map(
  "__contains__" -> MemberInfo(Seq(keyType), Seq(), BoolType, { case Seq(d, k) => MapContains(d, k) }),
  "__getitem__" -> MemberInfo(Seq(keyType), Seq(), valueType, { case Seq(d, k) => MapGet(d, k) }),
)

def selectMember(receiverType: Type, memberName: String): Option[MemberInfo] =
  receiverType match
    case IntType => intMemberTable.get(memberName)
    case BoolType => boolMemberTable.get(memberName)
    case StringType => strMemberTable.get(memberName)
    case ListType(s) => listMemberTable(s).get(memberName)
    case SetType(s) => setMemberTable(s).get(memberName)
    case MapType(sk, sv) => dictMemberTable(sk, sv).get(memberName)
    case _ => None