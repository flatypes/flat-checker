package flat.checker.py

import flat.checker.ast.*

final case class MemberInfo(required: Seq[Sort], optional: Seq[(Sort, Expr)], returns: Sort,
                            builder: PartialFunction[Seq[Expr], Expr],
                            preCond: Option[PartialFunction[Seq[Expr], Expr]] = None):
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
  "__add__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => ADD(x, y) }),
  "__sub__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => SUB(x, y) }),
  "__mul__" -> MemberInfo(Seq(IntSort), Seq(), IntSort, { case Seq(x, y) => MUL(x, y) }),
  "__eq__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => NE(x, y) }),
  "__lt__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => LT(x, y) }),
  "__gt__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => GT(x, y) }),
  "__le__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => LE(x, y) }),
  "__ge__" -> MemberInfo(Seq(IntSort), Seq(), BoolSort, { case Seq(x, y) => GE(x, y) }),
  "__str__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(n) => StrFromInt(n) }),
  "__chr__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(n) => StrFromCode(n) }),
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
  "__add__" -> MemberInfo(Seq(StrSort), Seq(), StrSort, { case Seq(s1, s2) => Concat(s1, s2) }),
  "__len__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(s) => Length(s) }),
  "__reversed__" -> MemberInfo(Seq(), Seq(), StrSort, { case Seq(s) => Reverse(s) }),
  "__contains__" -> MemberInfo(Seq(StrSort), Seq(), BoolSort, { case Seq(s, s1) => InfixOf(s1, s) }),
  "__int__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(s) => StrToInt(s) }),
  "__ord__" -> MemberInfo(Seq(), Seq(), IntSort, { case Seq(c) => StrToCode(c) }),
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
  "split" -> MemberInfo(Seq(StrSort), Seq(), ArraySort(StrSort), { case Seq(s, s1) => Split(s, s1) }),
)

def arrayMemberTable(elemSort: Sort) = Map(
  "__contains__" -> MemberInfo(Seq(elemSort), Seq(), BoolSort, { case Seq(xs, x) => ??? }),
  "__getitem__" -> MemberInfo(Seq(IntSort), Seq(), elemSort, { case Seq(xs, i) => ArrSelect(xs, i) }),
)

def dictMemberTable(keySort: Sort, valueSort: Sort) = Map(
  "__contains__" -> MemberInfo(Seq(keySort), Seq(), BoolSort, { case Seq(d, k) => DictContainsKey(d, k) }),
  "__getitem__" -> MemberInfo(Seq(keySort), Seq(), valueSort, { case Seq(d, k) => DictSelect(d, k) }),
)

def selectMember(receiverSort: Sort, memberName: String): Option[MemberInfo] =
  receiverSort match
    case IntSort => intMemberTable.get(memberName)
    case BoolSort => boolMemberTable.get(memberName)
    case StrSort => strMemberTable.get(memberName)
    case ArraySort(s) => arrayMemberTable(s).get(memberName)
    case DictSort(sk, sv) => dictMemberTable(sk, sv).get(memberName)
    case _ => None