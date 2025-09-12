package flat.checker.py

import flat.checker.Sort
import flat.checker.core.*

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
import flat.checker.core.ArithOp.*

val intMemberTable = Map(
  "__pos__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(n) => n }),
  "__neg__" -> MemberInfo(Seq(), Seq(), Sort.Int, {
    case Seq(Const(k: Int)) => Const(-k)
    case Seq(n) => SUB(Const(0), n)
  }),
  "__add__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Int, { case Seq(x, y) => ADD(x, y) }),
  "__sub__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Int, { case Seq(x, y) => SUB(x, y) }),
  "__eq__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => NE(x, y) }),
  "__lt__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => LT(x, y) }),
  "__gt__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => GT(x, y) }),
  "__le__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => LE(x, y) }),
  "__ge__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => GE(x, y) }),
  "__str__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(n) => StrFromInt(n) }),
  "__chr__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(n) => StrFromCode(n) }),
)

val boolMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => NE(x, y) }),
  "__and__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => And(x, y) }),
  "__or__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => Or(x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(x) => Not(x) }),
)

val strMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(x, y) => EQ(x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(x, y) => NE(x, y) }),
  "__add__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.String, { case Seq(s1, s2) => Concat(s1, s2) }),
  "__len__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(s) => Length(s) }),
  "__reversed__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(s) => Reverse(s) }),
  "__contains__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => InfixOf(s1, s) }),
  "__int__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(s) => StrToInt(s) }),
  "__ord__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(c) => StrToCode(c) }),
  "__getitem__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.String, { case Seq(s, i) => CharAt(s, i) }),
  "__getitem_slice__" -> MemberInfo(Seq(Sort.Int), Seq(Sort.Int -> mkUnit), Sort.String,
    { case Seq(s, i, j) => Substr(s, i, if j == mkUnit then Length(s) else j) }),
  "find" -> MemberInfo(Seq(Sort.String), Seq(Sort.Int -> Const(0)), Sort.Int,
    { case Seq(s, t, i) => if i == Const(0) then Find(s, t) else ADD(Find(Substr(s, i, Length(s)), t), i) }),
  "index" -> MemberInfo(Seq(Sort.String), Seq(Sort.Int -> Const(0)), Sort.Int,
    { case Seq(s, t, i) => if i == Const(0) then Find(s, t) else ADD(Find(Substr(s, i, Length(s)), t), i) },
    preCond = Some({ case Seq(s, t, i) => InfixOf(t, if i == Const(0) then s else Substr(s, i, Length(s))) })),
  "startswith" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => PrefixOf(s1, s) }),
  "endswith" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => SuffixOf(s1, s) }),
  "split" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Array(Sort.String), { case Seq(s, s1) => Split(s, s1) }),
)

def arrayMemberTable(elemSort: Sort) = Map(
  "__contains__" -> MemberInfo(Seq(elemSort), Seq(), Sort.Bool, { case Seq(xs, x) => ??? }),
  "__getitem__" -> MemberInfo(Seq(Sort.Int), Seq(), elemSort, { case Seq(xs, i) => ArraySelect(xs, i) }),
  "__setitem__" -> MemberInfo(Seq(Sort.Int, elemSort), Seq(), Sort.Bot,
    { case Seq(xs, i, x) => ??? }),
  "__all__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(xs) => ??? }),
  "__any__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(xs) => ??? }),
)

def selectMember(receiverSort: Sort, memberName: String): Option[MemberInfo] =
  receiverSort match
    case Sort.Int => intMemberTable.get(memberName)
    case Sort.Bool => boolMemberTable.get(memberName)
    case Sort.String => strMemberTable.get(memberName)
    case Sort.Array(s) => arrayMemberTable(s).get(memberName)
    case _ => None