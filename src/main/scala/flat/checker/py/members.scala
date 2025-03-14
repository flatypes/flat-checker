package flat.checker.py

import flat.checker.Sort
import flat.checker.ast.{Expr, Literal, Op, apply}

final case class MemberInfo(required: Seq[Sort], optional: Seq[(Sort, Expr)], returns: Sort,
                            builder: PartialFunction[Seq[Expr], Expr]):
  def apply(args: Seq[Expr]): Expr =
    require(1 + required.length <= args.length && args.length <= 1 + required.length + optional.length,
      s"arity mismatch: expect ${1 + required.length} (+ ${optional.length}), but found ${args.length}")
    val es = args ++ optional.drop(args.length - required.length - 1).map(_._2)
    assert(builder.isDefinedAt(es))
    builder.apply(es)

val intMemberTable = Map(
  "__pos__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(n) => n }),
  "__neg__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(n) => apply(Op.SUB, Literal(0), n) }),
  "__add__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Int, { case Seq(x, y) => apply(Op.ADD, x, y) }),
  "__sub__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Int, { case Seq(x, y) => apply(Op.SUB, x, y) }),
  "__eq__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.EQ, x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.NOT, apply(Op.EQ, x, y)) }),
  "__lt__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.LT, x, y) }),
  // x > y iff y < x
  "__gt__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.LT, y, x) }),
  // x <= y iff !(y < x)
  "__le__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.NOT, apply(Op.LT, y, x)) }),
  // x >= y iff !(x < y)
  "__ge__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.NOT, apply(Op.LT, x, y)) }),
  "__str__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(n) => apply(Op.STR_FROM_INT, n) }),
  "__chr__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(n) => apply(Op.CHAR_FROM_CODE, n) }),
)

val boolMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.EQ, x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.NOT, apply(Op.EQ, x, y)) }),
  "__and__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.AND, x, y) }),
  "__or__" -> MemberInfo(Seq(Sort.Bool), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.OR, x, y) }),
  "__not__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(x) => apply(Op.NOT, x) }),
)

val strMemberTable = Map(
  "__eq__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.EQ, x, y) }),
  "__ne__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(x, y) => apply(Op.NOT, apply(Op.EQ, x, y)) }),
  "__add__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.String, { case Seq(s1, s2) => apply(Op.CONCAT, s1, s2) }),
  "__len__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(s) => apply(Op.STR_LEN, s) }),
  "__reversed__" -> MemberInfo(Seq(), Seq(), Sort.String, { case Seq(s) => apply(Op.REVERSE, s) }),
  "__contains__" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => apply(Op.CONTAINS, s, s1) }),
  "__int__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(s) => apply(Op.STR_TO_INT, s) }),
  "__ord__" -> MemberInfo(Seq(), Seq(), Sort.Int, { case Seq(s) => apply(Op.CHAR_TO_CODE, s) }),
  "__getitem__" -> MemberInfo(Seq(Sort.Int), Seq(), Sort.String, { case Seq(s, i) => apply(Op.STR_AT, s, i) }),
  "__getitem_slice__" -> MemberInfo(Seq(Sort.Int), Seq(Sort.Int -> Literal(-1)), Sort.String,
    { case Seq(s, i, j) => apply(Op.SUBSTR, s, i, j) }),
  "find" -> MemberInfo(Seq(Sort.String), Seq(Sort.Int -> Literal(0)), Sort.Int,
    { case Seq(s, i, j) => apply(Op.INDEX_OF, s, i, j) }),
  "startswith" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => apply(Op.STARTS_WITH, s, s1) }),
  "endswith" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Bool, { case Seq(s, s1) => apply(Op.ENDS_WITH, s, s1) }),
  "split" -> MemberInfo(Seq(Sort.String), Seq(), Sort.Array(Sort.String), { case Seq(s, s1) => apply(Op.SPLIT, s, s1) }),
)

def arrayMemberTable(elemSort: Sort) = Map(
  "__contains__" -> MemberInfo(Seq(elemSort), Seq(), Sort.Bool, { case Seq(xs, x) => apply(Op.ARRAY_CONTAINS, xs, x) }),
  "__getitem__" -> MemberInfo(Seq(Sort.Int), Seq(), elemSort, { case Seq(xs, i) => apply(Op.ARRAY_AT, xs, i) }),
  "__setitem__" -> MemberInfo(Seq(Sort.Int, elemSort), Seq(), Sort.Bot,
    { case Seq(xs, i, x) => apply(Op.ARRAY_UPDATE, xs, i, x) }),
  "__all__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(xs) => apply(Op.ARRAY_FORALL_TRUE, xs) }),
  "__any__" -> MemberInfo(Seq(), Seq(), Sort.Bool, { case Seq(xs) => apply(Op.ARRAY_EXISTS_TRUE, xs) }),
)

def selectMember(receiverSort: Sort, memberName: String): Option[MemberInfo] =
  receiverSort match
    case Sort.Int => intMemberTable.get(memberName)
    case Sort.Bool => boolMemberTable.get(memberName)
    case Sort.String => strMemberTable.get(memberName)
    case Sort.Array(s) => arrayMemberTable(s).get(memberName)
    case _ => None