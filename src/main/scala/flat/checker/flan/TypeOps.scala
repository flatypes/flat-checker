package flat.checker.flan

import flat.checker.flan.tpd.*

object TypeOps:
  extension (typ: Type)
    def isSort: Boolean = typ match
      case IntType | BoolType | CharType | `strType` | NullType | NoType => true
      case RefinedType(_, _) => false
      case ListType(t) => t.isSort
      case SetType(t) => t.isSort
      case DictType(k, v) => k.isSort && v.isSort
      case TupleType(ts) => ts.forall(_.isSort)
      case FunType(ts, r) => ts.forall(_.isSort) && r.isSort
      case NullableType(t) => t.isSort

    def erase: Type = typ match
      case IntType | BoolType | CharType | `strType` | NullType | NoType => typ
      case ListType(t) => ListType(t.erase)
      case SetType(t) => SetType(t.erase)
      case DictType(k, v) => DictType(k.erase, v.erase)
      case RefinedType(t, _) => t.erase
      case TupleType(ts) => TupleType(ts.map(_.erase))
      case FunType(ps, r) => FunType(ps.map(_.erase), r.erase)
      case NullableType(t) => NullableType(t.erase)

    infix def :<:(right: Type): Boolean =
      if typ == right then true
      else
        // require both non-refined types
        (typ, right) match
          case (NoType, _) | (_, NoType) => true
          case (ListType(s1), ListType(s2)) => s1 :<: s2
          case (SetType(s1), SetType(s2)) => s1 :<: s2
          case (DictType(sk1, sv1), DictType(sk2, sv2)) => sk1 == sk2 && sv1 :<: sv2
          case (TupleType(ss1), TupleType(ss2)) =>
            ss1.length == ss2.length && (ss1 zip ss2).forall(_ :<: _)
          case (FunType(ss1, s1), FunType(ss2, s2)) =>
            ss1.length == ss2.length && (ss2 zip ss1).forall(_ :<: _) && s1 :<: s2
          case (NullType, NullableType(_)) => true
          case (t1, NullableType(t2)) => t1 :<: t2
          case _ => false

  extension (expr: Expr)
    def sort(using ctx: Map[String, Type]): Type = ???