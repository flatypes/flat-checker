package flat.checker.flan

import flat.checker.flan.tpd.*

object TypeOps:
  extension (typ: Type)
    def isSort: Boolean = typ match
      case IntType | BoolType | StrType | NullType | NoType => true
      case _: RefinedType => false
      case ListType(t) => t.isSort
      case SetType(t) => t.isSort
      case MapType(k, v) => k.isSort && v.isSort
      case TupleType(ts) => ts.forall(_.isSort)
      case FunType(ts, r) => ts.forall(_.isSort) && r.isSort
      case OptType(t) => t.isSort

    def erase: Type = typ match
      case IntType | BoolType | StrType | NullType | NoType => typ
      case ListType(t) => ListType(t.erase)
      case SetType(t) => SetType(t.erase)
      case MapType(k, v) => MapType(k.erase, v.erase)
      case RefinedType(_, t, _) => t.erase
      case TupleType(ts) => TupleType(ts.map(_.erase))
      case FunType(ps, r) => FunType(ps.map(_.erase), r.erase)
      case OptType(t) => OptType(t.erase)
