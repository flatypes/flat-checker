package flat.checker.flan

import flat.checker.flan.tpd.*

object sorting:
  extension (typ: Type)
    def isSort: Boolean = typ match
      case IntType | BoolType | CharType | StrType | NullType | NoType => true
      case ListType(t) => t.isSort
      case SetType(t) => t.isSort
      case MapType(tk, tv) => tk.isSort && tv.isSort
      case TupleType(ts) => ts.forall(_.isSort)
      case FunType(ts, t) => ts.forall(_.isSort) && t.isSort
      case OptType(t) => t.isSort
      case _: ReftType => false

    def toSort: Type =
      if typ.isSort then typ
      else typ match
        case ListType(t) => ListType(t.toSort)
        case SetType(t) => SetType(t.toSort)
        case MapType(tk, tv) => MapType(tk.toSort, tv.toSort)
        case TupleType(ts) => TupleType(ts.map(_.toSort))
        case FunType(ts, t) => FunType(ts.map(_.toSort), t.toSort)
        case OptType(t) => OptType(t.toSort)
        case ReftType(_, t, _) => t.toSort
        case _ => assert(false)

    def subsortOf(right: Type): Boolean =
      require(typ.isSort && right.isSort)
      if typ == right || typ == NoType || right == NoType then true
      else (typ, right) match
        case (ListType(t1), ListType(t2)) => t1.subsortOf(t2)
        case (SetType(t1), SetType(t2)) => t1.subsortOf(t2)
        case (MapType(tk1, tv1), MapType(tk2, tv2)) => tk1.subsortOf(tk2) && tv1.subsortOf(tv2)
        case (TupleType(ts1), TupleType(ts2)) =>
          ts1.length == ts2.length && (ts1 zip ts2).forall(_.subsortOf(_))
        case (FunType(ts1, t1), FunType(ts2, t2)) =>
          ts1.length == ts2.length && (ts2 zip ts1).forall(_.subsortOf(_)) && t1.subsortOf(t2)
        case (OptType(t1), OptType(t2)) => t1.subsortOf(t2)
        case (NullType, OptType(_)) => true
        case (t1, OptType(t2)) => t1.subsortOf(t2)
        case _ => false