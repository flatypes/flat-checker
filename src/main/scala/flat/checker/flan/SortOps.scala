package flat.checker.flan

import flat.checker.flan.tpd.*

object SortOps:
  extension (left: Sort)
    infix def :<:(right: Sort): Boolean =
      if left == right then true
      else
        (left, right) match
          case (NoSort, _) | (_, NoSort) => true
          case (SeqSort(s1), SeqSort(s2)) => s1 :<: s2
          case (SetSort(s1), SetSort(s2)) => s1 :<: s2
          case (MapSort(sk1, sv1), MapSort(sk2, sv2)) => sk1 == sk2 && sv1 :<: sv2
          case (TupleSort(ss1), TupleSort(ss2)) =>
            ss1.length == ss2.length && (ss1 zip ss2).forall(_ :<: _)
          case (FunSort(ss1, s1), FunSort(ss2, s2)) =>
            ss1.length == ss2.length && (ss2 zip ss1).forall(_ :<: _) && s1 :<: s2
          case (s, UnionSort(s1, s2)) => s :<: s1 || s :<: s2
          case _ => false

    infix def lub(right: Sort): Sort =
      (left, right) match
        case (NoSort, s) => s
        case (s, NoSort) => s
        case (SeqSort(e1), SeqSort(e2)) => SeqSort(e1 lub e2)
        case (SetSort(e1), SetSort(e2)) => SetSort(e1 lub e2)
        case (MapSort(k1, v1), MapSort(k2, v2)) if k1 == k2 => MapSort(k1, v1 lub v2)
        case (TupleSort(es1), TupleSort(es2)) if es1.length == es2.length =>
          TupleSort(es1.zip(es2).map { case (e1, e2) => e1 lub e2 })
        case _ =>
          if left :<: right then right
          else if right :<: left then left
          else UnionSort(left, right)

  extension (expr: Expr)
    def sort(using ctx: Map[String, Sort]): Sort =
      expr match
        // Constants and variables
        case Const(null) => NullSort
        case Const(_: Int) => IntSort
        case Const(_: Boolean) => BoolSort
        case Const(_: Char) => CharSort
        case Const(_: String) => stringSort
        case Var(x) => ctx(x)
        case MethodRef(f) => throw IllegalArgumentException(s"Cannot get sort of method reference: $f")
        // Functional
        case Apply(e, _) => e.sort.asInstanceOf[FunSort].returnSort
        case Lambda(ps, e) => FunSort(ps.map(_.typ.sort), e.sort)
        // Universal
        case _: Eq | Ne => BoolSort
        case Ite(_, e1, e2) => e1.sort lub e2.sort
        // Bool
        case _: And | Or | Not | Implies => BoolSort
        // Int
        case _: Negate | Add | Sub | Mul => IntSort
        case _: Le | Lt => BoolSort
        case _: BitAnd | BitOr | BitXor | BitShL | BitShR => IntSort
        // Char
        case _: CharToInt => IntSort
        case _: CharFromInt => CharSort
        case _: CharToString => stringSort
        // Seq
        case s: SeqLit => SeqSort(s.elemSort)
        case _: SeqLength | SeqIndexOf | SeqCount => IntSort
        case _: SeqContains | SeqStartsWith | SeqEndsWith | SeqForall => BoolSort
        case SeqSelect(e, _) => e.sort.asInstanceOf[SeqSort].elemSort
        case SeqUpdate(e, _, _) => e.sort
        case SeqSlice(e, _, _) => e.sort
        case SeqConcat(e, _) => e.sort
        case SeqReverse(e) => e.sort
        // String
        case _: StringSplit => SeqSort(stringSort)
        case _: StringTrim | StringToLower | StringToUpper | StringFromInt => stringSort
        case _: StringToInt => IntSort
        // Set
        case s: SetLit => SetSort(s.elemSort)
        case _: SetSize => IntSort
        case _: SetContains | Subset | SetForall => BoolSort
        case SetUnion(e, _) => e.sort
        case SetInter(e, _) => e.sort
        case SetDiff(e, _) => e.sort
        // Map
        case m: MapLit => MapSort(m.keySort, m.valSort)
        case MapKeys(e) => e.sort.asInstanceOf[MapSort].keySort
        case MapValues(e) => SetSort(e.sort.asInstanceOf[MapSort].valueSort)
        case MapItems(e) =>
          val mapSort = e.sort.asInstanceOf[MapSort]
          SetSort(TupleSort(List(mapSort.keySort, mapSort.valueSort)))
        case _: MapSize => IntSort
        case _: MapContains => BoolSort
        case MapSelect(e, _) => e.sort.asInstanceOf[MapSort].valueSort
        case MapUpdate(e, _, _) => e.sort
        // Tuple
        case TupleExpr(es) => TupleSort(es.map(_.sort))
        case TupleSelect(i, e) => e.sort.asInstanceOf[TupleSort].elemSorts(i)
        // Others
        case NoExpr => NoSort
