package flat.checker.flan

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

