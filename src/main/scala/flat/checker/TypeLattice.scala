package flat.checker

import flat.checker.ast.*

given TypeLattice: Lattice[Type]:
  def top: Type = AnyType

  def bot: Type = NoType

  def subElement(t1: Type, t2: Type): Boolean = (t1, t2) match
    case (_, AnyType) | (NoType, _) => true
    case (`unitType`, `unitType`) => true
    case (IntervalType(r1), IntervalType(r2)) => r1 :<: r2
    case (TernaryType(b1), TernaryType(b2)) => b1 :<: b2
    case (LangType(r1), LangType(r2)) => r1 :<: r2
    case (TupleType(ts1), TupleType(ts2)) if ts1.length == ts2.length =>
      (ts1 zip ts2).forall(_ :<: _)
    case (ArrayType(t1), ArrayType(t2)) => t1 == t2
    case (FunType(ts1, t1), FunType(ts2, t2)) if ts1.length == ts2.length =>
      (ts2 zip ts1).forall(_ :<: _) && (t1 :<: t2)
    case (HintType(h), _) => h.toType :<: t2
    case (_, HintType(h)) => t1 :<: h.toType
    case _ => false

  def join(t1: Type, t2: Type): Type = (t1, t2) match
    case (NoType, t) => t
    case (t, NoType) => t
    case (`unitType`, `unitType`) => unitType
    case (IntervalType(r1), IntervalType(r2)) => IntervalType(r1 | r2)
    case (TernaryType(b1), TernaryType(b2)) => TernaryType(b1 | b2)
    case (LangType(r1), LangType(r2)) => LangType(r1 | r2)
    case (TupleType(ts1), TupleType(ts2)) if ts1.length == ts2.length =>
      TupleType(for (x, y) <- ts1 zip ts2 yield x | y)
    case (ArrayType(t1), ArrayType(t2)) => ArrayType(t1 | t2)
    case (FunType(ts1, t1), FunType(ts2, t2)) if ts1.length == ts2.length =>
      FunType(for (x, y) <- ts1 zip ts2 yield x | y, t1 | t2)
    case (HintType(h1), HintType(h2)) if h1 == h2 => t1
    case (HintType(h), _) => join(h.toType, t2)
    case (_, HintType(h)) => join(t1, h.toType)
    case _ => AnyType
