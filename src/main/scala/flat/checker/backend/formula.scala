//package flat.checker.backend
//
//import flat.checker.Location
//import flat.checker.backend.core.{Expr, Type}
//
//object formula:
//  sealed trait Predicate extends Product:
//    def subst(mappings: Map[String, Expr]): Predicate
//
//    def collectVars: Set[String]
//
//  final case class HasType(value: Expr, typ: Type, loc: Location) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate = HasType(value.subst(mappings), typ, loc)
//
//    def collectVars: Set[String] = value.collectVars
//
//  final case class IsTrue(cond: Expr, loc: Location) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate = IsTrue(cond.subst(mappings), loc)
//
//    def collectVars: Set[String] = cond.collectVars
//
//  given Conversion[Expr, Predicate] = e => IsTrue(e, e.loc)
//
//  final case class EntryInv(cond: Expr, loc: Location) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate = EntryInv(cond.subst(mappings), loc)
//
//    def collectVars: Set[String] = cond.collectVars
//
//  final case class ExitInv(cond: Expr, loc: Location) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate = ExitInv(cond.subst(mappings), loc)
//
//    def collectVars: Set[String] = cond.collectVars
//
//  final case class Conj(predicates: List[Predicate]) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate = Conj(predicates.map(_.subst(mappings)))
//
//    def collectVars: Set[String] = predicates.flatMap(_.collectVars).toSet
//
//  object Conj:
//    def apply(conjuncts: Predicate*): Predicate = Conj(conjuncts.toList)
//
//  val mkTrue: Predicate = Conj(Nil)
//
//  final case class Imply(premises: List[Expr], conclusion: Predicate) extends Predicate:
//    def subst(mappings: Map[String, Expr]): Predicate =
//      Imply(premises.map(_.subst(mappings)), conclusion.subst(mappings))
//
//    def collectVars: Set[String] = premises.flatMap(_.collectVars).toSet | conclusion.collectVars
//
//  object Imply:
//    def apply(premise: Expr, conclusion: Predicate): Imply = Imply(List(premise), conclusion)
