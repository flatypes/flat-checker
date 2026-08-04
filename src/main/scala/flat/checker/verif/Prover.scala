package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.*
import flat.checker.domain.Prettifier.pp
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.checker.verif.Type.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Prover extends LazyLogging:
  def prove(goal: Goal): Boolean =
    goal.conclusion match
      // refinement type checking
      case StringInLang(e, r) =>
        val inferer = Inferer(goal)
        inferer.infer(e) match
          case TStr(r1) =>
            logger.debug("Inferred: {} ∈ {}", e.show, r1.pp)
            RESub.check(r1, r)
          case _ => ???
      case _ =>
        val solver = SMTSolver(using goal.sorts)
        goal.premises.foreach(solver.add)
        if solver.prove(goal.conclusion) then
          true
        else
          val lemmas = synthesizeLemmas(goal)
          if lemmas.nonEmpty then
            logger.debug(s"Lemmas: ${lemmas.map(_.show).mkString(", ")}")
            lemmas.foreach(solver.add)
            solver.prove(goal.conclusion)
          else
            false

  private def synthesizeLemmas(goal: Goal): List[Expr] =
    val inferer = Inferer(goal)
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.collect:
      case e@SeqLength(_) =>
        inferer.infer(e) match
          case TNat(set) =>
            lemmas += inNatSet(e, set)
          case _ => ()
      case e@SeqSelect(_, Const(i: Int)) =>
        inferer.infer(e) match
          case TChar(set) =>
            lemmas += inCharSet(e, set)
          case _ => ()
      case e@SeqStartsWith(_, _) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqEndsWith(_, _) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqContains(_, _) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@SeqIndexOf(_, Const(_: String), Const(0)) =>
        inferer.infer(e) match
          case TIndex(set) =>
            lemmas += inIndexSet(e, set)
          case _ => ()

      // Algebraic properties for count
      case e@SeqCount(SeqSlice(es, ei, ej), Const(s: String)) if s.length == 1 && goal.have(Lt(ei, ej)) =>
        val c = s.head
        // If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)`
        goal.premises.collectFirst:
          case Eq(SeqSelect(`es`, ek), Const(c: Char)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, Add(SeqCount(SeqSlice(es, Add(ei, 1), ej), Const(s)), 1))
          case Ne(SeqSelect(`es`, ek), Const(c: Char)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, SeqCount(SeqSlice(es, Add(ei, 1), ej), Const(s)))

    lemmas.toList

  private def inNatSet(elem: Expr, set: CountingRE): Expr =
    if set.isFinite then
      mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, Const(n)))
    else
      Le(set.min, elem)

  private def inCharSet(elem: Expr, set: CharSet): Expr =
    if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, Const(c)))
    else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, Const(c)))

  private def inBoolSet(elem: Expr, set: BoolSet): List[Expr] = set match
    case BoolSet.True => List(elem)
    case BoolSet.False => List(Not(elem))
    case _ => Nil

  private def inIndexSet(elem: Expr, set: IndexSet): Expr =
    val cases = ListBuffer.empty[Expr]
    if set.neg.nonEmpty then
      for i <- set.neg.toList.sorted do
        cases += Eq(elem, Const(i))
    if set.pos.isFinite then
      for n <- set.pos.toFinSet.toList.sorted do
        cases += Eq(elem, Const(n))
    else
      cases += Le(set.pos.min, elem)
    mkOr(cases.toList)
