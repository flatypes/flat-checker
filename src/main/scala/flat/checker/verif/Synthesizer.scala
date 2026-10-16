package flat.checker.verif

import flat.checker.flan.tpd.*

import scala.collection.mutable.ListBuffer

object Synthesizer:
  def synthesizeLemmas(goal: Goal): List[Expr] =
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.foreach: expr =>
      Inferer.infer(expr)(using goal) match
        case Domain.Top => false
        case dom =>
          lemmas ++= dom.membership(expr)
          true

    // Algebraic properties for count
//      case e@StrCount(Substr(es, ei, ej), StrConst(s)) if s.length == 1 && goal.have(Lt(ei, ej)) =>
//        val c = s.head
//         If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)`
//        goal.premises.collectFirst:
//          case Eq(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
//            lemmas += Eq(e, Add(StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)), IntConst(1)))
//          case Ne(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
//            lemmas += Eq(e, StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)))

    lemmas.toList
