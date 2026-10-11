package flat.checker.verif

import flat.checker.domain.*
import flat.checker.domain.StrREOps.{getLang, isFiniteLang}
import flat.checker.flan.tpd.*
import flat.checker.verif.Type.*

import scala.collection.mutable.ListBuffer

object Synthesizer:
  def synthesizeLemmas(goal: Goal): List[Expr] =
    val inferer = Inferer(goal)
    val lemmas = ListBuffer.empty[Expr]
    goal.conclusion.collect:
      case e: StrLength =>
        inferer.infer(e) match
          case TNat(set) =>
            lemmas += inNatSet(e, set)
          case _ =>
      case e: CharAt =>
        inferer.infer(e) match
          case TStr(RegEx.Lit(set)) =>
            lemmas += inCharSet(e, set)
          case _ => ()
      case e: Substr =>
        inferer.infer(e) match
          case TStr(r) =>
            lemmas += inRegEx(e, r)
          case _ => ()
      case e: (StrStartsWith | StrEndsWith | StrContains | StrIs) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@StrIndexOf(es, _, IntConst(0)) =>
        inferer.infer(e) match
          case TIndex(set) =>
            lemmas += inIndexSet(e, set)
          case _ => ()
      case e: StrToInt =>
        inferer.infer(e) match
          case TNum(range) =>
            lemmas += inNatRange(e, range)
          case _ => ()

      case e@Eq(_, StrConst(_)) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()
      case e@Ne(_, StrConst(_)) =>
        inferer.infer(e) match
          case TBool(set) =>
            lemmas ++= inBoolSet(e, set)
          case _ => ()

      // Algebraic properties for count
      case e@StrCount(Substr(es, ei, ej), StrConst(s)) if s.length == 1 && goal.have(Lt(ei, ej)) =>
        val c = s.head
        // If `i < j`, then `s[i:j].count(c) == s[i+1:j].count(c) + (if s[i] == c then 1 else 0)`
        goal.premises.collectFirst:
          case Eq(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, Add(StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)), IntConst(1)))
          case Ne(CharAt(`es`, ek), CharConst(c)) if goal.have(Eq(ek, ei)) =>
            lemmas += Eq(e, StrCount(Substr(es, Add(ei, IntConst(1)), ej), StrConst(s)))

    lemmas.toList

  private def inNatSet(elem: Expr, set: CountingRE): Expr =
    if set.isFinite then
      mkOr(for n <- set.toFinSet.toList.sorted yield Eq(elem, IntConst(n)))
    else
      Le(IntConst(set.min), elem)

  private def inCharSet(elem: Expr, set: CharSet): Expr =
    if set.pos then mkOr(for c <- set.chars.toList.sorted yield Eq(elem, CharConst(c)))
    else mkAnd(for c <- set.chars.toList.sorted yield Ne(elem, CharConst(c)))

  private def inRegEx(elem: Expr, r: StrRE): Expr = r match
    case RegEx.Lit(a) => inCharSet(elem, a)
    case _ =>
      if r.isFiniteLang then mkOr(for s <- r.getLang.toList.sorted yield Eq(elem, StrConst(s)))
      else StrIn(elem, r)

  private def inBoolSet(elem: Expr, set: BoolSet): List[Expr] = set match
    case BoolSet.True => List(elem)
    case BoolSet.False => List(Not(elem))
    case _ => Nil

  private def inIndexSet(elem: Expr, set: IndexSet): Expr =
    val cases = ListBuffer.empty[Expr]
    if set.neg.nonEmpty then
      for i <- set.neg.toList.sorted do
        cases += Eq(elem, IntConst(i))
    if set.pos.isFinite then
      for n <- set.pos.toFinSet.toList.sorted do
        cases += Eq(elem, IntConst(n))
    else
      cases += Le(IntConst(set.pos.min), elem)
    mkOr(cases.toList)

  private def inNatRange(elem: Expr, range: NatRange): Expr = range.max match
    case Some(max) => And(Le(IntConst(range.min), elem), Le(elem, IntConst(max)))
    case None => Le(IntConst(range.min), elem)

