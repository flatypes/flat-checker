package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.given
import flat.checker.flan.*
import flat.checker.flan.SortOps.sort
import flat.checker.flan.Subst.subst
import flat.checker.flan.tpd.*

object Simplifier extends LazyLogging:
  extension (expr: Expr)
    private def isConst: Boolean = expr match
      case Const(_) => true
      case SeqLit(es) => es.forall(_.isConst)
      case SetLit(es) => es.forall(_.isConst) && es.distinct.size == es.size
      case MapLit(eks, _) => eks.forall(_.isConst) && eks.distinct.size == eks.size
      case _ => false

    def simplify(using sorts: Map[String, Sort]): Expr = expr match
      case Const(_) | Var(_) | MethodRef(_) => expr

      // Functional
      case Apply(e, es) =>
        (e.simplify, es.map(_.simplify)) match
          case (f@Lambda(_, e), es) => e.subst(f.paramNames, es)
          case (e, es) => Apply(e, es)(expr.range)
      case Lambda(ps, e) => Lambda(ps, e.simplify)(expr.range)

      // Basic
      case Eq(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(v1), Const(v2)) => Const(v1 == v2)(expr.range)
          case (e1, e2) => if e1 == e2 then Const(true) else Eq(e1, e2)(expr.range)
      case Ne(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(v1), Const(v2)) => Const(v1 != v2)(expr.range)
          case (e1, e2) => Ne(e1, e2)(expr.range)
      case Ite(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (Const(true), e1, _) => e1
          case (Const(false), _, e2) => e2
          case (e, e1, e2) => Ite(e, e1, e2)(expr.range)

      // Boolean
      case And(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(b1: Boolean), Const(b2: Boolean)) => Const(b1 && b2)(expr.range)
          case (Const(true), e) => e
          case (e, Const(true)) => e
          case (Const(false), _) | (_, Const(false)) => Const(false)(expr.range)
          case (e1, e2) => And(e1, e2)(expr.range)
      case Or(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(b1: Boolean), Const(b2: Boolean)) => Const(b1 || b2)(expr.range)
          case (Const(false), e) => e
          case (e, Const(false)) => e
          case (Const(true), _) | (_, Const(true)) => Const(true)(expr.range)
          case (e1, e2) => Or(e1, e2)(expr.range)
      case Not(e) =>
        e.simplify match
          case Const(b: Boolean) => Const(!b)(expr.range)
          case Not(e) => e
          case And(e1, e2) => Or(Not(e1)(expr.range).simplify, Not(e2)(expr.range).simplify)(expr.range)
          case Or(e1, e2) => And(Not(e1)(expr.range).simplify, Not(e2)(expr.range).simplify)(expr.range)
          case Eq(e1, e2) => Ne(e1, e2)(expr.range)
          case Ne(e1, e2) => Eq(e1, e2)(expr.range)
          case Le(e1, e2) => Lt(e2, e1)(expr.range)
          case Lt(e1, e2) => Le(e2, e1)(expr.range)
          case e => Not(e)(expr.range)
      case Implies(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(b1: Boolean), Const(b2: Boolean)) => Const(!b1 || b2)(expr.range)
          case (Const(true), e) => e
          case (Const(false), _) | (_, Const(true)) => Const(true)(expr.range)
          case (e1, e2) => Implies(e1, e2)(expr.range)

      // Int arithmetic
      case Negate(e) =>
        e.simplify match
          case Const(i: Int) => Const(-i)(expr.range)
          case Negate(e) => e
          case e => Negate(e)(expr.range)
      case Add(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) => Const(i1 + i2)(expr.range)
          case (Const(0), e) => e
          case (e, Const(0)) => e
          case (e1, e2) => Add(e1, e2)(expr.range)
      case Sub(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) => Const(i1 - i2)(expr.range)
          case (e, Const(0)) => e
          case (e1, e2) => Sub(e1, e2)(expr.range)
      case Mul(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) => Const(i1 * i2)(expr.range)
          case (Const(1), e) => e
          case (e, Const(1)) => e
          case (Const(0), _) | (_, Const(0)) => Const(0)(expr.range)
          case (e1, e2) => Mul(e1, e2)(expr.range)
      case Div(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) if i2 != 0 => Const(i1 / i2)(expr.range)
          case (e, Const(1)) => e
          case (e1, e2) => Div(e1, e2)(expr.range)
      case Mod(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) if i2 != 0 => Const(i1 % i2)(expr.range)
          case (e, Const(1)) => Const(0)(expr.range)
          case (e1, e2) => Mod(e1, e2)(expr.range)

      // Int relational
      case Le(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) => Const(i1 <= i2)(expr.range)
          case (e1, e2) => Le(e1, e2)(expr.range)
      case Lt(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (Const(i1: Int), Const(i2: Int)) => Const(i1 < i2)(expr.range)
          case (SeqIndexOf(e1, _, _), SeqLength(e2)) if e1 == e2 => Const(true)(expr.range) // s.indexOf(t) < |s|
          case (e1, e2) => Lt(e1, e2)(expr.range)

      // Char
      case CharIn(e, a) =>
        e.simplify match
          case Const(c: Char) => Const(a.contains(c))(expr.range)
          case e => CharIn(e, a)(expr.range)
      case CharToInt(e) =>
        e.simplify match
          case Const(c: Char) => Const(c.toInt)(expr.range)
          case e => CharToInt(e)(expr.range)
      case CharFromInt(e) =>
        e.simplify match
          case Const(i: Int) => Const(i.toChar)(expr.range)
          case e => CharFromInt(e)(expr.range)
      case CharToString(e) =>
        e.simplify match
          case Const(c: Char) => Const(c.toString)(expr.range)
          case e => CharToString(e)(expr.range)

      // Seq
      case s@SeqLit(es) =>
        val es1 = es.map(_.simplify)
        if s.elemSort == CharSort && s.isConst then
          Const(es1.collect { case Const(c: Char) => c }.mkString)(expr.range)
        else
          SeqLit(es1)(s.elemSort, s.range)
      case SeqLength(e) =>
        e.simplify match
          case SeqLit(es) => Const(es.length)(expr.range)
          case Const(s: String) => Const(s.length)(expr.range)
          case e => SeqLength(e)(expr.range)
      case SeqSelect(e, ei) =>
        (e.simplify, ei.simplify) match
          case (SeqLit(es), Const(i: Int)) => es(i)
          case (Const(s: String), Const(i: Int)) => Const(s.charAt(i))(expr.range)
          case (e, ei) => SeqSelect(e, ei)(expr.range)
      case SeqUpdate(e, ei, ex) =>
        (e.simplify, ei.simplify, ex.simplify) match
          case (s@SeqLit(es), Const(i: Int), ev) => SeqLit(es.updated(i, ev))(s.elemSort, s.range)
          case (Const(s: String), Const(i: Int), Const(c: Char)) => Const(s.updated(i, c))(expr.range)
          case (e, ei, ev) => SeqUpdate(e, ei, ev)(expr.range)
      case SeqSlice(e, ei, NoExpr) =>
        (e.simplify, ei.simplify) match
          case (s@SeqLit(es), Const(i: Int)) => SeqLit(es.drop(i))(s.elemSort, s.range)
          case (Const(s: String), Const(i: Int)) => Const(s.substring(i))(expr.range)
          case (e, ei) => SeqSlice(e, ei, NoExpr)(expr.range)
      case SeqSlice(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (s@SeqLit(es), Const(i: Int), Const(j: Int)) => SeqLit(es.slice(i, j))(s.elemSort, s.range)
          case (Const(s: String), Const(i: Int), Const(j: Int)) => Const(s.substring(i, j))(expr.range)
          case (e, ei, ej) if ei == ej =>
            e.sort match
              case `stringSort` => Const("")(expr.range)
              case SeqSort(s) => SeqLit(Nil)(s, expr.range)
              case _ => assert(false)
          case (e, ei, ej) => SeqSlice(e, ei, ej)(expr.range)
      case SeqConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SeqLit(es1), SeqLit(es2)) => SeqLit(es1 ++ es2)(s1.elemSort, expr.range)
          case (Const(s1: String), Const(s2: String)) => Const(s1 + s2)(expr.range)
          case (e1, e2) => SeqConcat(e1, e2)(expr.range)
      case SeqReverse(e) =>
        e.simplify match
          case s@SeqLit(es) => SeqLit(es.reverse)(s.elemSort, s.range)
          case Const(s: String) => Const(s.reverse)(expr.range)
          case e => SeqReverse(e)(expr.range)
      case SeqIndexOf(e, et, ei) =>
        (e.simplify, et.simplify, ei.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2), Const(i: Int)) if s1.isConst && s2.isConst =>
            Const(es1.indexOfSlice(es2, i))(expr.range)
          case (Const(s: String), Const(t: String), Const(i: Int)) => Const(s.indexOf(t, i))(expr.range)
          case (e, et, ei) => SeqIndexOf(e, et, ei)(expr.range)
      case SeqContains(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst =>
            Const(es1.containsSlice(es2))(expr.range)
          case (Const(s: String), Const(t: String)) => Const(s.contains(t))(expr.range)
          case (e, et) => SeqContains(e, et)(expr.range)
      case SeqStartsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => Const(es1.startsWith(es2))(expr.range)
          case (Const(s: String), Const(t: String)) => Const(s.startsWith(t))(expr.range)
          case (e, et) => SeqStartsWith(e, et)(expr.range)
      case SeqEndsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => Const(es1.endsWith(es2))(expr.range)
          case (Const(s: String), Const(t: String)) => Const(s.endsWith(t))(expr.range)
          case (e, et) => SeqEndsWith(e, et)(expr.range)
      case SeqCount(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => Const(es1.countSlice(es2))(expr.range)
          case (Const(s: String), Const(t: String)) => Const(s.countSlice(t))(expr.range)
          case (e, et) => SeqCount(e, et)(expr.range)
      case SeqForall(e, ep) => SeqForall(e.simplify, ep.simplify)(expr.range)

      // String-specific
      case StrReplace(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (Const(s: String), Const(s1: String), Const(s2: String)) => Const(s.replace(s1, s2))(expr.range)
          case (e, e1, e2) => StrReplace(e, e1, e2)(expr.range)
      case StringSplit(e, ex, None) =>
        (e.simplify, ex.simplify) match
          case (Const(s: String), Const(t: String)) => SeqLit(s.split(t).toList.map(Const(_)))(stringSort, expr.range)
          case (e, et) => StringSplit(e, et)(expr.range)
      case StringSplit(e, ex, Some(em)) =>
        (e.simplify, ex.simplify, em.simplify) match
          case (Const(s: String), Const(t: String), Const(m: Int)) =>
            SeqLit(s.split(t, m).toList.map(Const(_)))(stringSort, expr.range)
          case (e, et, em) => StringSplit(e, et, Some(em))(expr.range)
      case StringTrim(e) =>
        e.simplify match
          case Const(s: String) => Const(s.trim)(expr.range)
          case e => StringTrim(e)(expr.range)
      case StringToLower(e) =>
        e.simplify match
          case Const(s: String) => Const(s.toLowerCase)(expr.range)
          case e => StringToLower(e)(expr.range)
      case StringToUpper(e) =>
        e.simplify match
          case Const(s: String) => Const(s.toUpperCase)(expr.range)
          case e => StringToUpper(e)(expr.range)
      case StringToInt(e) =>
        e.simplify match
          case Const(s: String) => Const(s.toInt)(expr.range)
          case e => StringToInt(e)(expr.range)
      case StrFromInt(e, fmt) =>
        e.simplify match
          case Const(i: Int) => Const(i.toString)(expr.range)
          case e => StrFromInt(e, fmt)(expr.range)
      case StrIsAscii(e) =>
        e.simplify match
          case Const(s: String) => Const(s.forall(_.toInt <= 0x7F))(expr.range)
          case e => StrIsAscii(e)(expr.range)

      // Set
      case s@SetLit(es) =>
        SetLit(es.map(_.simplify))(s.elemSort, s.range)
      case SetSize(e) =>
        e.simplify match
          case s@SetLit(es) => Const(es.size)(expr.range)
          case e => SetSize(e)(expr.range)
      case SetContains(e, ex) =>
        (e.simplify, ex.simplify) match
          case (s@SetLit(es), ex) if s.isConst && ex.isConst => Const(es.contains(ex))(expr.range)
          case (e, ex) => SetContains(e, ex)(expr.range)
      case Subset(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetLit(es1), s2@SetLit(es2)) if s1.isConst && s2.isConst =>
            Const(es1.toSet.subsetOf(es2.toSet))(expr.range)
          case (e1, e2) => Subset(e1, e2)(expr.range)
      case SetUnion(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetLit(es1), s2@SetLit(es2)) if s1.isConst && s2.isConst =>
            SetLit((es1.toSet | es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetUnion(e1, e2)(expr.range)
      case SetInter(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetLit(es1), s2@SetLit(es2)) if s1.isConst && s2.isConst =>
            SetLit((es1.toSet & es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetInter(e1, e2)(expr.range)
      case SetDiff(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetLit(es1), s2@SetLit(es2)) if s1.isConst && s2.isConst =>
            SetLit((es1.toSet -- es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetDiff(e1, e2)(expr.range)
      case SetForall(e, ep) => SetForall(e.simplify, ep.simplify)(expr.range)

      // Map
      case m@MapLit(eks, evs) => MapLit(eks.map(_.simplify), evs.map(_.simplify))(m.keySort, m.valSort, m.range)
      case MapKeys(e) =>
        e.simplify match
          case m@MapLit(eks, _) if m.isConst => SetLit(eks)(m.keySort, expr.range)
          case e => MapKeys(e)(expr.range)
      case MapValues(e) =>
        e.simplify match
          case m@MapLit(_, evs) if m.isConst => SetLit(evs)(m.valSort, expr.range)
          case e => MapValues(e)(expr.range)
      case MapItems(e) =>
        e.simplify match
          case m@MapLit(eks, evs) if m.isConst =>
            SetLit(eks.zip(evs).map(mkTuple(_, _)))(mkTupleSort(m.keySort, m.valSort), expr.range)
          case e => MapItems(e)(expr.range)
      case MapSize(e) =>
        e.simplify match
          case MapLit(eks, _) => Const(eks.size)(expr.range)
          case e => MapSize(e)(expr.range)
      case MapContains(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapLit(eks, _), ek) if m.isConst && ek.isConst => Const(eks.contains(ek))(expr.range)
          case (e, ek) => MapContains(e, ek)(expr.range)
      case MapSelect(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapLit(eks, evs), ek) if m.isConst && ek.isConst => evs(eks.lastIndexOf(ek))
          case (map, ek) => MapSelect(map, ek)(expr.range)
      case MapUpdate(e, ek, ev) =>
        (e.simplify, ek.simplify, ev.simplify) match
          case (m@MapLit(eks, evs), ek, ev) if m.isConst && ek.isConst =>
            val i = eks.lastIndexOf(ek)
            if i >= 0 then MapLit(eks, evs.updated(i, ev))(m.keySort, m.valSort, expr.range)
            else MapLit(eks :+ ek, evs :+ ev)(m.keySort, m.valSort, expr.range)
          case (map, ek, ev) => MapUpdate(map, ek, ev)(expr.range)

      // Tuple
      case TupleExpr(es) => TupleExpr(es.map(_.simplify))(expr.range)
      case TupleSelect(i, es) =>
        es.simplify match
          case TupleExpr(es) => es(i)
          case es => TupleSelect(i, es)(expr.range)

      // Domain
      case StringInLang(e, r) =>
        e.simplify match
          case Const(s: String) => Const(r.contains(s.toList))
          case e => StringInLang(e, r)

      case _ => throw NotImplementedError(s"Simplifier.simplify(${expr.getClass.getSimpleName})")

    def toCNF: List[Expr] = expr match
      case And(e1, e2) => e1.toCNF ++ e2.toCNF
      case _ => List(expr)

  extension (s: String)
    private def countSlice(t: String): Int = s.sliding(t.length).count(_ == t)

  extension [T](s: List[T])
    private def countSlice(t: List[T]): Int = s.sliding(t.length).count(_ == t)