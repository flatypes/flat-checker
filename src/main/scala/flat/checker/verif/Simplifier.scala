package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.given
import flat.checker.flan.*
import flat.checker.flan.Subst.subst
import flat.checker.flan.tpd.*

object Simplifier extends LazyLogging:
  extension (expr: Expr)
    private def isConst: Boolean = expr match
      case IntLit(_) | BoolLit(_) | CharLit(_) | StrLit(_) | NullLit() => true
      case SeqLit(es) => es.forall(_.isConst)
      case SetLit(es) => es.forall(_.isConst) && es.distinct.size == es.size
      case MapLit(eks, _) => eks.forall(_.isConst) && eks.distinct.size == eks.size
      case _ => false

    def simplify: Expr = expr match
      case IntLit(_) | BoolLit(_) | CharLit(_) | StrLit(_) | NullLit() => expr
      case Var(_) | MethodRef(_) => expr

      // Functional
      case Apply(e, es) =>
        (e.simplify, es.map(_.simplify)) match
          case (f@Lambda(_, e), es) => e.subst(f.paramNames, es)
          case (e, es) => Apply(e, es)(expr.range)
      case Lambda(ps, e) => Lambda(ps, e.simplify)(expr.range)

      // Basic
      case Eq(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e1, e2) if e1 == e2 => BoolLit(true)(expr.range)
          case (e1, e2) => Eq(e1, e2)(expr.range)
      case Ne(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e1, e2) => Ne(e1, e2)(expr.range)
      case Ite(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (BoolLit(true), e1, _) => e1
          case (BoolLit(false), _, e2) => e2
          case (e, e1, e2) => Ite(e, e1, e2)(expr.range)

      // Boolean
      case And(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolLit(b1), BoolLit(b2)) => BoolLit(b1 && b2)(expr.range)
          case (BoolLit(true), e) => e
          case (e, BoolLit(true)) => e
          case (BoolLit(false), _) | (_, BoolLit(false)) => BoolLit(false)(expr.range)
          case (e1, e2) => And(e1, e2)(expr.range)
      case Or(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolLit(b1), BoolLit(b2)) => BoolLit(b1 || b2)(expr.range)
          case (BoolLit(false), e) => e
          case (e, BoolLit(false)) => e
          case (BoolLit(true), _) | (_, BoolLit(true)) => BoolLit(true)(expr.range)
          case (e1, e2) => Or(e1, e2)(expr.range)
      case Not(e) =>
        e.simplify match
          case BoolLit(b) => BoolLit(!b)(expr.range)
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
          case (BoolLit(b1), BoolLit(b2)) => BoolLit(!b1 || b2)(expr.range)
          case (BoolLit(true), e) => e
          case (BoolLit(false), _) | (_, BoolLit(true)) => BoolLit(true)(expr.range)
          case (e1, e2) => Implies(e1, e2)(expr.range)

      // Int arithmetic
      case Negate(e) =>
        e.simplify match
          case IntLit(i) => IntLit(-i)(expr.range)
          case Negate(e) => e
          case e => Negate(e)(expr.range)
      case Add(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) => IntLit(i1 + i2)(expr.range)
          case (IntLit(0), e) => e
          case (e, IntLit(0)) => e
          case (e1, e2) => Add(e1, e2)(expr.range)
      case Sub(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) => IntLit(i1 - i2)(expr.range)
          case (e, IntLit(0)) => e
          case (e1, e2) => Sub(e1, e2)(expr.range)
      case Mul(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) => IntLit(i1 * i2)(expr.range)
          case (IntLit(1), e) => e
          case (e, IntLit(1)) => e
          case (IntLit(0), _) | (_, IntLit(0)) => IntLit(0)(expr.range)
          case (e1, e2) => Mul(e1, e2)(expr.range)
      case Div(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) if i2 != 0 => IntLit(i1 / i2)(expr.range)
          case (e, IntLit(1)) => e
          case (e1, e2) => Div(e1, e2)(expr.range)
      case Mod(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) if i2 != 0 => IntLit(i1 % i2)(expr.range)
          case (e, IntLit(1)) => IntLit(0)(expr.range)
          case (e1, e2) => Mod(e1, e2)(expr.range)

      // Int relational
      case Le(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) => BoolLit(i1 <= i2)(expr.range)
          case (e1, e2) => Le(e1, e2)(expr.range)
      case Lt(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntLit(i1), IntLit(i2)) => BoolLit(i1 < i2)(expr.range)
          case (SeqIndexOf(e1, _, _), SeqLength(e2)) if e1 == e2 => BoolLit(true)(expr.range) // s.indexOf(t) < |s|
          case (e1, e2) => Lt(e1, e2)(expr.range)

      // Char
      case CharIn(e, a) =>
        e.simplify match
          case CharLit(c) => BoolLit(a.contains(c))(expr.range)
          case e => CharIn(e, a)(expr.range)
      case CharToInt(e) =>
        e.simplify match
          case CharLit(c) => IntLit(c.toInt)(expr.range)
          case e => CharToInt(e)(expr.range)
      case CharFromInt(e) =>
        e.simplify match
          case IntLit(i) => CharLit(i.toChar)(expr.range)
          case e => CharFromInt(e)(expr.range)
      case CharToString(e) =>
        e.simplify match
          case CharLit(c) => StrLit(c.toString)(expr.range)
          case e => CharToString(e)(expr.range)

      // Seq
      case s@SeqLit(es) =>
        val es1 = es.map(_.simplify)
        if s.elemSort == CharSort && s.isConst then
          StrLit(es1.collect { case CharLit(c) => c }.mkString)(expr.range)
        else
          SeqLit(es1)(s.elemSort, s.range)
      case SeqLength(e) =>
        e.simplify match
          case SeqLit(es) => IntLit(es.length)(expr.range)
          case StrLit(s) => IntLit(s.length)(expr.range)
          case e => SeqLength(e)(expr.range)
      case ListAt(e, ei) =>
        (e.simplify, ei.simplify) match
          case (SeqLit(es), IntLit(i)) => es(i.intValue)
          case (StrLit(s), IntLit(i)) => CharLit(s.charAt(i.intValue))(expr.range)
          case (e, ei) => ListAt(e, ei)(expr.range)
      case SeqUpdate(e, ei, ex) =>
        (e.simplify, ei.simplify, ex.simplify) match
          case (s@SeqLit(es), IntLit(i), ev) => SeqLit(es.updated(i.intValue, ev))(s.elemSort, s.range)
          case (StrLit(s), IntLit(i), CharLit(c)) => StrLit(s.updated(i.intValue, c))(expr.range)
          case (e, ei, ev) => SeqUpdate(e, ei, ev)(expr.range)
      case SeqSlice(e, ei, NoExpr) =>
        (e.simplify, ei.simplify) match
          case (s@SeqLit(es), IntLit(i)) => SeqLit(es.drop(i.intValue))(s.elemSort, s.range)
          case (StrLit(s), IntLit(i)) => StrLit(s.substring(i.intValue))(expr.range)
          case (e, ei) => SeqSlice(e, ei, NoExpr)(expr.range)
      case SeqSlice(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (s@SeqLit(es), IntLit(i), IntLit(j)) =>
            SeqLit(es.slice(i.intValue, j.intValue))(s.elemSort, s.range)
          case (StrLit(s), IntLit(i), IntLit(j)) => StrLit(s.substring(i.intValue, j.intValue))
//          case (e, ei, ej) if ei == ej =>
//            e.sort match
//              case `strType` => Const("")(expr.range)
//              case SeqSort(s) => SeqLit(Nil)(s, expr.range)
//              case _ => assert(false)
          case (e, ei, ej) => SeqSlice(e, ei, ej)
      case SeqConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SeqLit(es1), SeqLit(es2)) => SeqLit(es1 ++ es2)(s1.elemSort, expr.range)
          case (StrLit(s1), StrLit(s2)) => StrLit(s1 + s2)(expr.range)
          case (e1, e2) => SeqConcat(e1, e2)(expr.range)
      case SeqReverse(e) =>
        e.simplify match
          case s@SeqLit(es) => SeqLit(es.reverse)(s.elemSort, s.range)
          case StrLit(s) => StrLit(s.reverse)(expr.range)
          case e => SeqReverse(e)(expr.range)
      case SeqIndexOf(e, et, ei) =>
        (e.simplify, et.simplify, ei.simplify) match
          case (StrLit(s), StrLit(t), IntLit(i)) => IntLit(s.indexOf(t, i.intValue))
          case (e, et, ei) => SeqIndexOf(e, et, ei)(expr.range)
      case ListContainsSlice(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst =>
            BoolLit(es1.containsSlice(es2))(expr.range)
          case (StrLit(s), StrLit(t)) => BoolLit(s.contains(t))(expr.range)
          case (e, et) => ListContainsSlice(e, et)(expr.range)
      case SeqStartsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => BoolLit(es1.startsWith(es2))(expr.range)
          case (StrLit(s), StrLit(t)) => BoolLit(s.startsWith(t))(expr.range)
          case (e, et) => SeqStartsWith(e, et)(expr.range)
      case SeqEndsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => BoolLit(es1.endsWith(es2))(expr.range)
          case (StrLit(s), StrLit(t)) => BoolLit(s.endsWith(t))(expr.range)
          case (e, et) => SeqEndsWith(e, et)(expr.range)
      case SeqCount(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@SeqLit(es1), s2@SeqLit(es2)) if s1.isConst && s2.isConst => IntLit(es1.countSlice(es2))(expr.range)
          case (StrLit(s), StrLit(t)) => IntLit(s.countSlice(t))(expr.range)
          case (e, et) => SeqCount(e, et)(expr.range)
      case SeqForall(e, ep) => SeqForall(e.simplify, ep.simplify)(expr.range)

      // String-specific
      case StrReplace(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (StrLit(s), StrLit(s1), StrLit(s2)) => StrLit(s.replace(s1, s2))(expr.range)
          case (e, e1, e2) => StrReplace(e, e1, e2)(expr.range)
      case StrSplit(e, ex, None) =>
        (e.simplify, ex.simplify) match
          case (StrLit(s), StrLit(t)) => SeqLit(s.split(t).toList.map(StrLit(_)))(strType, expr.range)
          case (e, et) => StrSplit(e, et)(expr.range)
      case StrSplit(e, ex, Some(em)) =>
        (e.simplify, ex.simplify, em.simplify) match
          case (StrLit(s), StrLit(t), IntLit(m)) =>
            SeqLit(s.split(t, m.intValue).toList.map(StrLit(_)))(strType, expr.range)
          case (e, et, em) => StrSplit(e, et, Some(em))(expr.range)
      case StrTrim(e) =>
        e.simplify match
          case StrLit(s) => StrLit(s.trim)(expr.range)
          case e => StrTrim(e)(expr.range)
      case StrToLower(e) =>
        e.simplify match
          case StrLit(s) => StrLit(s.toLowerCase)(expr.range)
          case e => StrToLower(e)(expr.range)
      case StrToUpper(e) =>
        e.simplify match
          case StrLit(s) => StrLit(s.toUpperCase)(expr.range)
          case e => StrToUpper(e)(expr.range)
      case StrToInt(e) =>
        e.simplify match
          case StrLit(s) => IntLit(s.toInt)(expr.range)
          case e => StrToInt(e)(expr.range)
      case StrFromInt(e, fmt) =>
        e.simplify match
          case IntLit(i) => StrLit(i.toString)(expr.range)
          case e => StrFromInt(e, fmt)(expr.range)
      case StrIsAscii(e) =>
        e.simplify match
          case StrLit(s) => BoolLit(s.forall(_.toInt <= 0x7F))(expr.range)
          case e => StrIsAscii(e)(expr.range)

      // Set
      case s@SetLit(es) =>
        SetLit(es.map(_.simplify))(s.elemSort, s.range)
      case SetSize(e) =>
        e.simplify match
          case s@SetLit(es) => IntLit(es.size)(expr.range)
          case e => SetSize(e)(expr.range)
      case SetContains(e, ex) =>
        (e.simplify, ex.simplify) match
          case (s@SetLit(es), ex) if s.isConst && ex.isConst => BoolLit(es.contains(ex))(expr.range)
          case (e, ex) => SetContains(e, ex)(expr.range)
      case Subset(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetLit(es1), s2@SetLit(es2)) if s1.isConst && s2.isConst =>
            BoolLit(es1.toSet.subsetOf(es2.toSet))(expr.range)
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
            SetLit(eks.zip(evs).map(mkTuple(_, _)))(mkTupleType(m.keySort, m.valSort), expr.range)
          case e => MapItems(e)(expr.range)
      case MapSize(e) =>
        e.simplify match
          case MapLit(eks, _) => IntLit(eks.size)(expr.range)
          case e => MapSize(e)(expr.range)
      case MapContains(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapLit(eks, _), ek) if m.isConst && ek.isConst => BoolLit(eks.contains(ek))(expr.range)
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
          case StrLit(s) => BoolLit(r.contains(s.toList))
          case e => StringInLang(e, r)

      case _ => throw NotImplementedError(s"Simplifier.simplify(${expr.getClass.getSimpleName})")

    def toCNF: List[Expr] = expr match
      case And(e1, e2) => e1.toCNF ++ e2.toCNF
      case _ => List(expr)

  extension (s: String)
    private def countSlice(t: String): Int = s.sliding(t.length).count(_ == t)

  extension [T](s: List[T])
    private def countSlice(t: List[T]): Int = s.sliding(t.length).count(_ == t)