package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.given
import flat.checker.flan.*
import flat.checker.flan.Subst.subst
import flat.checker.flan.tpd.*

object Simplifier extends LazyLogging:
  extension (expr: Expr)
    private def isConst: Boolean = expr match
      case IntConst(_) | BoolConst(_) | StrConst(_) | NullConst() => true
      case ListExpr(es) => es.forall(_.isConst)
      case SetExpr(es) => es.forall(_.isConst) && es.distinct.size == es.size
      case MapExpr(eks, _) => eks.forall(_.isConst) && eks.distinct.size == eks.size
      case _ => false

    def simplify: Expr = expr match
      case IntConst(_) | BoolConst(_) | CharConst(_) | StrConst(_) | NullConst() => expr
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
          case (e1, e2) if e1 == e2 => BoolConst(true)(expr.range)
          case (e1, e2) => Eq(e1, e2)(expr.range)
      case Ne(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e1, e2) => Ne(e1, e2)(expr.range)
      case Ite(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (BoolConst(true), e1, _) => e1
          case (BoolConst(false), _, e2) => e2
          case (e, e1, e2) => Ite(e, e1, e2)(expr.range)

      // Boolean
      case And(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 && b2)(expr.range)
          case (BoolConst(true), e) => e
          case (e, BoolConst(true)) => e
          case (BoolConst(false), _) | (_, BoolConst(false)) => BoolConst(false)(expr.range)
          case (e1, e2) => And(e1, e2)(expr.range)
      case Or(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 || b2)(expr.range)
          case (BoolConst(false), e) => e
          case (e, BoolConst(false)) => e
          case (BoolConst(true), _) | (_, BoolConst(true)) => BoolConst(true)(expr.range)
          case (e1, e2) => Or(e1, e2)(expr.range)
      case Not(e) =>
        e.simplify match
          case BoolConst(b) => BoolConst(!b)(expr.range)
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
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(!b1 || b2)(expr.range)
          case (BoolConst(true), e) => e
          case (BoolConst(false), _) | (_, BoolConst(true)) => BoolConst(true)(expr.range)
          case (e1, e2) => Implies(e1, e2)(expr.range)

      // Int arithmetic
      case Negate(e) =>
        e.simplify match
          case IntConst(i) => IntConst(-i)(expr.range)
          case Negate(e) => e
          case e => Negate(e)(expr.range)
      case Add(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 + i2)(expr.range)
          case (IntConst(0), e) => e
          case (e, IntConst(0)) => e
          case (e1, e2) => Add(e1, e2)(expr.range)
      case Sub(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 - i2)(expr.range)
          case (e, IntConst(0)) => e
          case (e1, e2) => Sub(e1, e2)(expr.range)
      case Mul(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 * i2)(expr.range)
          case (IntConst(1), e) => e
          case (e, IntConst(1)) => e
          case (IntConst(0), _) | (_, IntConst(0)) => IntConst(0)(expr.range)
          case (e1, e2) => Mul(e1, e2)(expr.range)
      case Div(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) if i2 != 0 => IntConst(i1 / i2)(expr.range)
          case (e, IntConst(1)) => e
          case (e1, e2) => Div(e1, e2)(expr.range)
      case Mod(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) if i2 != 0 => IntConst(i1 % i2)(expr.range)
          case (e, IntConst(1)) => IntConst(0)(expr.range)
          case (e1, e2) => Mod(e1, e2)(expr.range)

      // Int relational
      case Le(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => BoolConst(i1 <= i2)(expr.range)
          case (e1, e2) => Le(e1, e2)(expr.range)
      case Lt(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => BoolConst(i1 < i2)(expr.range)
          case (StrIndexOf(e1, _, _), StrLength(e2)) if e1 == e2 => BoolConst(true)(expr.range) // s.indexOf(t) < |s|
          case (e1, e2) => Lt(e1, e2)(expr.range)

      // String: access
      case StrLength(e) =>
        e.simplify match
          case StrConst(s) => IntConst(s.length)(expr.range)
          case e => StrLength(e)(expr.range)
      case CharAt(e, ei) =>
        (e.simplify, ei.simplify) match
          case (StrConst(s), IntConst(i)) => CharConst(s.charAt(i.intValue))
          case (e, ei) => CharAt(e, ei)(expr.range)
      case Substr(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (StrConst(s), IntConst(i), IntConst(j)) => StrConst(s.substring(i.intValue, j.intValue))(expr.range)
          case (e, ei, ej) => Substr(e, ei, ej)(expr.range)

      // String: construction
      case StrConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@ListExpr(es1), ListExpr(es2)) => ListExpr(es1 ++ es2)(s1.elemSort, expr.range)
          case (StrConst(s1), StrConst(s2)) => StrConst(s1 + s2)(expr.range)
          case (e1, e2) => StrConcat(e1, e2)(expr.range)
      case StrReverse(e) =>
        e.simplify match
          case s@ListExpr(es) => ListExpr(es.reverse)(s.elemSort, s.range)
          case StrConst(s) => StrConst(s.reverse)(expr.range)
          case e => StrReverse(e)(expr.range)
      case StrJoin(e, el) =>
        (e.simplify, el.simplify) match
          case (StrConst(sep), ListExpr(es)) if es.forall(_.isInstanceOf[StrConst]) =>
            val ss = es.collect { case StrConst(s) => s }
            StrConst(ss.mkString(sep))(expr.range)
          case (e, el) => StrJoin(e, el)(expr.range)

      // String: test
      case StrStartsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.startsWith(t))(expr.range)
          case (e, et) => StrStartsWith(e, et)(expr.range)
      case StrEndsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.endsWith(t))(expr.range)
          case (e, et) => StrEndsWith(e, et)(expr.range)
      case StrIs(e, a) =>
        e.simplify match
          case StrConst(s) => ???
          case e => StrIs(e, a)
      case StrIn(e, r) =>
        e.simplify match
          case StrConst(s) => BoolConst(r.contains(s.toList))
          case e => StrIn(e, r)

      // String: search
      case StrContains(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.contains(t))(expr.range)
          case (e, et) => StrContains(e, et)(expr.range)
      case StrIndexOf(e, et, ei) =>
        (e.simplify, et.simplify, ei.simplify) match
          case (StrConst(s), StrConst(t), IntConst(i)) => IntConst(s.indexOf(t, i.toInt))
          case (e, et, ei) => StrIndexOf(e, et, ei)(expr.range)
      case StrCount(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@ListExpr(es1), s2@ListExpr(es2)) if s1.isConst && s2.isConst => IntConst(es1.countSlice(es2))(expr.range)
          case (StrConst(s), StrConst(t)) => IntConst(s.countSlice(t))(expr.range)
          case (e, et) => StrCount(e, et)(expr.range)
      case StrSplit(e, ex, None) =>
        (e.simplify, ex.simplify) match
          case (StrConst(s), StrConst(t)) => ListExpr(s.split(t).toList.map(StrConst(_)))(StrType, expr.range)
          case (e, et) => StrSplit(e, et)(expr.range)
      case StrSplit(e, ex, Some(em)) =>
        (e.simplify, ex.simplify, em.simplify) match
          case (StrConst(s), StrConst(t), IntConst(m)) =>
            ListExpr(s.split(t, m.intValue).toList.map(StrConst(_)))(StrType, expr.range)
          case (e, et, em) => StrSplit(e, et, Some(em))(expr.range)
      case StrReplace(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (StrConst(s), StrConst(s1), StrConst(s2)) => StrConst(s.replace(s1, s2))(expr.range)
          case (e, e1, e2) => StrReplace(e, e1, e2)(expr.range)

      // String: conversion
      case StrTrim(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.trim)(expr.range)
          case e => StrTrim(e)(expr.range)
      case StrToLower(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.toLowerCase)(expr.range)
          case e => StrToLower(e)(expr.range)
      case StrToUpper(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.toUpperCase)(expr.range)
          case e => StrToUpper(e)(expr.range)
      case StrToInt(e) =>
        e.simplify match
          case StrConst(s) => IntConst(s.toInt)(expr.range)
          case e => StrToInt(e)(expr.range)
      case IntFormat(e, fmt) =>
        e.simplify match
          case IntConst(i) => StrConst(i.toString)(expr.range)
          case e => IntFormat(e, fmt)(expr.range)
      case CharToCode(e) =>
        e.simplify match
          case CharConst(c) => IntConst(c.toInt)(expr.range)
          case e => CharToCode(e)(expr.range)
      case CodeToChar(e) =>
        e.simplify match
          case IntConst(i) => CharConst(i.toChar)
          case e => CodeToChar(e)(expr.range)

      // List
      case e@ListExpr(es) => ListExpr(es.map(_.simplify))(e.elemSort, e.range)
      case ListLength(e) =>
        e.simplify match
          case s@ListExpr(es) => IntConst(es.size)(expr.range)
          case e => ListLength(e)(expr.range)
      case ListSelect(e, ei) =>
        (e.simplify, ei.simplify) match
          case (s@ListExpr(es), IntConst(i)) => es(i.intValue)
          case (e, ei) => ListSelect(e, ei)(expr.range)
      case ListSlice(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (lst@ListExpr(es), IntConst(i), IntConst(j)) =>
            ListExpr(es.slice(i.toInt, j.toInt))(lst.elemSort, lst.range)
          case (e, ei, ej) => ListSlice(e, ei, ej)(expr.range)
      case ListConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (lst@ListExpr(es1), ListExpr(es2)) => ListExpr(es1 ++ es2)(lst.elemSort, expr.range)
          case (e1, e2) => ListConcat(e1, e2)(expr.range)
      case ListUpdate(e, ei, ev) =>
        (e.simplify, ei.simplify, ev.simplify) match
          case (s@ListExpr(es), IntConst(i), ev) => ListExpr(es.updated(i.toInt, ev))(s.elemSort, s.range)
          case (e, ei, ev) => ListUpdate(e, ei, ev)(expr.range)
      case ListContains(e, ev) =>
        (e.simplify, ev.simplify) match
          case (s@ListExpr(es), ex) if s.isConst && ex.isConst => BoolConst(es.contains(ex))(expr.range)
          case (e, ev) => ListContains(e, ev)(expr.range)

      // List: higher-order
      case ListForall(e, ep) => ListForall(e.simplify, ep.simplify)(expr.range)
      case ListExists(e, ep) => ListExists(e.simplify, ep.simplify)(expr.range)
      case ListMap(e, ef) => ListMap(e.simplify, ef.simplify)(expr.range)
      case ListFilter(e, ep) => ListFilter(e.simplify, ep.simplify)(expr.range)

      // Set
      case s@SetExpr(es) =>
        SetExpr(es.map(_.simplify))(s.elemSort, s.range)
      case SetSize(e) =>
        e.simplify match
          case s@SetExpr(es) => IntConst(es.size)(expr.range)
          case e => SetSize(e)(expr.range)
      case SetContains(e, ex) =>
        (e.simplify, ex.simplify) match
          case (s@SetExpr(es), ex) if s.isConst && ex.isConst => BoolConst(es.contains(ex))(expr.range)
          case (e, ex) => SetContains(e, ex)(expr.range)
      case Subset(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            BoolConst(es1.toSet.subsetOf(es2.toSet))(expr.range)
          case (e1, e2) => Subset(e1, e2)(expr.range)
      case SetUnion(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet | es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetUnion(e1, e2)(expr.range)
      case SetInter(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet & es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetInter(e1, e2)(expr.range)
      case SetDiff(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet -- es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetDiff(e1, e2)(expr.range)
      case SetForall(e, ep) => SetForall(e.simplify, ep.simplify)(expr.range)

      // Map
      case m@MapExpr(eks, evs) => MapExpr(eks.map(_.simplify), evs.map(_.simplify))(m.keySort, m.valSort, m.range)
      case MapKeys(e) =>
        e.simplify match
          case m@MapExpr(eks, _) if m.isConst => SetExpr(eks)(m.keySort, expr.range)
          case e => MapKeys(e)(expr.range)
      case MapValues(e) =>
        e.simplify match
          case m@MapExpr(_, evs) if m.isConst => SetExpr(evs)(m.valSort, expr.range)
          case e => MapValues(e)(expr.range)
      case MapItems(e) =>
        e.simplify match
          case m@MapExpr(eks, evs) if m.isConst =>
            SetExpr(eks.zip(evs).map(mkTuple(_, _)))(mkTupleType(m.keySort, m.valSort), expr.range)
          case e => MapItems(e)(expr.range)
      case MapSize(e) =>
        e.simplify match
          case MapExpr(eks, _) => IntConst(eks.size)(expr.range)
          case e => MapSize(e)(expr.range)
      case MapContains(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapExpr(eks, _), ek) if m.isConst && ek.isConst => BoolConst(eks.contains(ek))(expr.range)
          case (e, ek) => MapContains(e, ek)(expr.range)
      case MapSelect(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapExpr(eks, evs), ek) if m.isConst && ek.isConst => evs(eks.lastIndexOf(ek))
          case (map, ek) => MapSelect(map, ek)(expr.range)
      case MapUpdate(e, ek, ev) =>
        (e.simplify, ek.simplify, ev.simplify) match
          case (m@MapExpr(eks, evs), ek, ev) if m.isConst && ek.isConst =>
            val i = eks.lastIndexOf(ek)
            if i >= 0 then MapExpr(eks, evs.updated(i, ev))(m.keySort, m.valSort, expr.range)
            else MapExpr(eks :+ ek, evs :+ ev)(m.keySort, m.valSort, expr.range)
          case (map, ek, ev) => MapUpdate(map, ek, ev)(expr.range)

      // Tuple
      case TupleExpr(es) => TupleExpr(es.map(_.simplify))(expr.range)
      case TupleSelect(i, es) =>
        es.simplify match
          case TupleExpr(es) => es(i)
          case es => TupleSelect(i, es)(expr.range)

      case _ => throw NotImplementedError(s"Simplifier.simplify(${expr.getClass.getSimpleName})")

    def toCNF: List[Expr] = expr match
      case And(e1, e2) => e1.toCNF ++ e2.toCNF
      case _ => List(expr)

  extension (s: String)
    private def countSlice(t: String): Int = s.sliding(t.length).count(_ == t)

  extension [T](s: List[T])
    private def countSlice(t: List[T]): Int = s.sliding(t.length).count(_ == t)