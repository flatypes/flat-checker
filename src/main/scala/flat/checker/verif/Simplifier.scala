package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.given
import flat.checker.flan.*
import flat.checker.flan.Subst.subst
import flat.checker.flan.tpd.*

object Simplifier extends LazyLogging:
  extension (expr: Expr)
    private def isConst: Boolean = expr match
      case IntConst(_) | BoolConst(_) | CharConst(_) | StrConst(_) | NullConst() => true
      case _ => false

    def simplify: Expr = expr match
      case IntConst(_) | BoolConst(_) | CharConst(_) | StrConst(_) | NullConst() | Var(_) => expr

      // Functional
      case LambdaExpr(ps, e) => LambdaExpr(ps, e.simplify)
      case Apply(e, es) =>
        (e.simplify, es.map(_.simplify)) match
          case (f@LambdaExpr(_, e), es) =>
            assert(f.paramNames.length == es.length, "lambda apply arity mismatch")
            e.subst(f.paramNames, es)
          case (e, es) => Apply(e, es)

      // Basic
      case Eq(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e1, e2) if e1 == e2 => BoolConst(true)
          case (e1, e2) if e1.isConst && e2.isConst => BoolConst(e1 == e2)
          case (e1, e2) => Eq(e1, e2)
      case Ne(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e1, e2) if e1.isConst && e2.isConst => BoolConst(e1 != e2)
          case (e1, e2) => Ne(e1, e2)
      case Ite(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (BoolConst(true), e1, _) => e1
          case (BoolConst(false), _, e2) => e2
          case (e, e1, e2) => Ite(e, e1, e2)

      // Boolean
      case And(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 && b2)
          case (BoolConst(true), e) => e
          case (e, BoolConst(true)) => e
          case (BoolConst(false), _) | (_, BoolConst(false)) => BoolConst(false)
          case (e1, e2) => And(e1, e2)
      case Or(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 || b2)
          case (BoolConst(false), e) => e
          case (e, BoolConst(false)) => e
          case (BoolConst(true), _) | (_, BoolConst(true)) => BoolConst(true)
          case (e1, e2) => Or(e1, e2)
      case Not(e) =>
        e.simplify match
          case BoolConst(b) => BoolConst(!b)
          case Not(e) => e
          case And(e1, e2) => Or(Not(e1).simplify, Not(e2).simplify)
          case Or(e1, e2) => And(Not(e1).simplify, Not(e2).simplify)
          case Eq(e1, e2) => Ne(e1, e2)
          case Ne(e1, e2) => Eq(e1, e2)
          case Le(e1, e2) => Lt(e2, e1)
          case Lt(e1, e2) => Le(e2, e1)
          case e => Not(e)
      case Implies(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (BoolConst(b1), BoolConst(b2)) => BoolConst(!b1 || b2)
          case (BoolConst(true), e) => e
          case (BoolConst(false), _) | (_, BoolConst(true)) => BoolConst(true)
          case (e1, e2) => Implies(e1, e2)

      // Int arithmetic
      case Negate(e) =>
        e.simplify match
          case IntConst(i) => IntConst(-i)
          case Negate(e) => e
          case e => Negate(e)
      case Add(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 + i2)
          case (IntConst(0), e) => e
          case (e, IntConst(0)) => e
          case (e1, e2) => Add(e1, e2)
      case Sub(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (e, IntConst(0)) => e // e - 0 = e
          case (e1, Sub(e2, e)) if e2 == e1 => e // e1 - (e1 - e) = e
          case (e1, Sub(Sub(e2, e3), e4)) if e2 == e1 => Add(e3, e4) // e1 - (e1 - e3 - e4) = e3 + e4
          case (Sub(e, IntConst(i)), IntConst(j)) => Sub(e, IntConst(i + j)) // e - i - j = e - (i + j)
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 - i2)
          case (e1, e2) => Sub(e1, e2)
      case Mul(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 * i2)
          case (IntConst(1), e) => e
          case (e, IntConst(1)) => e
          case (IntConst(0), _) | (_, IntConst(0)) => IntConst(0)
          case (e1, e2) => Mul(e1, e2)
      case Div(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) if i2 != 0 => IntConst(i1 / i2)
          case (e, IntConst(1)) => e
          case (e1, e2) => Div(e1, e2)
      case Mod(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) if i2 != 0 => IntConst(i1 % i2)
          case (e, IntConst(1)) => IntConst(0)
          case (e1, e2) => Mod(e1, e2)
      case Pow(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(Math.pow(i1.toDouble, i2.toDouble).toInt)
          case (e, IntConst(0)) => IntConst(1)
          case (e, IntConst(1)) => e
          case (IntConst(0), _) => IntConst(0)
          case (IntConst(1), _) => IntConst(1)
          case (e1, e2) => Pow(e1, e2)

      // Int relational
      case Le(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => BoolConst(i1 <= i2)
          case (e1, e2) => Le(e1, e2)
      case Lt(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => BoolConst(i1 < i2)
          case (StrIndexOf(e1, _, _), StrLength(e2)) if e1 == e2 => BoolConst(true)  // s.indexOf(t) < |s|
          case (e1, e2) => Lt(e1, e2)

      // Integer bitwise
      case BitAnd(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 & i2)
          case (left, right) => BitAnd(left, right)
      case BitOr(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 | i2)
          case (left, right) => BitOr(left, right)
      case BitXor(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 ^ i2)
          case (left, right) => BitXor(left, right)
      case BitNot(e) =>
        e.simplify match
          case IntConst(i) => IntConst(~i)
          case value => BitNot(value)
      case BitShL(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 << i2.toInt)
          case (left, right) => BitShL(left, right)
      case BitShR(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (IntConst(i1), IntConst(i2)) => IntConst(i1 >> i2.toInt)
          case (left, right) => BitShR(left, right)

      // Char
      case CharToStr(e) =>
        e.simplify match
          case CharConst(c) => StrConst(c.toString)
          case chr => CharToStr(chr)
      case CharIn(e, r) =>
        e.simplify match
          case CharConst(c) => BoolConst(r.contains(c))
          case chr => CharIn(chr, r)

      // String: access
      case StrLength(e) =>
        e.simplify match
          case StrConst(s) => IntConst(s.length)
          case e => StrLength(e)
      case CharAt(e, ei) =>
        (e.simplify, ei.simplify) match
          case (StrConst(s), IntConst(i)) => CharConst(s.charAt(i.intValue))
          case (e, ei) => CharAt(e, ei)
      case Substr(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (StrConst(""), _, _) => StrConst("")
          case (StrConst(s), IntConst(i), IntConst(j)) if 0 <= i && i <= j && j < s.length =>
            StrConst(s.substring(i.toInt, j.toInt))
          case (e, ei, ej) if ei == ej => StrConst("")
          case (e, ei, ej) => Substr(e, ei, ej)

      // String: construction
      case StrConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@ListExpr(es1), ListExpr(es2)) => ListExpr(es1 ++ es2)(s1.elemSort, expr.range)
          case (StrConst(s1), StrConst(s2)) => StrConst(s1 + s2)
          case (e1, e2) => StrConcat(e1, e2)
      case StrReverse(e) =>
        e.simplify match
          case s@ListExpr(es) => ListExpr(es.reverse)(s.elemSort, s.range)
          case StrConst(s) => StrConst(s.reverse)
          case e => StrReverse(e)
      case StrJoin(e, el) =>
        (e.simplify, el.simplify) match
          case (StrConst(sep), ListExpr(es)) if es.forall(_.isInstanceOf[StrConst]) =>
            val ss = es.collect { case StrConst(s) => s }
            StrConst(ss.mkString(sep))
          case (e, el) => StrJoin(e, el)

      // String: test
      case StrStartsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.startsWith(t))
          case (e, et) => StrStartsWith(e, et)
      case StrEndsWith(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.endsWith(t))
          case (e, et) => StrEndsWith(e, et)
      case StrIn(e, r) =>
        e.simplify match
          case StrConst(s) => BoolConst(r.contains(s.toList))
          case e => StrIn(e, r)

      // String: search
      case StrContains(e, et) =>
        (e.simplify, et.simplify) match
          case (StrConst(s), StrConst(t)) => BoolConst(s.contains(t))
          case (e, et) => StrContains(e, et)
      case StrIndexOf(e, et, ei) =>
        (e.simplify, et.simplify, ei.simplify) match
          case (StrConst(s), StrConst(t), IntConst(i)) => IntConst(s.indexOf(t, i.toInt))
          case (e, et, ei) => StrIndexOf(e, et, ei)
      case StrCount(e, et) =>
        (e.simplify, et.simplify) match
          case (s1@ListExpr(es1), s2@ListExpr(es2)) if s1.isConst && s2.isConst => IntConst(es1.countSlice(es2))
          case (StrConst(s), StrConst(t)) => IntConst(s.countSlice(t))
          case (e, et) => StrCount(e, et)
      case StrSplit(e, ex, None) =>
        (e.simplify, ex.simplify) match
          case (StrConst(s), StrConst(t)) => ListExpr(s.split(t).toList.map(StrConst(_)))(StrType, expr.range)
          case (e, et) => StrSplit(e, et)
      case StrSplit(e, ex, Some(em)) =>
        (e.simplify, ex.simplify, em.simplify) match
          case (StrConst(s), StrConst(t), IntConst(m)) =>
            ListExpr(s.split(t, m.intValue).toList.map(StrConst(_)))(StrType, expr.range)
          case (e, et, em) => StrSplit(e, et, Some(em))
      case StrReplace(e, e1, e2) =>
        (e.simplify, e1.simplify, e2.simplify) match
          case (StrConst(s), StrConst(s1), StrConst(s2)) => StrConst(s.replace(s1, s2))
          case (e, e1, e2) => StrReplace(e, e1, e2)

      // String: conversion
      case StrTrim(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.trim)
          case e => StrTrim(e)
      case StrToLower(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.toLowerCase)
          case e => StrToLower(e)
      case StrToUpper(e) =>
        e.simplify match
          case StrConst(s) => StrConst(s.toUpperCase)
          case e => StrToUpper(e)
      case StrToInt(e, eb) =>
        (e.simplify, eb.simplify) match
          case (StrConst(s), IntConst(b)) => IntConst(BigInt(s, b.toInt))
          case (e, eb) => StrToInt(e, eb)
      case IntFormat(e, fmt) =>
        e.simplify match
          case IntConst(i) => StrConst(i.toString)
          case e => IntFormat(e, fmt)
      case CharToCode(e) =>
        e.simplify match
          case CharConst(c) => IntConst(c.toInt)
          case e => CharToCode(e)
      case CodeToChar(e) =>
        e.simplify match
          case IntConst(i) => CharConst(i.toChar)
          case e => CodeToChar(e)

      // String higher-order
      case StrForall(e, ep) =>
        e.simplify match
          case StrConst("") => BoolConst(true)
          case e => StrForall(e, ep)

      // List
      case e@ListExpr(es) => ListExpr(es.map(_.simplify))(e.elemSort, e.range)
      case ListLength(e) =>
        e.simplify match
          case s@ListExpr(es) => IntConst(es.size)
          case e => ListLength(e)
      case ListSelect(e, ei) =>
        (e.simplify, ei.simplify) match
          case (s@ListExpr(es), IntConst(i)) => es(i.intValue)
          case (e, ei) => ListSelect(e, ei)
      case ListSlice(e, ei, ej) =>
        (e.simplify, ei.simplify, ej.simplify) match
          case (lst@ListExpr(es), IntConst(i), IntConst(j)) =>
            ListExpr(es.slice(i.toInt, j.toInt))(lst.elemSort, lst.range)
          case (e, ei, ej) => ListSlice(e, ei, ej)
      case ListConcat(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (lst@ListExpr(es1), ListExpr(es2)) => ListExpr(es1 ++ es2)(lst.elemSort, expr.range)
          case (e1, e2) => ListConcat(e1, e2)
      case ListUpdate(e, ei, ev) =>
        (e.simplify, ei.simplify, ev.simplify) match
          case (s@ListExpr(es), IntConst(i), ev) => ListExpr(es.updated(i.toInt, ev))(s.elemSort, s.range)
          case (e, ei, ev) => ListUpdate(e, ei, ev)
      case ListContains(e, ev) =>
        (e.simplify, ev.simplify) match
          case (s@ListExpr(es), ex) if s.isConst && ex.isConst => BoolConst(es.contains(ex))
          case (e, ev) => ListContains(e, ev)
      case ListIndexOf(e, ev, ei) =>
        (e.simplify, ev.simplify, ei.simplify) match
          case (lst, elem, start) => ListIndexOf(lst, elem, start)

      // List: higher-order
      case ListForall(e, ep) => ListForall(e.simplify, ep.simplify)
      case ListExists(e, ep) => ListExists(e.simplify, ep.simplify)
      case ListMap(e, ef) => ListMap(e.simplify, ef.simplify)
      case ListFilter(e, ep) => ListFilter(e.simplify, ep.simplify)

      // Set
      case s@SetExpr(es) =>
        SetExpr(es.map(_.simplify))(s.elemSort, s.range)
      case SetSize(e) =>
        e.simplify match
          case s@SetExpr(es) => IntConst(es.size)
          case e => SetSize(e)
      case SetContains(e, ex) =>
        (e.simplify, ex.simplify) match
          case (s@SetExpr(es), ex) if s.isConst && ex.isConst => BoolConst(es.contains(ex))
          case (e, ex) => SetContains(e, ex)
      case Subset(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            BoolConst(es1.toSet.subsetOf(es2.toSet))
          case (e1, e2) => Subset(e1, e2)
      case SetUnion(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet | es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetUnion(e1, e2)
      case SetInter(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet & es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetInter(e1, e2)
      case SetDiff(e1, e2) =>
        (e1.simplify, e2.simplify) match
          case (s1@SetExpr(es1), s2@SetExpr(es2)) if s1.isConst && s2.isConst =>
            SetExpr((es1.toSet -- es2.toSet).toList)(s1.elemSort, expr.range)
          case (e1, e2) => SetDiff(e1, e2)
      case SetForall(e, ep) => SetForall(e.simplify, ep.simplify)

      // Map
      case m@MapExpr(eks, evs) => MapExpr(eks.map(_.simplify), evs.map(_.simplify))(m.keySort, m.valSort, m.range)
      case MapKeys(e) =>
        e.simplify match
          case m@MapExpr(eks, _) if m.isConst => SetExpr(eks)(m.keySort, expr.range)
          case e => MapKeys(e)
      case MapValues(e) =>
        e.simplify match
          case m@MapExpr(_, evs) if m.isConst => SetExpr(evs)(m.valSort, expr.range)
          case e => MapValues(e)
      case MapItems(e) =>
        e.simplify match
          case m@MapExpr(eks, evs) if m.isConst =>
            SetExpr(eks.zip(evs).map(mkTuple(_, _)))(mkTupleType(m.keySort, m.valSort), expr.range)
          case e => MapItems(e)
      case MapSize(e) =>
        e.simplify match
          case MapExpr(eks, _) => IntConst(eks.size)
          case e => MapSize(e)
      case MapContains(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapExpr(eks, _), ek) if m.isConst && ek.isConst => BoolConst(eks.contains(ek))
          case (e, ek) => MapContains(e, ek)
      case MapSelect(e, ek) =>
        (e.simplify, ek.simplify) match
          case (m@MapExpr(eks, evs), ek) if m.isConst && ek.isConst => evs(eks.lastIndexOf(ek))
          case (map, ek) => MapSelect(map, ek)
      case MapUpdate(e, ek, ev) =>
        (e.simplify, ek.simplify, ev.simplify) match
          case (m@MapExpr(eks, evs), ek, ev) if m.isConst && ek.isConst =>
            val i = eks.lastIndexOf(ek)
            if i >= 0 then MapExpr(eks, evs.updated(i, ev))(m.keySort, m.valSort, expr.range)
            else MapExpr(eks :+ ek, evs :+ ev)(m.keySort, m.valSort, expr.range)
          case (map, ek, ev) => MapUpdate(map, ek, ev)

      // Tuple
      case TupleExpr(es) => TupleExpr(es.map(_.simplify))
      case TupleSelect(i, es) =>
        es.simplify match
          case TupleExpr(es) => es(i)
          case es => TupleSelect(i, es)

      case NoExpr => expr
      case _ => throw NotImplementedError(s"Simplifier.simplify(${expr.getClass.getSimpleName})")

    def toCNF: List[Expr] = expr match
      case BoolConst(true) => Nil
      case And(e1, e2) => e1.toCNF ++ e2.toCNF
      case _ => List(expr)

  extension (s: String)
    private def countSlice(t: String): Int = s.sliding(t.length).count(_ == t)

  extension [T](s: List[T])
    private def countSlice(t: List[T]): Int = s.sliding(t.length).count(_ == t)
