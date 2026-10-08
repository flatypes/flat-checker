package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.*
import flat.checker.flan.tpd.*
import io.github.cvc5
import io.github.cvc5.Kind.*
import io.github.cvc5.{Kind, Term, TermManager}

import scala.collection.mutable

class SMTEncoder(using vars: Map[String, Type]) extends LazyLogging:
  val tm: TermManager = TermManager()

  private val strCount = tm.mkConst((str, str) -> int, "str.count")

  private val uninterpretedTerms = mutable.Map.empty[Expr, cvc5.Term]

  def encodeSort(sort: Type): cvc5.Sort = sort match
    case IntType => tm.getIntegerSort
    case BoolType => tm.getBooleanSort
    case StrType => tm.getStringSort
    case ListType(t) => tm.mkSequenceSort(encodeSort(t))
    case SetType(t) => tm.mkSetSort(encodeSort(t))
    case MapType(tk, tv) => encodeMapSort(tk, tv)
    case TupleType(ts) => tm.mkTupleSort(ts.map(encodeSort).toArray)
    case FunType(ps, r) => tm.mkFunctionSort(ps.map(encodeSort).toArray, encodeSort(r))
    case RefinedType(t, _) => encodeSort(t)
    case _ => throw UnsupportedOperationException(s"encode sort ${sort.getClass.getSimpleName}")

  def encodeExpr(expr: Expr)(using ctx: Map[String, cvc5.Term]): cvc5.Term = expr match
    case IntConst(i) => tm.mkInteger(i.toString)
    case BoolConst(b) => tm.mkBoolean(b)
    case StrConst(s) => tm.mkString(escapeSMTString(s), true)
    case NullConst() => throw UnsupportedOperationException("encode null")
    case Var(x) =>
      ctx.get(x) match
        case Some(t) => t
        case None => encodeUninterpreted(expr, vars(x), x)
    case Lambda(ps, e) =>
      val vars = ps.map(p => tm.mkVar(encodeSort(p.typ)))
      mkLambda(vars, encodeExpr(e)(using ctx ++ (ps.map(_.name) zip vars).toMap))
    case Apply(ef, es) => tm.mkTerm(APPLY_UF, (encodeExpr(ef) :: es.map(encodeExpr)).toArray)

    // Basic
    case Eq(e1, e2) => tm.mkTerm(EQUAL, encodeExpr(e1), encodeExpr(e2))
    case Ne(e1, e2) => tm.mkTerm(DISTINCT, encodeExpr(e1), encodeExpr(e2))
    case Ite(e, e1, e2) => tm.mkTerm(ITE, encodeExpr(e), encodeExpr(e1), encodeExpr(e2))

    // Bool
    case And(e1, e2) => tm.mkTerm(AND, encodeExpr(e1), encodeExpr(e2))
    case Or(e1, e2) => tm.mkTerm(OR, encodeExpr(e1), encodeExpr(e2))
    case Not(e) => tm.mkTerm(NOT, encodeExpr(e))
    case Implies(e1, e2) => tm.mkTerm(IMPLIES, encodeExpr(e1), encodeExpr(e2))

    // Int arithmetic
    case Negate(e) => tm.mkTerm(NEG, encodeExpr(e))
    case Add(e1, e2) => tm.mkTerm(ADD, encodeExpr(e1), encodeExpr(e2))
    case Sub(e1, e2) => tm.mkTerm(SUB, encodeExpr(e1), encodeExpr(e2))
    case Mul(e1, e2) => tm.mkTerm(MULT, encodeExpr(e1), encodeExpr(e2))
    case Div(e1, e2) => tm.mkTerm(INTS_DIVISION, encodeExpr(e1), encodeExpr(e2))
    case Mod(e1, e2) => tm.mkTerm(INTS_MODULUS, encodeExpr(e1), encodeExpr(e2))

    // Int relational
    case Le(e1, e2) => tm.mkTerm(LEQ, encodeExpr(e1), encodeExpr(e2))
    case Lt(e1, e2) => tm.mkTerm(LT, encodeExpr(e1), encodeExpr(e2))

    // String: access
    case StrLength(e) => tm.mkTerm(STRING_LENGTH, encodeExpr(e))
    case CharAt(e, ei) => tm.mkTerm(STRING_CHARAT, encodeExpr(e), encodeExpr(ei))
    case Substr(e, ei, ej) =>
      val start = encodeExpr(ei)
      val end = encodeExpr(ej)
      tm.mkTerm(STRING_SUBSTR, encodeExpr(e), start, tm.mkTerm(SUB, end, start))

    // String: construction
    case StrConcat(e1, e2) => tm.mkTerm(STRING_CONCAT, encodeExpr(e1), encodeExpr(e2))
    case StrReverse(e) => tm.mkTerm(STRING_REV, encodeExpr(e))
    case _: StrJoin => encodeUninterpreted(expr, StrType)

    // String: test
    case StrStartsWith(e, et) => tm.mkTerm(STRING_PREFIX, encodeExpr(et), encodeExpr(e))
    case StrEndsWith(e, et) => tm.mkTerm(STRING_SUFFIX, encodeExpr(et), encodeExpr(e))
    case StrIs(_, _) => encodeUninterpreted(expr, BoolType)
    case StrIn(_, _) => encodeUninterpreted(expr, BoolType)

    // String: search
    case StrContains(e, et) => tm.mkTerm(STRING_CONTAINS, encodeExpr(e), encodeExpr(et))
    case StrIndexOf(e, et, ei) => tm.mkTerm(STRING_INDEXOF, encodeExpr(e), encodeExpr(et), encodeExpr(ei))
    case StrCount(e, et) => mkApplyUF(strCount, encodeExpr(e), encodeExpr(et))
    case _: StrSplit => encodeUninterpreted(expr, ListType(StrType))
    case _: StrReplace => encodeUninterpreted(expr, StrType)

    // String: conversion
    case _: StrTrim => encodeUninterpreted(expr, StrType)
    case StrToLower(e) => tm.mkTerm(STRING_TO_LOWER, encodeExpr(e))
    case StrToUpper(e) => tm.mkTerm(STRING_TO_UPPER, encodeExpr(e))
    case StrToInt(e) => tm.mkTerm(STRING_TO_INT, encodeExpr(e))
    case _: IntFormat => encodeUninterpreted(expr, StrType)
    case CharToCode(e) => tm.mkTerm(STRING_TO_CODE, encodeExpr(e))
    case CodeToChar(e) => tm.mkTerm(STRING_FROM_CODE, encodeExpr(e))

    // List
    case e@ListExpr(es) => mkSeq(encodeSort(e.elemSort), es.map(encodeExpr))
    case ListLength(e) => tm.mkTerm(SEQ_LENGTH, encodeExpr(e))
    case ListSelect(e, ei) => tm.mkTerm(SEQ_NTH, encodeExpr(e), encodeExpr(ei))
    case ListSlice(e, ei, ej) =>
      val start = encodeExpr(ei)
      val end = encodeExpr(ej)
      tm.mkTerm(SEQ_EXTRACT, encodeExpr(e), start, tm.mkTerm(SUB, end, start))
    case ListConcat(e1, e2) => tm.mkTerm(SEQ_CONCAT, encodeExpr(e1), encodeExpr(e2))
    case ListUpdate(e, ei, ex) =>
      tm.mkTerm(SEQ_UPDATE, encodeExpr(e), encodeExpr(ei), tm.mkTerm(SEQ_UNIT, encodeExpr(ex)))
    case ListContains(e, ev) => tm.mkTerm(SEQ_CONTAINS, encodeExpr(e), encodeExpr(ev))

    // List: higher-order
    case _: ListForall => encodeUninterpreted(expr, BoolType)
    case _: ListExists => encodeUninterpreted(expr, BoolType)
    case _: ListMap => encodeUninterpreted(expr, ListType(StrType))
    case _: ListFilter => encodeUninterpreted(expr, ListType(StrType))

    // Set
    case e@SetExpr(es) => mkSet(encodeSort(e.elemSort), es.map(encodeExpr))
    case SetSize(e) => tm.mkTerm(SET_CARD, encodeExpr(e))
    case SetContains(e, ev) => tm.mkTerm(SET_MEMBER, encodeExpr(ev), encodeExpr(e))
    case Subset(e1, e2) => tm.mkTerm(SET_SUBSET, encodeExpr(e1), encodeExpr(e2))
    case SetUnion(e1, e2) => tm.mkTerm(SET_UNION, encodeExpr(e1), encodeExpr(e2))
    case SetInter(e1, e2) => tm.mkTerm(SET_INTER, encodeExpr(e1), encodeExpr(e2))
    case SetDiff(e1, e2) => tm.mkTerm(SET_MINUS, encodeExpr(e1), encodeExpr(e2))
    case SetForall(e, ep) => tm.mkTerm(SET_ALL, encodeExpr(ep), encodeExpr(e))

    // Map
    case e@MapExpr(eks, evs) =>
      val keySort = encodeSort(e.keySort)
      val keys = eks.map(encodeExpr)
      val arrSort = tm.mkArraySort(keySort, encodeSort(e.valSort))
      tm.mkTuple(Array(mkSet(keySort, keys), mkArray(arrSort, keys, evs.map(encodeExpr))))
    case MapKeys(e) => encodeTupleSelect(0, e)
    case MapValues(e) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(SET_MAP, mkLambda(key, tm.mkTerm(SELECT, arr, key)), keys)
    case MapItems(e) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(SET_MAP, mkLambda(key, tm.mkTuple(Array(key, tm.mkTerm(SELECT, arr, key)))), keys)
    case MapSize(e) =>
      val keys = encodeTupleSelect(0, e)
      tm.mkTerm(SET_CARD, keys)
    case MapContains(e, ek) =>
      val keys = encodeTupleSelect(0, e)
      tm.mkTerm(SET_MEMBER, encodeExpr(ek), keys)
    case MapSelect(e, ek) =>
      val arr = encodeTupleSelect(1, e)
      tm.mkTerm(SELECT, arr, encodeExpr(ek))
    case MapUpdate(e, ek, ev) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = encodeExpr(ek)
      tm.mkTuple(Array(tm.mkTerm(SET_INSERT, key, keys), tm.mkTerm(STORE, arr, key, encodeExpr(ev))))

    // Tuple
    case TupleExpr(es) => tm.mkTuple(es.map(encodeExpr).toArray)
    case TupleSelect(i, e) => encodeTupleSelect(i, e)

    // Others
    case _ => throw UnsupportedOperationException(s"encode ${expr.getClass.getSimpleName}")

  // DSL
  inline def int: cvc5.Sort = tm.getIntegerSort

  inline def bool: cvc5.Sort = tm.getBooleanSort

  inline def str: cvc5.Sort = tm.getStringSort

  extension (sort: cvc5.Sort)
    inline def ->(returnSort: cvc5.Sort): cvc5.Sort = tm.mkFunctionSort(Array(sort), returnSort)

  extension (sorts: (cvc5.Sort, cvc5.Sort))
    inline def ->(returnSort: cvc5.Sort): cvc5.Sort = tm.mkFunctionSort(Array(sorts._1, sorts._2), returnSort)

  // Terms
  given Conversion[Boolean, cvc5.Term] = tm.mkBoolean

  given Conversion[Int, cvc5.Term] = tm.mkInteger

  inline def mkForall(variables: List[cvc5.Term], body: cvc5.Term, patterns: cvc5.Term*): cvc5.Term =
    tm.mkTerm(FORALL, tm.mkTerm(VARIABLE_LIST, variables.toArray), body,
      tm.mkTerm(INST_PATTERN_LIST, Array(tm.mkTerm(INST_PATTERN, patterns.toArray))))

  inline def mkForall(variable: cvc5.Term, body: cvc5.Term): cvc5.Term = mkForall(List(variable), body)

  /** We encode a (partial) map as a pair of:
   * 1. a set that stores all keys
   * 2. an array (which is a total function) that stores values for each key (uninterpreted for undefined keys)
   */
  private def encodeMapSort(keySort: Type, valSort: Type): cvc5.Sort =
    val k = encodeSort(keySort)
    tm.mkTupleSort(Array(tm.mkSetSort(k), tm.mkArraySort(k, encodeSort(valSort))))

  private def encodeUninterpreted(expr: Expr, sort: Type, name: String = ""): cvc5.Term =
    uninterpretedTerms.get(expr) match
      case Some(t) => t
      case None =>
        val term = if name == "" then tm.mkConst(encodeSort(sort)) else tm.mkConst(encodeSort(sort), name)
        uninterpretedTerms(expr) = term
        term

  private def encodeTupleSelect(i: Int, tupleExpr: Expr)(using ctx: Map[String, cvc5.Term]): cvc5.Term =
    val tuple = encodeExpr(tupleExpr)
    val sel = tuple.getSort.getDatatype.getConstructor(0).getSelector(i).getTerm
    tm.mkTerm(APPLY_SELECTOR, sel, tuple)

  private inline def mkApplyUF(func: cvc5.Term, args: cvc5.Term*): cvc5.Term =
    tm.mkTerm(APPLY_UF, (func +: args).toArray)

  private inline def mkSet(elemSort: cvc5.Sort, elems: List[cvc5.Term]): cvc5.Term =
    val emptySet = tm.mkEmptySet(tm.mkSetSort(elemSort))
    if elems.isEmpty then emptySet
    else tm.mkTerm(SET_INSERT, (elems :+ emptySet).toArray)

  private inline def mkSeq(elemSort: cvc5.Sort, elems: List[cvc5.Term]): cvc5.Term =
    val emptySeq = tm.mkEmptySequence(elemSort)
    if elems.isEmpty then emptySeq
    else if elems.size == 1 then tm.mkTerm(SEQ_UNIT, elems.head)
    else tm.mkTerm(SEQ_CONCAT, elems.map(tm.mkTerm(SEQ_UNIT, _)).toArray)

  private inline def mkArray(arraySort: cvc5.Sort, keys: List[cvc5.Term], values: List[cvc5.Term]): cvc5.Term =
    val emptyArray = tm.mkConst(arraySort)
    keys.zip(values).foldLeft(emptyArray) { case (a, (i, v)) => tm.mkTerm(STORE, a, i, v) }

  private inline def mkLambda(variable: cvc5.Term, body: cvc5.Term): cvc5.Term =
    tm.mkTerm(LAMBDA, tm.mkTerm(VARIABLE_LIST, variable), body)

  private inline def mkLambda(variables: List[cvc5.Term], body: cvc5.Term): cvc5.Term =
    tm.mkTerm(LAMBDA, tm.mkTerm(VARIABLE_LIST, variables.toArray), body)

  private inline def mkSubstr(s: cvc5.Term, start: cvc5.Term, end: cvc5.Term): cvc5.Term =
    tm.mkTerm(STRING_SUBSTR, s, start, tm.mkTerm(SUB, end, start))