package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.*
import flat.checker.flan.tpd.*
import io.github.cvc5
import io.github.cvc5.Kind.*
import io.github.cvc5.{Kind, Term, TermManager}

import scala.collection.mutable

final case class EncodingCtx(binderStack: List[Map[String, cvc5.Term]] = Nil):
  def pushed(binders: List[(String, cvc5.Term)]): EncodingCtx =
    copy(binderStack = binders.toMap :: binderStack)

  def lookup(name: String): Option[cvc5.Term] =
    binderStack.collectFirst { case m if m.contains(name) => m(name) }

object EncodingCtx:
  val empty: EncodingCtx = EncodingCtx(Nil)

class SMTEncoder(sorts: Map[String, Type], tm: TermManager) extends LazyLogging:
  def encodeExpr(expr: Expr, ctx: EncodingCtx): cvc5.Term = expr match
    case IntConst(v) => tm.mkInteger(v.toString)
    case BoolConst(v) => tm.mkBoolean(v)
    case StrConst(v) => tm.mkString(escapeSMTString(v), true)
    case NullConst() => throw IllegalArgumentException("NullConst")
    case Var(x) => ctx.lookup(x).getOrElse(encodeVar(x))
    case TupleExpr(es) => tm.mkTuple(es.map(encodeExpr(_, ctx)).toArray)
    case Lambda(ps, e) =>
      val binders = for (x, s) <- ps yield x -> tm.mkVar(encodeSort(s), x)
      val body = encodeExpr(e, ctx.pushed(binders))
      tm.mkTerm(LAMBDA, tm.mkTerm(VARIABLE_LIST, binders.map(_._2).toArray), body)
    case Apply(ef, es) => tm.mkTerm(APPLY_UF, (ef :: es).map(encodeExpr(_, ctx)).toArray)

    // Equality
    case Eq(Var(x), NullConst()) =>
      require(ctx.lookup(x).isEmpty, s"optional type binder: $x")
      encodeVarIsNull(x)
    case Ne(Var(x), NullConst()) =>
      require(ctx.lookup(x).isEmpty, s"optional type binder: $x")
      tm.mkTerm(NOT, encodeVarIsNull(x))
    case Eq(e1, e2) => tm.mkTerm(EQUAL, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Ne(e1, e2) => tm.mkTerm(DISTINCT, encodeExpr(e1, ctx), encodeExpr(e2, ctx))

    // Integer arithmetic
    case Negate(e) => tm.mkTerm(NEG, encodeExpr(e, ctx))
    case Add(e1, e2) => tm.mkTerm(ADD, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Sub(e1, e2) => tm.mkTerm(SUB, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Mul(e1, e2) => tm.mkTerm(MULT, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Div(e1, e2) => tm.mkTerm(INTS_DIVISION, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Mod(e1, e2) => tm.mkTerm(INTS_MODULUS, encodeExpr(e1, ctx), encodeExpr(e2, ctx))

    // Integer relational
    case Le(e1, e2) => tm.mkTerm(LEQ, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Lt(e1, e2) => tm.mkTerm(LT, encodeExpr(e1, ctx), encodeExpr(e2, ctx))

    // Boolean
    case And(e1, e2) => tm.mkTerm(AND, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Or(e1, e2) => tm.mkTerm(OR, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Not(e) => tm.mkTerm(NOT, encodeExpr(e, ctx))
    case Implies(e1, e2) => tm.mkTerm(IMPLIES, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case Ite(e, e1, e2) => tm.mkTerm(ITE, encodeExpr(e, ctx), encodeExpr(e1, ctx), encodeExpr(e2, ctx))

    // String
    case StrConcat(e1, e2) => tm.mkTerm(STRING_CONCAT, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case StrLength(e) => tm.mkTerm(STRING_LENGTH, encodeExpr(e, ctx))
    case CharAt(e, ei) => tm.mkTerm(STRING_CHARAT, encodeExpr(e, ctx), encodeExpr(ei, ctx))
    case Substr(e, ei, ej) =>
      val start = encodeExpr(ei, ctx)
      val end = encodeExpr(ej, ctx)
      tm.mkTerm(STRING_SUBSTR, encodeExpr(e, ctx), start, tm.mkTerm(SUB, end, start))
    case StrStartsWith(e, et) => tm.mkTerm(STRING_PREFIX, encodeExpr(et, ctx), encodeExpr(e, ctx))
    case StrEndsWith(e, et) => tm.mkTerm(STRING_SUFFIX, encodeExpr(et, ctx), encodeExpr(e, ctx))
    case StrReverse(e) => tm.mkTerm(STRING_REV, encodeExpr(e, ctx))

    // String: find and replace
    case StrContains(e, et) => tm.mkTerm(STRING_CONTAINS, encodeExpr(e, ctx), encodeExpr(et, ctx))
    case StrIndexOf(e, et, ei) =>
      tm.mkTerm(STRING_INDEXOF, encodeExpr(e, ctx), encodeExpr(et, ctx), encodeExpr(ei, ctx))
    case StrReplace(e, et, er) =>
      tm.mkTerm(STRING_REPLACE_ALL, encodeExpr(e, ctx), encodeExpr(et, ctx), encodeExpr(er, ctx))
    case StrCount(e, et) => mkApplyUF(STRING_COUNT, encodeExpr(e, ctx), encodeExpr(et, ctx))

    // String: split and join
    case StrSplit(e, et, _) => mkApplyUF(STRING_SPLIT, encodeExpr(e, ctx), encodeExpr(et, ctx))
    case StrJoin(e, el) => mkApplyUF(STRING_JOIN, encodeExpr(e, ctx), encodeExpr(el, ctx))

    // String: transformation
    case StrTrim(e) => mkApplyUF(STR_TRIM, encodeExpr(e, ctx))
    case StrToLower(e) => tm.mkTerm(STRING_TO_LOWER, encodeExpr(e, ctx))
    case StrToUpper(e) => tm.mkTerm(STRING_TO_UPPER, encodeExpr(e, ctx))

    // String: conversion
    case CharToCode(e) => tm.mkTerm(STRING_TO_CODE, encodeExpr(e, ctx))
    case CodeToChar(e) => tm.mkTerm(STRING_FROM_CODE, encodeExpr(e, ctx))
    case StrToInt(e) => tm.mkTerm(STRING_TO_INT, encodeExpr(e, ctx))
    case IntFormat(e, _) => tm.mkTerm(STRING_FROM_INT, encodeExpr(e, ctx))

    // String: other
    case StrIs(_, _) => tm.mkConst(bool)
    case StrIn(_, _) => tm.mkConst(bool)

    // List
    case e@ListExpr(es) => mkSeq(encodeSort(e.elemSort), es.map(encodeExpr(_, ctx)))
    case ListLength(e) => tm.mkTerm(SEQ_LENGTH, encodeExpr(e, ctx))
    case ListSelect(e, ei) => tm.mkTerm(SEQ_NTH, encodeExpr(e, ctx), encodeExpr(ei, ctx))
    case ListSlice(e, ei, ej) =>
      val start = encodeExpr(ei, ctx)
      val end = encodeExpr(ej, ctx)
      tm.mkTerm(SEQ_EXTRACT, encodeExpr(e, ctx), start, tm.mkTerm(SUB, end, start))
    case ListConcat(e1, e2) => tm.mkTerm(SEQ_CONCAT, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case ListUpdate(e, ei, ex) =>
      tm.mkTerm(SEQ_UPDATE, encodeExpr(e, ctx), encodeExpr(ei, ctx), tm.mkTerm(SEQ_UNIT, encodeExpr(ex, ctx)))
    case ListContains(e, ev) => tm.mkTerm(SEQ_CONTAINS, encodeExpr(e, ctx), encodeExpr(ev, ctx))

    // List: higher-order
    case _: ListForall => tm.mkConst(bool)
    case _: ListExists => tm.mkConst(bool)
    case ListMap(e, ef) =>
      val seq = encodeExpr(e, ctx)
      tm.mkConst(seq.getSort)
    case ListFilter(e, ef) =>
      val seq = encodeExpr(e, ctx)
      tm.mkConst(seq.getSort)

    // Set
    case e@SetExpr(es) => mkSet(encodeSort(e.elemSort), es.map(encodeExpr(_, ctx)))
    case SetSize(e) => tm.mkTerm(SET_CARD, encodeExpr(e, ctx))
    case SetContains(e, ev) => tm.mkTerm(SET_MEMBER, encodeExpr(ev, ctx), encodeExpr(e, ctx))
    case Subset(e1, e2) => tm.mkTerm(SET_SUBSET, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case SetUnion(e1, e2) => tm.mkTerm(SET_UNION, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case SetInter(e1, e2) => tm.mkTerm(SET_INTER, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case SetDiff(e1, e2) => tm.mkTerm(SET_MINUS, encodeExpr(e1, ctx), encodeExpr(e2, ctx))
    case SetForall(e, ef) => tm.mkTerm(SET_ALL, encodeExpr(ef, ctx), encodeExpr(e, ctx))

    // Map
    case e@MapExpr(eks, evs) =>
      val keySort = encodeSort(e.keySort)
      val keys = eks.map(encodeExpr(_, ctx))
      val arrSort = tm.mkArraySort(keySort, encodeSort(e.valSort))
      tm.mkTuple(Array(mkSet(keySort, keys), mkArray(arrSort, keys, evs.map(encodeExpr(_, ctx)))))
    case MapKeys(e) => encodeTupleSelect(0, e, ctx)
    case MapValues(e) =>
      val keys = encodeTupleSelect(0, e, ctx)
      val arr = encodeTupleSelect(1, e, ctx)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(SET_MAP, mkLambda(key, tm.mkTerm(SELECT, arr, key)), keys)
    case MapItems(e) =>
      val keys = encodeTupleSelect(0, e, ctx)
      val arr = encodeTupleSelect(1, e, ctx)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(SET_MAP, mkLambda(key, tm.mkTuple(Array(key, tm.mkTerm(SELECT, arr, key)))), keys)
    case MapSize(e) =>
      val keys = encodeTupleSelect(0, e, ctx)
      tm.mkTerm(SET_CARD, keys)
    case MapContains(e, ek) =>
      val keys = encodeTupleSelect(0, e, ctx)
      tm.mkTerm(SET_MEMBER, encodeExpr(ek, ctx), keys)
    case MapSelect(e, ek) =>
      val arr = encodeTupleSelect(1, e, ctx)
      tm.mkTerm(SELECT, arr, encodeExpr(ek, ctx))
    case MapUpdate(e, ek, ev) =>
      val keys = encodeTupleSelect(0, e, ctx)
      val arr = encodeTupleSelect(1, e, ctx)
      val key = encodeExpr(ek, ctx)
      tm.mkTuple(Array(tm.mkTerm(SET_INSERT, key, keys), tm.mkTerm(STORE, arr, key, encodeExpr(ev, ctx))))

    // Tuple
    case TupleSelect(i, e) => encodeTupleSelect(i, e, ctx)

  private val vars: mutable.Map[String, cvc5.Term] = mutable.Map.empty

  private def encodeVar(name: String): cvc5.Term =
    vars.get(name) match
      case Some(t) => t
      case None =>
        val term = tm.mkConst(encodeSort(sorts(name)), name)
        vars(name) = term
        term

  private def encodeVarIsNull(name: String): cvc5.Term =
    sorts(name) match
      case OptType(_) =>
        val indicatorName = name + ".isNull"
        vars.get(indicatorName) match
          case Some(t) => t
          case None =>
            val term = tm.mkConst(tm.getBooleanSort, indicatorName)
            vars(indicatorName) = term
            term
      case _ =>
        throw IllegalArgumentException(s"$name has non-optional type ${sorts(name)}")

  private def encodeSort(sort: Type): cvc5.Sort = sort match
    case IntType => tm.getIntegerSort
    case BoolType => tm.getBooleanSort
    case StrType => tm.getStringSort
    case ListType(t) => tm.mkSequenceSort(encodeSort(t))
    case SetType(t) => tm.mkSetSort(encodeSort(t))
    case MapType(tk, tv) => encodeMapSort(tk, tv)
    case TupleType(ts) => tm.mkTupleSort(ts.map(encodeSort).toArray)
    case FunType(ps, r) => tm.mkFunctionSort(ps.map(encodeSort).toArray, encodeSort(r))
    // optional types are encoded as the underlying type, with a separate boolean for nullness
    case OptType(t) => encodeSort(t)
    case _ => throw IllegalArgumentException(sort.getClass.getSimpleName)

  // DSL
  inline def int: cvc5.Sort = tm.getIntegerSort

  inline def bool: cvc5.Sort = tm.getBooleanSort

  inline def str: cvc5.Sort = tm.getStringSort

  def list(s: cvc5.Sort): cvc5.Sort = tm.mkSequenceSort(s)

  extension (sort: cvc5.Sort)
    inline def ->(returnSort: cvc5.Sort): cvc5.Sort = tm.mkFunctionSort(Array(sort), returnSort)

  extension (sorts: (cvc5.Sort, cvc5.Sort))
    inline def ->(returnSort: cvc5.Sort): cvc5.Sort = tm.mkFunctionSort(Array(sorts._1, sorts._2), returnSort)

  // Uninterpreted operations
  private val STRING_COUNT = tm.mkConst((str, str) -> int, "str.count")
  private val STRING_SPLIT = tm.mkConst((str, str) -> list(str), "str.split")
  private val STRING_JOIN = tm.mkConst((str, list(str)) -> str, "str.join")
  private val STR_TRIM = tm.mkConst(str -> str, "str.trim")

  /** We encode a (partial) map as a pair of:
   * 1. a set that stores all keys
   * 2. an array (which is a total function) that stores values for each key (uninterpreted for undefined keys)
   */
  private def encodeMapSort(keySort: Type, valSort: Type): cvc5.Sort =
    val k = encodeSort(keySort)
    tm.mkTupleSort(Array(tm.mkSetSort(k), tm.mkArraySort(k, encodeSort(valSort))))

  private def encodeTupleSelect(i: Int, tupleExpr: Expr, ctx: EncodingCtx): cvc5.Term =
    val tuple = encodeExpr(tupleExpr, ctx)
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
