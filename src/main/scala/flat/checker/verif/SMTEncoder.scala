package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.*
import io.github.cvc5
import io.github.cvc5.{Kind, TermManager}

import scala.collection.mutable

class SMTEncoder(using vars: Map[String, Sort]):
  val tm: TermManager = TermManager()

  def encodeSort(sort: Sort): cvc5.Sort = sort match
    case BoolSort => tm.getBooleanSort
    case IntSort => tm.getIntegerSort
    case CharSort => tm.getStringSort
    case `stringSort` => tm.getStringSort
    case SeqSort(s) => tm.mkSequenceSort(encodeSort(s))
    case SetSort(s) => tm.mkSetSort(encodeSort(s))
    case MapSort(sk, sv) => encodeMapSort(sk, sv)
    case TupleSort(ss) => tm.mkTupleSort(ss.map(encodeSort).toArray)
    case _ => throw UnsupportedOperationException(s"encode ${sort.getClass.getSimpleName}")

  /** We encode a (partial) map as a pair of:
   * 1. a set that stores all keys
   * 2. an array (which is a total function) that stores values for each key (uninterpreted for undefined keys)
   */
  private def encodeMapSort(keySort: Sort, valSort: Sort): cvc5.Sort =
    val k = encodeSort(keySort)
    tm.mkTupleSort(Array(tm.mkSetSort(k), tm.mkArraySort(k, encodeSort(valSort))))

  def encodeExpr(expr: Expr)(using ctx: Map[String, cvc5.Term]): cvc5.Term = expr match
    case Const(b: Boolean) => tm.mkBoolean(b)
    case Const(i: Int) => tm.mkInteger(i.toString)
    case Const(c: Char) => tm.mkString(escapeString(c.toString), true)
    case Const(s: String) => tm.mkString(escapeString(s), true)
    case Var(x) =>
      ctx.get(x) match
        case Some(t) => t
        case None => encodeUninterpreted(expr, vars(x))
    case Apply(ef, es) => tm.mkTerm(Kind.APPLY_UF, (encodeExpr(ef) :: es.map(encodeExpr)).toArray)
    case Lambda(ps, e) =>
      val vars = for VarDecl(x, t) <- ps yield x -> tm.mkVar(encodeSort(t.sort), x)
      mkLambda(vars.map(_._2), encodeExpr(e)(using ctx ++ vars))

    // Basic
    case Eq(e1, e2) => tm.mkTerm(Kind.EQUAL, encodeExpr(e1), encodeExpr(e2))
    case Ne(e1, e2) => tm.mkTerm(Kind.DISTINCT, encodeExpr(e1), encodeExpr(e2))
    case Ite(e, e1, e2) => tm.mkTerm(Kind.ITE, encodeExpr(e), encodeExpr(e1), encodeExpr(e2))

    // Bool
    case And(e1, e2) => tm.mkTerm(Kind.AND, encodeExpr(e1), encodeExpr(e2))
    case Or(e1, e2) => tm.mkTerm(Kind.OR, encodeExpr(e1), encodeExpr(e2))
    case Not(e) => tm.mkTerm(Kind.NOT, encodeExpr(e))
    case Implies(e1, e2) => tm.mkTerm(Kind.IMPLIES, encodeExpr(e1), encodeExpr(e2))

    // Int arithmetic
    case Negate(e) => tm.mkTerm(Kind.NEG, encodeExpr(e))
    case Add(e1, e2) => tm.mkTerm(Kind.ADD, encodeExpr(e1), encodeExpr(e2))
    case Sub(e1, e2) => tm.mkTerm(Kind.SUB, encodeExpr(e1), encodeExpr(e2))
    case Mul(e1, e2) => tm.mkTerm(Kind.MULT, encodeExpr(e1), encodeExpr(e2))

    // Int relational
    case Le(e1, e2) => tm.mkTerm(Kind.LEQ, encodeExpr(e1), encodeExpr(e2))
    case Lt(e1, e2) => tm.mkTerm(Kind.LT, encodeExpr(e1), encodeExpr(e2))

    // Char
    case CharToInt(e) => tm.mkTerm(Kind.STRING_TO_CODE, encodeExpr(e))
    case CharFromInt(e) => tm.mkTerm(Kind.STRING_FROM_CODE, encodeExpr(e))
    case CharToString(e) => encodeExpr(e)

    // Seq
    case e@SeqLit(es) =>
      val elems = es.map(encodeExpr)
      e.elemSort match
        case CharSort =>
          if elems.isEmpty then tm.mkString("")
          else tm.mkTerm(Kind.STRING_CONCAT, elems.toArray)
        case _ =>
          val elemSort = encodeSort(e.elemSort)
          if elems.isEmpty then tm.mkEmptySequence(elemSort)
          else tm.mkTerm(Kind.SEQ_CONCAT, elems.map(tm.mkTerm(Kind.SEQ_UNIT, _)).toArray)
    case SeqLength(e) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_LENGTH else Kind.SEQ_LENGTH, seq)
    case SeqSelect(e, ei) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_CHARAT else Kind.SEQ_NTH, seq, encodeExpr(ei))
    case SeqUpdate(e, ei, ex) =>
      val seq = encodeExpr(e)
      if seq.getSort.isString then
        tm.mkTerm(Kind.STRING_UPDATE, seq, encodeExpr(ei), encodeExpr(ex))
      else
        tm.mkTerm(Kind.SEQ_UPDATE, seq, encodeExpr(ei), tm.mkTerm(Kind.SEQ_UNIT, encodeExpr(ex)))
    case SeqSlice(e, ei, ej) =>
      val seq = encodeExpr(e)
      val start = encodeExpr(ei)
      val end = ej match
        case NoExpr => tm.mkTerm(if seq.getSort.isString then Kind.STRING_LENGTH else Kind.SEQ_LENGTH, seq)
        case _ => encodeExpr(ej)
      val count = tm.mkTerm(Kind.SUB, end, start)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_SUBSTR else Kind.SEQ_EXTRACT, seq, start, count)
    case SeqConcat(e1, e2) =>
      val seq1 = encodeExpr(e1)
      tm.mkTerm(if seq1.getSort.isString then Kind.STRING_CONCAT else Kind.SEQ_CONCAT, seq1, encodeExpr(e2))
    case SeqReverse(e) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_REV else Kind.SEQ_REV, seq)
    case SeqStartsWith(e, et) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_PREFIX else Kind.SEQ_PREFIX, encodeExpr(et), seq)
    case SeqEndsWith(e, et) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_SUFFIX else Kind.SEQ_SUFFIX, encodeExpr(et), seq)

    // Seq find
    case SeqContains(e, et) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_CONTAINS else Kind.SEQ_CONTAINS, seq, encodeExpr(et))
    case SeqIndexOf(e, et, ei) =>
      val seq = encodeExpr(e)
      tm.mkTerm(if seq.getSort.isString then Kind.STRING_INDEXOF else Kind.SEQ_INDEXOF,
        seq, encodeExpr(et), encodeExpr(ei))
    case _: SeqCount => encodeUninterpreted(expr, IntSort)
    case _: SeqForall => encodeUninterpreted(expr, BoolSort)

    // String-specific
    case _: StringSplit => encodeUninterpreted(expr, SeqSort(stringSort))
    case _: StringTrim => encodeUninterpreted(expr, stringSort)
    case StringToLower(e) => tm.mkTerm(Kind.STRING_TO_LOWER, encodeExpr(e))
    case StringToUpper(e) => tm.mkTerm(Kind.STRING_TO_UPPER, encodeExpr(e))
    case StringToInt(e) => tm.mkTerm(Kind.STRING_TO_INT, encodeExpr(e))
    case StringFromInt(e) => tm.mkTerm(Kind.STRING_FROM_INT, encodeExpr(e))

    // Set
    case e@SetLit(es) => mkSet(encodeSort(e.elemSort), es.map(encodeExpr))
    case SetSize(e) => tm.mkTerm(Kind.SET_CARD, encodeExpr(e))
    case SetContains(e, ev) => tm.mkTerm(Kind.SET_MEMBER, encodeExpr(ev), encodeExpr(e))
    case Subset(e1, e2) => tm.mkTerm(Kind.SET_SUBSET, encodeExpr(e1), encodeExpr(e2))
    case SetUnion(e1, e2) => tm.mkTerm(Kind.SET_UNION, encodeExpr(e1), encodeExpr(e2))
    case SetInter(e1, e2) => tm.mkTerm(Kind.SET_INTER, encodeExpr(e1), encodeExpr(e2))
    case SetDiff(e1, e2) => tm.mkTerm(Kind.SET_MINUS, encodeExpr(e1), encodeExpr(e2))
    case SetForall(e, ep) => tm.mkTerm(Kind.SET_ALL, encodeExpr(ep), encodeExpr(e))

    // Map Operations
    case e@MapLit(eks, evs) =>
      val keySort = encodeSort(e.keySort)
      val keys = eks.map(encodeExpr)
      val arrSort = tm.mkArraySort(keySort, encodeSort(e.valSort))
      tm.mkTuple(Array(mkSet(keySort, keys), mkArray(arrSort, keys, evs.map(encodeExpr))))
    case MapKeys(e) => encodeTupleSelect(0, e)
    case MapValues(e) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(Kind.SET_MAP, mkLambda(key, tm.mkTerm(Kind.SELECT, arr, key)), keys)
    case MapItems(e) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = tm.mkVar(keys.getSort.getSetElementSort)
      tm.mkTerm(Kind.SET_MAP, mkLambda(key, tm.mkTuple(Array(key, tm.mkTerm(Kind.SELECT, arr, key)))), keys)
    case MapSize(e) =>
      val keys = encodeTupleSelect(0, e)
      tm.mkTerm(Kind.SET_CARD, keys)
    case MapContains(e, ek) =>
      val keys = encodeTupleSelect(0, e)
      tm.mkTerm(Kind.SET_MEMBER, encodeExpr(ek), keys)
    case MapSelect(e, ek) =>
      val arr = encodeTupleSelect(1, e)
      tm.mkTerm(Kind.SELECT, arr, encodeExpr(ek))
    case MapUpdate(e, ek, ev) =>
      val keys = encodeTupleSelect(0, e)
      val arr = encodeTupleSelect(1, e)
      val key = encodeExpr(ek)
      tm.mkTuple(Array(tm.mkTerm(Kind.SET_INSERT, key, keys), tm.mkTerm(Kind.STORE, arr, key, encodeExpr(ev))))

    // Tuple Operations
    case TupleExpr(es) => tm.mkTuple(es.map(encodeExpr).toArray)
    case TupleSelect(i, e) => encodeTupleSelect(i, e)

    // Others
    case _ => throw UnsupportedOperationException(s"encode ${expr.getClass.getSimpleName}")

  private val uninterpretedTerms = mutable.Map.empty[Expr, cvc5.Term]

  private def encodeUninterpreted(expr: Expr, sort: Sort): cvc5.Term =
    uninterpretedTerms.get(expr) match
      case Some(t) => t
      case None =>
        val term = tm.mkConst(encodeSort(sort))
        uninterpretedTerms(expr) = term
        term

  private def encodeTupleSelect(i: Int, tupleExpr: Expr)(using ctx: Map[String, cvc5.Term]): cvc5.Term =
    val tuple = encodeExpr(tupleExpr)
    val sel = tuple.getSort.getDatatype.getConstructor(0).getSelector(i).getTerm
    tm.mkTerm(Kind.APPLY_SELECTOR, sel, tuple)

  private def mkSet(elemSort: cvc5.Sort, elems: List[cvc5.Term]): cvc5.Term =
    val emptySet = tm.mkEmptySet(elemSort)
    tm.mkTerm(Kind.SET_INSERT, (elems :+ emptySet).toArray)

  private def mkArray(arraySort: cvc5.Sort, keys: List[cvc5.Term], values: List[cvc5.Term]): cvc5.Term =
    val emptyArray = tm.mkConst(arraySort)
    keys.zip(values).foldLeft(emptyArray) { case (a, (i, v)) => tm.mkTerm(Kind.STORE, a, i, v) }

  private def mkLambda(variable: cvc5.Term, body: cvc5.Term): cvc5.Term =
    tm.mkTerm(Kind.LAMBDA, tm.mkTerm(Kind.VARIABLE_LIST, variable), body)

  private def mkLambda(variables: List[cvc5.Term], body: cvc5.Term): cvc5.Term =
    tm.mkTerm(Kind.LAMBDA, tm.mkTerm(Kind.VARIABLE_LIST, variables.toArray), body)