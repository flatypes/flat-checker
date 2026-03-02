package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Ops.CmpOp
import flat.checker.ast.*
import flat.regex.RegEx.*
import flat.regex.{Domain, ProductDomain, RegEx}
import io.github.cvc5
import io.github.cvc5.Kind

import scala.collection.mutable

class SMTEncoder(using varCtx: VarCtx, sortingContext: SortingContext, extractMode: Boolean) extends LazyLogging:
  val tm = cvc5.TermManager()

  def encodeSort(sort: Sort): cvc5.Sort = sort match
    case IntSort => tm.getIntegerSort
    case BoolSort => tm.getBooleanSort
    case StringSort => tm.getStringSort
    case UnitSort => tm.mkTupleSort(Array.empty)
    case TupleSort(ss) => tm.mkTupleSort(ss.map(encodeSort).toArray)
    case SeqSort(s) => tm.mkSequenceSort(encodeSort(s))
    case SetSort(s) => tm.mkSetSort(encodeSort(s))
    case _ => throw IllegalArgumentException(sort.toString)

  def encodeRE(re: RegEx): cvc5.Term = re match
    case RENone => tm.mkTerm(Kind.REGEXP_NONE)
    case RENull => tm.mkTerm(Kind.STRING_TO_REGEXP, tm.mkString(""))
    case RELit(cs) =>
      if cs.isEmpty then tm.mkTerm(Kind.REGEXP_NONE)
      else if cs.isFull then tm.mkTerm(Kind.REGEXP_ALLCHAR)
      else
        val (chars, isInc) = cs.toSMT
        val cases = chars.map:
          case c: Char => tm.mkTerm(Kind.STRING_TO_REGEXP, tm.mkString(c.toString))
          case (c1, c2) => tm.mkTerm(Kind.REGEXP_RANGE, tm.mkString(c1.toString), tm.mkString(c2.toString))
        val t = cases.reduce(tm.mkTerm(Kind.REGEXP_UNION, _, _))
        if isInc then t else tm.mkTerm(Kind.REGEXP_DIFF, tm.mkTerm(Kind.REGEXP_ALLCHAR), t)
    case REConcat(r1, r2) =>
      val t1 = encodeRE(r1)
      val t2 = encodeRE(r2)
      tm.mkTerm(Kind.REGEXP_CONCAT, t1, t2)
    case REUnion(r1, r2) =>
      val t1 = encodeRE(r1)
      val t2 = encodeRE(r2)
      tm.mkTerm(Kind.REGEXP_UNION, t1, t2)
    case REStar(r) =>
      val t = encodeRE(r)
      tm.mkTerm(Kind.REGEXP_STAR, t)

  private val ctx = mutable.Map.empty[String | Expr, cvc5.Term]

  def getCtx: Map[Expr, cvc5.Term] = ctx.collect { case (e: Expr, t) => e -> t }.toMap

  private def encodeAtomicExpr(expr: Expr): cvc5.Term =
    ctx.get(expr) match
      case Some(t) => t
      case None =>
        val t = tm.mkConst(encodeSort(expr.sort))
        ctx(expr) = t
        t

  def encodeExpr(expr: Expr): cvc5.Term = expr match
    case Const(n: Int) => tm.mkInteger(n)
    case Const(b: Boolean) => tm.mkBoolean(b)
    case Const(s: String) => tm.mkString(s)
    case Var(x) => ctx.getOrElse(x, encodeAtomicExpr(Var(x)))
    case TupleOf(es) =>
      val ts = es.map(encodeExpr)
      tm.mkTuple(ts.toArray)
    case RefinedBy(e, t) =>
      if extractMode then encodeRefinedBy(e, t) else tm.mkConst(tm.getBooleanSort)

    // Boolean operations
    case And(bs) => tm.mkTerm(Kind.AND, bs.map(encodeExpr).toArray)
    case Or(bs) => tm.mkTerm(Kind.OR, bs.map(encodeExpr).toArray)
    case Not(b) =>
      val t = encodeExpr(b)
      tm.mkTerm(Kind.NOT, t)
    case Ite(b, e1, e2) =>
      val t = encodeExpr(b)
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      tm.mkTerm(Kind.ITE, t, t1, t2)
    case Forall(xs, e) =>
      val binders = for x <- xs yield
        val binder = tm.mkVar(encodeSort(x.sort), x.name)
        ctx(x.name) = binder
        binder
      val body = encodeExpr(e)
      for x <- xs do
        ctx.remove(x.name)
      tm.mkTerm(Kind.FORALL, tm.mkTerm(Kind.VARIABLE_LIST, binders.toArray), body)
    case RelExpr(op, e1, e2) =>
      val kind = op match
        case CmpOp.EQ => Kind.EQUAL
        case CmpOp.NE => Kind.DISTINCT
        case CmpOp.LE => Kind.LEQ
        case CmpOp.LT => Kind.LT
        case CmpOp.GE => Kind.GEQ
        case CmpOp.GT => Kind.GT
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      tm.mkTerm(kind, t1, t2)

    // Arithmetic operations
    case Negate(e) => // -e = 0 - e
      val t = encodeExpr(e)
      tm.mkTerm(Kind.SUB, tm.mkInteger(0), t)
    case Add(e1, e2) => tm.mkTerm(Kind.ADD, encodeExpr(e1), encodeExpr(e2))
    case Sub(e1, e2) => tm.mkTerm(Kind.SUB, encodeExpr(e1), encodeExpr(e2))
    case Mul(e1, e2) => tm.mkTerm(Kind.MULT, encodeExpr(e1), encodeExpr(e2))

    // String operations
    case StringConcat(es1, es2) =>
      val ts1 = encodeExpr(es1)
      val ts2 = encodeExpr(es2)
      tm.mkTerm(Kind.STRING_CONCAT, ts1, ts2)
    case StringLength(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_LENGTH, ts)
    case CharAt(es, ei) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      tm.mkTerm(Kind.STRING_CHARAT, ts, ti)
    case Substring(es, ei, ej) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      val tj = encodeExpr(ej)
      val tl = tm.mkTerm(Kind.SUB, tj, ti)
      // the last argument is the length of the substring
      tm.mkTerm(Kind.STRING_SUBSTR, ts, ti, tl)
    case StringStartsWith(es, et) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_PREFIX, tt, ts)
    case StringEndsWith(es, et) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_SUFFIX, tt, ts)
    case StringContains(es, et) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_CONTAINS, ts, tt)
    case StringIndexOf(es, et) =>
      val ts = encodeExpr(es)
      val tt = encodeExpr(et)
      // the last argument is the start index of finding
      tm.mkTerm(Kind.STRING_INDEXOF, ts, tt, tm.mkInteger(0))
    case StringReverse(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_REV, ts)
    case CharToCode(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_TO_CODE, ts)
    case CharFromCode(e) =>
      val t = encodeExpr(e)
      tm.mkTerm(Kind.STRING_FROM_CODE, t)
    case StringToInt(es, 10) =>
      val ts = encodeExpr(es)
      // NOTE: only nonnegative values are supported
      tm.mkTerm(Kind.STRING_TO_INT, ts)
    case StringFromInt(e) =>
      val t = encodeExpr(e)
      // NOTE: only nonnegative values are supported
      tm.mkTerm(Kind.STRING_FROM_INT, t)

    // List operations
    case SeqOf(es) =>
      val elems = es.map(encodeExpr)
      val singletons = elems.map(tm.mkTerm(Kind.SEQ_UNIT, _))
      singletons.reduce(tm.mkTerm(Kind.SEQ_CONCAT, _, _))
    case SeqLength(e) =>
      val seq = encodeExpr(e)
      tm.mkTerm(Kind.SEQ_LENGTH, seq)
    case SeqConcat(e1, e2) =>
      val seq1 = encodeExpr(e1)
      val seq2 = encodeExpr(e2)
      tm.mkTerm(Kind.SEQ_CONCAT, seq1, seq2)
    case SeqGet(e, ei) =>
      val seq = encodeExpr(e)
      val idx = encodeExpr(ei)
      tm.mkTerm(Kind.SEQ_NTH, seq, idx)
    case SeqSlice(e, ei, ej) =>
      val seq = encodeExpr(e)
      val start = encodeExpr(ei)
      val end = encodeExpr(ej)
      val len = tm.mkTerm(Kind.SUB, end, start)
      tm.mkTerm(Kind.SEQ_EXTRACT, seq, start, len)
    case SeqContains(e, ex) =>
      val seq = encodeExpr(e)
      val elem = encodeExpr(ex)
      val elemSeq = tm.mkTerm(Kind.SEQ_UNIT, elem)
      tm.mkTerm(Kind.SEQ_CONTAINS, seq, elemSeq)
    case SeqIndexOf(e, ex, ei, ej) =>
      val seq = encodeExpr(e)
      val elem = encodeExpr(ex)
      val elemSeq = tm.mkTerm(Kind.SEQ_UNIT, elem)
      val start = encodeExpr(ei)
      val seq1 =
        if ej == SeqLength(e) then seq
        else tm.mkTerm(Kind.SEQ_EXTRACT, seq, tm.mkInteger(0), encodeExpr(ej))
      tm.mkTerm(Kind.SEQ_INDEXOF, seq1, elemSeq, start)

    // Set operations
    case SetOf(elems) =>
      val sets = elems.map: e =>
        val te = encodeExpr(e)
        tm.mkTerm(Kind.SET_SINGLETON, te)
      if sets.isEmpty then tm.mkEmptySet(tm.getIntegerSort) else sets.reduce(tm.mkTerm(Kind.SET_UNION, _, _))

    case Subset(e1, e2) =>
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      tm.mkTerm(Kind.SET_SUBSET, t1, t2)

    // Map operations
    case MapOf(items) =>
      val keys = items.map { case TupleOf(List(k, _)) => encodeExpr(k) }
      val values = items.map { case TupleOf(List(_, v)) => encodeExpr(v) }
      val dictSort = tm.mkArraySort(keys.head.getSort, values.head.getSort)
      var dictTerm = tm.mkConst(dictSort)
      for i <- items.indices do
        dictTerm = tm.mkTerm(Kind.STORE, dictTerm, keys(i), values(i))
      dictTerm

    case MapContains(d: MapOf, ek) =>
      val keys = d.keys.map(encodeExpr)
      val keySet = keys.map(tm.mkTerm(Kind.SET_SINGLETON, _)).reduce(tm.mkTerm(Kind.SET_UNION, _, _))
      val tk = encodeExpr(ek)
      tm.mkTerm(Kind.SET_MEMBER, tk, keySet)

    case MapGet(dict, key) =>
      val td = encodeExpr(dict)
      val tk = encodeExpr(key)
      tm.mkTerm(Kind.SELECT, td, tk)

    // Others
    case _ => encodeAtomicExpr(expr)

  private def encodeRefinedBy(value: Expr, domain: Domain): cvc5.Term = domain match
    case r: RegEx =>
      val t = encodeExpr(value)
      tm.mkTerm(Kind.STRING_IN_REGEXP, t, encodeRE(r))
    case ProductDomain(ts) if ts.forall(_.isInstanceOf[RegEx]) =>
      val rs = ts.map(_.asInstanceOf[RegEx])
      value match
        case TupleOf(es) if es.length == ts.length =>
          val tests = for i <- rs.indices yield
            val tsi = encodeExpr(es(i))
            tm.mkTerm(Kind.STRING_IN_REGEXP, tsi, encodeRE(rs(i)))
          tests.reduce(_.andTerm(_))
        case _ =>
          val t = encodeExpr(value)
          val dt = t.getSort.getDatatype
          val tests = for i <- rs.indices yield
            val ti = tm.mkTerm(Kind.APPLY_SELECTOR, dt.getConstructor(0).getSelector(i).getTerm, t)
            tm.mkTerm(Kind.STRING_IN_REGEXP, ti, encodeRE(rs(i)))
          tests.reduce(_.andTerm(_))
    case _ =>
      throw UnsupportedOperationException(value.toString + " is " + domain.toString)