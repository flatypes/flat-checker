package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.ast.*
import flat.regex.RegEx
import flat.regex.RegEx.*
import io.github.cvc5
import io.github.cvc5.Kind

import scala.collection.mutable

final class SMTSolver(using config: Config, types: Types) extends LazyLogging:
  private val tm = cvc5.TermManager()
  private val slv = cvc5.Solver(tm)

  def push(): Unit = slv.push()

  def pop(): Unit = slv.pop()

  def assume(cond: Expr): Unit =
    val t = encodeExpr(cond)
    slv.assertFormula(t)

  def proves(conclusion: Expr): Boolean =
    slv.push()
    val t = encodeExpr(conclusion)
    slv.assertFormula(t.notTerm)
    val slvResult = slv.checkSat()
    slv.pop()
    slvResult.isUnsat

  def solve(conclusion: Expr): Either[String, Unit] =
    slv.push()
    val t = encodeExpr(conclusion)
    slv.assertFormula(t.notTerm)
    val slvResult = slv.checkSat()
    val result =
      if slvResult.isUnsat then
        Right(())
      else if slvResult.isSat then
        val model = for (x, t) <- consts yield x + " = " + slv.getValue(t).toString
        Left(model.mkString("\n"))
      else
        Left(slvResult.getUnknownExplanation.toString)
    slv.pop()
    result

  private def encodeSort(sort: Sort): cvc5.Sort = sort match
    case Sort.Top | Sort.Bot => assert(false)
    case Sort.I => tm.getIntegerSort
    case Sort.B => tm.getBooleanSort
    case Sort.S => tm.getStringSort
    case Sort.Tuple(ss) => tm.mkTupleSort(ss.map(encodeSort).toArray)
    case Sort.Array(s) => tm.mkArraySort(tm.getIntegerSort, encodeSort(s))
    case Sort.Fun(ss, s) => tm.mkFunctionSort(ss.map(encodeSort).toArray, encodeSort(s))

  private def encodeHasType(term: cvc5.Term, typ: Type): Option[cvc5.Term] = typ match
    case LangType(r) => Some(tm.mkTerm(Kind.STRING_IN_REGEXP, term, encodeRE(r)))
    case _ => None

  private def encodeRE(re: RegEx): cvc5.Term = re match
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

  private val consts = mutable.Map.empty[String, cvc5.Term]

  private def encodeVar(name: String): cvc5.Term = consts.get(name) match
    case Some(t) => t
    case None =>
      val s = encodeSort(types(name).toSort)
      val t = tm.mkConst(s, name)
      consts(name) = t
      t

  def encodeExpr(expr: Expr): cvc5.Term = expr match
    case Const(n: Int) => tm.mkInteger(n)
    case Const(b: Boolean) => tm.mkBoolean(b)
    case Const(s: String) => tm.mkString(s)
    case Var(x) => encodeVar(x)
    case TupleExpr(es) =>
      val ts = es.map(encodeExpr)
      tm.mkTuple(ts.toArray)
    case TypeTest(e, t) =>
      if config.extractMode then encodeTypeTest(e, t) else tm.mkConst(tm.getBooleanSort)

    // Boolean operations
    case And(b1, b2) =>
      val t1 = encodeExpr(b1)
      val t2 = encodeExpr(b2)
      tm.mkTerm(Kind.AND, t1, t2)
    case Or(b1, b2) =>
      val t1 = encodeExpr(b1)
      val t2 = encodeExpr(b2)
      tm.mkTerm(Kind.OR, t1, t2)
    case Not(b) =>
      val t = encodeExpr(b)
      tm.mkTerm(Kind.NOT, t)
    case Ite(b, e1, e2) =>
      val t = encodeExpr(b)
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      tm.mkTerm(Kind.ITE, t, t1, t2)
    case Cmp(op, e1, e2) =>
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
    case Arith(op, e1, e2) =>
      val kind = op match
        case ArithOp.ADD => Kind.ADD
        case ArithOp.SUB => Kind.SUB
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      tm.mkTerm(kind, t1, t2)

    // String operations
    case Concat(es1, es2) =>
      val ts1 = encodeExpr(es1)
      val ts2 = encodeExpr(es2)
      tm.mkTerm(Kind.STRING_CONCAT, ts1, ts2)
    case Length(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_LENGTH, ts)
    case CharAt(es, ei) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      tm.mkTerm(Kind.STRING_CHARAT, ts, ti)
    case Substr(es, ei, ej) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      val tj = encodeExpr(ej)
      val tl = tm.mkTerm(Kind.SUB, tj, ti)
      // the last argument is the length of the substring
      tm.mkTerm(Kind.STRING_SUBSTR, ts, ti, tl)
    case PrefixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_PREFIX, tt, ts)
    case SuffixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_SUFFIX, tt, ts)
    case InfixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_CONTAINS, ts, tt)
    case Find(es, et) =>
      val ts = encodeExpr(es)
      val tt = encodeExpr(et)
      // the last argument is the start index of finding
      tm.mkTerm(Kind.STRING_INDEXOF, ts, tt, tm.mkInteger(0))
    case Reverse(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_REV, ts)
    case StrToCode(es) =>
      val ts = encodeExpr(es)
      tm.mkTerm(Kind.STRING_TO_CODE, ts)
    case StrFromCode(e) =>
      val t = encodeExpr(e)
      tm.mkTerm(Kind.STRING_FROM_CODE, t)
    case StrToInt(es) =>
      val ts = encodeExpr(es)
      // NOTE: only nonnegative values are supported
      tm.mkTerm(Kind.STRING_TO_INT, ts)
    case StrFromInt(e) =>
      val t = encodeExpr(e)
      // NOTE: only nonnegative values are supported
      tm.mkTerm(Kind.STRING_FROM_INT, t)

    // Array operations
    case ArrSelect(ea, ei) =>
      val ta = encodeExpr(ea)
      val ti = encodeExpr(ei)
      // Given an index sort `I` and element sort `E`, an array sort `Array I E` is defined for every index `i` in `I`,
      // i.e., a total map from `I` to `E`. Here we simply set `I` to the integer sort.
      tm.mkTerm(Kind.SELECT, ta, ti)

    // Others
    case _ => tm.mkConst(encodeSort(expr.sort))

  private def encodeTypeTest(value: Expr, typ: Type): cvc5.Term = typ match
    case LangType(r) =>
      val t = encodeExpr(value)
      tm.mkTerm(Kind.STRING_IN_REGEXP, t, encodeRE(r))
    case TupleType(ts) if ts.forall(_.isInstanceOf[LangType]) =>
      val rs = ts.map(_.asInstanceOf[LangType].re)
      value match
        case TupleExpr(es) if es.length == ts.length =>
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
      throw UnsupportedOperationException(value.toString + " is " + typ.toString)