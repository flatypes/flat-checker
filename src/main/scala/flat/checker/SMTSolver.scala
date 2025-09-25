package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.checker.core.*
import flat.regex.RegEx
import flat.regex.RegEx.*
import io.github.cvc5
import io.github.cvc5.Kind

import scala.collection.mutable

class SMTSolver(using config: Config) extends LazyLogging:
  private val smt = cvc5.TermManager()

  class Task(private val slv: cvc5.Solver, val consts: Map[String, cvc5.Term]):
    def assertions: List[cvc5.Term] = slv.getAssertions.toList

    def proves(): Boolean =
      val result = slv.checkSat()
      result.isUnsat

    def solve(): Either[String, Unit] =
      val result = slv.checkSat()
      if result.isUnsat then
        Right(())
      else if result.isSat then
        val model = for (x, t) <- consts yield x + " = " + slv.getValue(t).toString
        Left(model.mkString("\n"))
      else
        Left(result.getUnknownExplanation.toString)

  /** Creates an SMT solving task from the given proof goal. */
  def create(conclusion: Expr)(using pCtx: PrfCtx): Task =
    val slv = cvc5.Solver(smt)
    slv.setLogic("ALL")
    slv.setOption("produce-models", "true")
    slv.setOption("tlimit-per", config.smtTimeLimit.toString)

    given eCtx: ECtx = mutable.Map.empty

    given Counter = new Counter

    // 1. Declare variables
    for
      e <- pCtx.hypotheses :+ conclusion
      x <- e.collectVars
    do
      val s = encodeSort(pCtx.types(x).toSort)
      val t = smt.mkConst(s, x)
      eCtx(Var(x)) = t
      if config.extractMode then
        for tp <- encodeHasType(t, pCtx.types(x)) do slv.assertFormula(tp)
    // 2. Assert hypotheses
    for h <- pCtx.hypotheses do
      val t = encodeExpr(h)
      slv.assertFormula(t)
    // 3. Assert that the conclusion is false
    val t = encodeExpr(conclusion)
    slv.assertFormula(t.notTerm)

    val consts = Map.from(for e -> t <- eCtx yield t.getSymbol -> t)
    Task(slv, consts)

  private def encodeSort(sort: Sort): cvc5.Sort = sort match
    case Sort.Top | Sort.Bot => assert(false)
    case Sort.I => smt.getIntegerSort
    case Sort.B => smt.getBooleanSort
    case Sort.S => smt.getStringSort
    case Sort.Tuple(ss) => smt.mkTupleSort(ss.map(encodeSort).toArray)
    case Sort.Array(s) => smt.mkArraySort(smt.getIntegerSort, encodeSort(s))
    case Sort.Fun(ss, s) => smt.mkFunctionSort(ss.map(encodeSort).toArray, encodeSort(s))

  private def encodeHasType(term: cvc5.Term, typ: Type): Option[cvc5.Term] = typ match
    case LangType(r) => Some(smt.mkTerm(Kind.STRING_IN_REGEXP, term, encodeRE(r)))
    case _ => None

  private def encodeRE(re: RegEx): cvc5.Term = re match
    case RENone => smt.mkTerm(Kind.REGEXP_NONE)
    case RENull => smt.mkTerm(Kind.STRING_TO_REGEXP, smt.mkString(""))
    case RELit(cs) =>
      if cs.isEmpty then smt.mkTerm(Kind.REGEXP_NONE)
      else if cs.isFull then smt.mkTerm(Kind.REGEXP_ALLCHAR)
      else
        val (chars, isInc) = cs.toSMT
        val cases = chars.map:
          case c: Char => smt.mkTerm(Kind.STRING_TO_REGEXP, smt.mkString(c.toString))
          case (c1, c2) => smt.mkTerm(Kind.REGEXP_RANGE, smt.mkString(c1.toString), smt.mkString(c2.toString))
        val t = cases.reduce(smt.mkTerm(Kind.REGEXP_UNION, _, _))
        if isInc then t else smt.mkTerm(Kind.REGEXP_DIFF, smt.mkTerm(Kind.REGEXP_ALLCHAR), t)
    case REConcat(r1, r2) =>
      val t1 = encodeRE(r1)
      val t2 = encodeRE(r2)
      smt.mkTerm(Kind.REGEXP_CONCAT, t1, t2)
    case REUnion(r1, r2) =>
      val t1 = encodeRE(r1)
      val t2 = encodeRE(r2)
      smt.mkTerm(Kind.REGEXP_UNION, t1, t2)
    case REStar(r) =>
      val t = encodeRE(r)
      smt.mkTerm(Kind.REGEXP_STAR, t)

  private type ECtx = mutable.Map[Expr, cvc5.Term]

  private class Counter:
    private var counter = 0

    def next(): Int =
      counter += 1
      counter

  private def encodeAbs(expr: Expr)(using eCtx: ECtx, counter: Counter): cvc5.Term = eCtx.get(expr) match
    case Some(t) => t
    case None =>
      val t = smt.mkConst(encodeSort(expr.sort), s"abs@${counter.next()}")
      eCtx += expr -> t
      t

  private def encodeExpr(expr: Expr)(using eCtx: ECtx, counter: Counter): cvc5.Term = expr match
    case Const(n: Int) => smt.mkInteger(n)
    case Const(b: Boolean) => smt.mkBoolean(b)
    case Const(s: String) => smt.mkString(s)
    case ex@Var(_) => eCtx(ex)
    case TupleExpr(es) =>
      val ts = es.map(encodeExpr)
      smt.mkTuple(ts.toArray)
    case TypeTest(e, t) =>
      if config.extractMode then encodeTypeTest(e, t) else encodeAbs(expr)

    // Boolean operations
    case And(b1, b2) =>
      val t1 = encodeExpr(b1)
      val t2 = encodeExpr(b2)
      smt.mkTerm(Kind.AND, t1, t2)
    case Or(b1, b2) =>
      val t1 = encodeExpr(b1)
      val t2 = encodeExpr(b2)
      smt.mkTerm(Kind.OR, t1, t2)
    case Not(b) =>
      val t = encodeExpr(b)
      smt.mkTerm(Kind.NOT, t)
    case Ite(b, e1, e2) =>
      val t = encodeExpr(b)
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      smt.mkTerm(Kind.ITE, t, t1, t2)
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
      smt.mkTerm(kind, t1, t2)

    // Arithmetic operations
    case Negate(e) => // -e = 0 - e
      val t = encodeExpr(e)
      smt.mkTerm(Kind.SUB, smt.mkInteger(0), t)
    case Arith(op, e1, e2) =>
      val kind = op match
        case ArithOp.ADD => Kind.ADD
        case ArithOp.SUB => Kind.SUB
      val t1 = encodeExpr(e1)
      val t2 = encodeExpr(e2)
      smt.mkTerm(kind, t1, t2)

    // String operations
    case Concat(es1, es2) =>
      val ts1 = encodeExpr(es1)
      val ts2 = encodeExpr(es2)
      smt.mkTerm(Kind.STRING_CONCAT, ts1, ts2)
    case Length(es) =>
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_LENGTH, ts)
    case CharAt(es, ei) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      smt.mkTerm(Kind.STRING_CHARAT, ts, ti)
    case Substr(es, ei, ej) =>
      val ts = encodeExpr(es)
      val ti = encodeExpr(ei)
      val tj = encodeExpr(ej)
      val tl = smt.mkTerm(Kind.SUB, tj, ti)
      // the last argument is the length of the substring
      smt.mkTerm(Kind.STRING_SUBSTR, ts, ti, tl)
    case PrefixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_PREFIX, tt, ts)
    case SuffixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_SUFFIX, tt, ts)
    case InfixOf(et, es) =>
      val tt = encodeExpr(et)
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_CONTAINS, ts, tt)
    case Find(es, et) =>
      val ts = encodeExpr(es)
      val tt = encodeExpr(et)
      // the last argument is the start index of finding
      smt.mkTerm(Kind.STRING_INDEXOF, ts, tt, smt.mkInteger(0))
    case Reverse(es) =>
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_REV, ts)
    case StrToCode(es) =>
      val ts = encodeExpr(es)
      smt.mkTerm(Kind.STRING_TO_CODE, ts)
    case StrFromCode(e) =>
      val t = encodeExpr(e)
      smt.mkTerm(Kind.STRING_FROM_CODE, t)
    case StrToInt(es) =>
      val ts = encodeExpr(es)
      // NOTE: only nonnegative values are supported
      smt.mkTerm(Kind.STRING_TO_INT, ts)
    case StrFromInt(e) =>
      val t = encodeExpr(e)
      // NOTE: only nonnegative values are supported
      smt.mkTerm(Kind.STRING_FROM_INT, t)

    // Array operations
    case ArraySelect(ea, ei) =>
      val ta = encodeExpr(ea)
      val ti = encodeExpr(ei)
      // Given an index sort `I` and element sort `E`, an array sort `Array I E` is defined for every index `i` in `I`,
      // i.e., a total map from `I` to `E`. Here we simply set `I` to the integer sort.
      smt.mkTerm(Kind.SELECT, ta, ti)

    // Others
    case _ => encodeAbs(expr)

  private def encodeTypeTest(value: Expr, typ: Type)(using eCtx: ECtx, counter: Counter): cvc5.Term = typ match
    case LangType(r) =>
      val t = encodeExpr(value)
      smt.mkTerm(Kind.STRING_IN_REGEXP, t, encodeRE(r))
    case TupleType(ts) if ts.forall(_.isInstanceOf[LangType]) =>
      val rs = ts.map(_.asInstanceOf[LangType].re)
      value match
        case TupleExpr(es) if es.length == ts.length =>
          val tests = for i <- rs.indices yield
            val tsi = encodeExpr(es(i))
            smt.mkTerm(Kind.STRING_IN_REGEXP, tsi, encodeRE(rs(i)))
          tests.reduce(_.andTerm(_))
        case _ =>
          val t = encodeExpr(value)
          val dt = t.getSort.getDatatype
          val tests = for i <- rs.indices yield
            val ti = smt.mkTerm(Kind.APPLY_SELECTOR, dt.getConstructor(0).getSelector(i).getTerm, t)
            smt.mkTerm(Kind.STRING_IN_REGEXP, ti, encodeRE(rs(i)))
          tests.reduce(_.andTerm(_))
    case _ =>
      throw UnsupportedOperationException(value.toString + " is " + typ.toString)

  def proves(conclusion: Expr)(using ctx: PrfCtx): Boolean =
    val task = create(conclusion)
    task.proves()

  def solve(conclusion: Expr)(using pCtx: PrfCtx): Either[String, Unit] =
    val task = create(conclusion)
    task.solve()
