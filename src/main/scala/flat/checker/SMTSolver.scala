package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.RegExpr
import flat.{Config, regex}
import io.github.cvc5.{Sort as SMTSort, *}

import scala.collection.mutable

enum SolverResult:
  case Valid
  case Invalid(counterModel: String)

import flat.checker.SolverResult.*

class SMTSolver(using config: Config) extends LazyLogging:
  private val smt = TermManager()

  def create(goal: Expr)(using pCtx: PrfCtx): (Solver, Map[String, Term]) =
    val slv = Solver(smt)
    slv.setLogic("ALL")
    slv.setOption("produce-models", "true")
    slv.setOption("tlimit-per", config.smtTimeLimit.toString)

    val vars = (goal :: pCtx.assumptions).flatMap(_.collectVars).toSet
    val ctxBuf = mutable.Map.empty[String, Term]
    for x <- vars do
      val s = encodeType(pCtx.types(x))
      ctxBuf(x) = smt.mkConst(s, x)
      if config.extractTo.isDefined || config.smtOnly then
        pCtx.types(x) match
          case LangType(r) => slv.assertFormula(smt.mkTerm(Kind.STRING_IN_REGEXP, ctxBuf(x), encodeRegExpr(r)))
          case _ =>

    val ctx = ctxBuf.toMap
    for e <- pCtx.assumptions do slv.assertFormula(encodeExpr(e, ctx))
    slv.assertFormula(encodeExpr(goal, ctx).notTerm)
    (slv, ctx)

  def prove(goal: Expr)(using pCtx: PrfCtx): SolverResult =
    val (slv, ctx) = create(goal)
    val result = slv.checkSat()
    if result.isUnsat then Valid
    else if result.isSat then
      val vars = (goal :: pCtx.assumptions).flatMap(_.collectVars)
      val terms = Array.from(for x <- vars yield ctx(x))
      val values = for x <- vars yield x + " = " + slv.getValue(ctx(x)).toString
      Invalid(values.mkString("\n"))
    else Invalid("solver error: " + result.getUnknownExplanation.toString)

  def canProve(lemma: Expr)(using ctx: PrfCtx): Boolean =
    prove(lemma) match
      case SolverResult.Valid => true
      case _ => false

  private def encodeType(typ: Type): SMTSort = typ.toSort match
    case Sort.Top => assert(false)
    case Sort.Bot => assert(false)
    case Sort.Int => smt.getIntegerSort
    case Sort.Bool => smt.getBooleanSort
    case Sort.String => smt.getStringSort
    case Sort.Tuple(ts) => smt.mkTupleSort(ts.map(encodeType(_)).toArray)
    case Sort.Array(t) => smt.mkArraySort(smt.getIntegerSort, encodeType(t))
    case Sort.Fun(ts, t) => smt.mkFunctionSort(ts.map(encodeType(_)).toArray, encodeType(t))

  import RegExpr.*

  private def encodeRegExpr(re: RegExpr): Term = re match
    case RENone => smt.mkTerm(Kind.REGEXP_NONE)
    case RENull => smt.mkTerm(Kind.STRING_TO_REGEXP, smt.mkString(""))
    case REChar(cs) =>
      if cs.isFull then smt.mkTerm(Kind.REGEXP_ALLCHAR)
      else
        val t =
          if cs.chars.size == 1 then encodeRegExprChar(cs.chars.head)
          else smt.mkTerm(Kind.REGEXP_UNION, cs.chars.map(encodeRegExprChar).toArray)
        if cs.polarity then t else smt.mkTerm(Kind.REGEXP_DIFF, smt.mkTerm(Kind.REGEXP_ALLCHAR), t)
    case REConcat(r1, r2) =>
      val t1 = encodeRegExpr(r1)
      val t2 = encodeRegExpr(r2)
      smt.mkTerm(Kind.REGEXP_CONCAT, t1, t2)
    case REUnion(r1, r2) =>
      val t1 = encodeRegExpr(r1)
      val t2 = encodeRegExpr(r2)
      smt.mkTerm(Kind.REGEXP_UNION, t1, t2)
    case RELoop(range, r) =>
      val t = encodeRegExpr(r)
      val m1 = range.lower
      range.upper match
        case Some(m2) => smt.mkTerm(smt.mkOp(Kind.REGEXP_LOOP, m1, m2), t)
        case None => // r^m1 r*
          smt.mkTerm(Kind.REGEXP_CONCAT,
            smt.mkTerm(smt.mkOp(Kind.REGEXP_REPEAT, m1), t), smt.mkTerm(Kind.REGEXP_STAR, t))

  private inline def encodeRegExprChar(char: Char): Term =
    smt.mkTerm(Kind.STRING_TO_REGEXP, smt.mkString(char.toString))

  private type Ctx = Map[String, Term]

  private def encodeExpr(expr: Expr, ctx: Ctx): Term = expr.accept(Encoder, ctx)

  private object Encoder extends ExprVisitor[Ctx, Term]:
    given Conversion[Int, Term] = smt.mkInteger

    given Conversion[Boolean, Term] = smt.mkBoolean

    given Conversion[String, Term] = smt.mkString

    override def visitConst(node: Const, ctx: Ctx): Term =
      node.value match
        case i: Int => i
        case b: Boolean => b
        case s: String => s

    override def visitVar(node: Var, ctx: Ctx): Term = ctx(node.name)

    override def visitTupleExpr(node: TupleExpr, ctx: Ctx): Term =
      val terms = for elem <- node.elems yield elem.accept(this, ctx)
      smt.mkTuple(terms.toArray)

    override def visitTypeTest(node: TypeTest, ctx: Ctx): Term =
      if config.extractTo.isDefined || config.smtOnly then
        node.typ match
          case LangType(r) =>
            val t = node.value.accept(this, ctx)
            smt.mkTerm(Kind.STRING_IN_REGEXP, t, encodeRegExpr(r))
          case TupleType(ts) if ts.forall(_.isInstanceOf[LangType]) =>
            val rs = ts.map(_.asInstanceOf[LangType].re)
            node.value match
              case TupleExpr(es) if es.length == ts.length =>
                val tests = for i <- rs.indices yield
                  val ei = es(i).accept(this, ctx)
                  smt.mkTerm(Kind.STRING_IN_REGEXP, ei, encodeRegExpr(rs(i)))
                tests.reduce(_.andTerm(_))
              case _ =>
                val t = node.value.accept(this, ctx)
                val dt = t.getSort.getDatatype
                val tests = for i <- rs.indices yield
                  val ti = smt.mkTerm(Kind.APPLY_SELECTOR, dt.getConstructor(0).getSelector(i).getTerm, t)
                  smt.mkTerm(Kind.STRING_IN_REGEXP, ti, encodeRegExpr(rs(i)))
                tests.reduce(_.andTerm(_))
          case _ =>
            throw UnsupportedOperationException(node.toString)
      else
        smt.mkConst(smt.getBooleanSort)

    override def visitAnd(node: And, ctx: Ctx): Term =
      val b1 = node.left.accept(this, ctx)
      val b2 = node.right.accept(this, ctx)
      smt.mkTerm(Kind.AND, b1, b2)

    override def visitOr(node: Or, ctx: Ctx): Term =
      val b1 = node.left.accept(this, ctx)
      val b2 = node.right.accept(this, ctx)
      smt.mkTerm(Kind.OR, b1, b2)

    override def visitNot(node: Not, ctx: Ctx): Term =
      val b = node.operand.accept(this, ctx)
      smt.mkTerm(Kind.NOT, b)

    override def visitIte(node: Ite, ctx: Ctx): Term =
      val b = node.test.accept(this, ctx)
      val e1 = node.thenValue.accept(this, ctx)
      val e2 = node.elseValue.accept(this, ctx)
      smt.mkTerm(Kind.ITE, b, e1, e2)

    override def visitCmp(node: Cmp, ctx: Ctx): Term =
      val i1 = node.left.accept(this, ctx)
      val i2 = node.right.accept(this, ctx)
      val kind = node.op match
        case CmpOp.EQ => Kind.EQUAL
        case CmpOp.NE => Kind.DISTINCT
        case CmpOp.LE => Kind.LEQ
        case CmpOp.LT => Kind.LT
        case CmpOp.GE => Kind.GEQ
        case CmpOp.GT => Kind.GT
      smt.mkTerm(kind, i1, i2)

    override def visitNegate(node: Negate, ctx: Ctx): Term =
      val x = node.value.accept(this, ctx)
      smt.mkTerm(Kind.SUB, 0, x)

    override def visitArith(node: Arith, ctx: Ctx): Term =
      val x = node.left.accept(this, ctx)
      val y = node.right.accept(this, ctx)
      val kind = node.op match
        case ArithOp.ADD => Kind.ADD
        case ArithOp.SUB => Kind.SUB
      smt.mkTerm(kind, x, y)

    override def visitStrConcat(node: StrConcat, ctx: Ctx): Term =
      val s1 = node.left.accept(this, ctx)
      val s2 = node.right.accept(this, ctx)
      smt.mkTerm(Kind.STRING_CONCAT, s1, s2)

    override def visitStrLen(node: StrLen, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      smt.mkTerm(Kind.STRING_LENGTH, s)

    override def visitStrAt(node: StrAt, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val i = node.index.accept(this, ctx)
      // If the index `i` is negative or greater than the length of the string `s`, the result is the empty string.
      smt.mkTerm(Kind.STRING_CHARAT, s, i)

    override def visitStrStartsWith(node: StrStartsWith, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val t = node.prefix.accept(this, ctx)
      smt.mkTerm(Kind.STRING_PREFIX, t, s)

    override def visitStrEndsWith(node: StrEndsWith, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val t = node.suffix.accept(this, ctx)
      smt.mkTerm(Kind.STRING_SUFFIX, t, s)

    override def visitStrContains(node: StrContains, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val t = node.infix.accept(this, ctx)
      smt.mkTerm(Kind.STRING_CONTAINS, s, t)

    override def visitStrSlice(node: StrSlice, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val i = node.fromIndex.accept(this, ctx)
      val j = node.untilIndex.accept(this, ctx)
      val l = smt.mkTerm(Kind.SUB, j, i)
      // If the start index `i` is negative or greater than the length of the string `s`,
      // or the length `l` is negative, the result is the empty string.
      smt.mkTerm(Kind.STRING_SUBSTR, s, i, l)

    override def visitStrFind(node: StrFind, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      val t = node.target.accept(this, ctx)
      // If the index `i` is negative or greater than the length of string `s`,
      // or the substring `t` does not appear in `s` after index `i`, the result is -1.
      smt.mkTerm(Kind.STRING_INDEXOF, s, t, smt.mkInteger(0))

    override def visitStrSplit(node: StrSplit, ctx: Ctx): Term =
      throw UnsupportedOperationException("smt solver does not support string split")

    override def visitStrRev(node: StrRev, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      smt.mkTerm(Kind.STRING_REV, s)

    override def visitCharToCode(node: StrToCode, ctx: Ctx): Term =
      val s = node.char.accept(this, ctx)
      // If the string does not have length one, return -1.
      smt.mkTerm(Kind.STRING_TO_CODE, s)

    override def visitCharFromCode(node: StrFromCode, ctx: Ctx): Term =
      val i = node.code.accept(this, ctx)
      // If the argument is out-of-bounds, return the empty string.
      smt.mkTerm(Kind.STRING_FROM_CODE, i)

    override def visitStrToInt(node: StrToInt, ctx: Ctx): Term =
      val s = node.str.accept(this, ctx)
      // If the string does not contain an integer or the integer is negative, the operator returns -1.
      smt.mkTerm(Kind.ITE, smt.mkTerm(Kind.STRING_PREFIX, "-", s), // if s.startsWith("-")
        smt.mkTerm(Kind.SUB, 0, smt.mkTerm(Kind.STRING_TO_INT,
          smt.mkTerm(Kind.STRING_SUBSTR, s, 1, smt.mkTerm(Kind.STRING_LENGTH, s)))), // then 0 - (s.drop(1).toInt)
        smt.mkTerm(Kind.STRING_TO_INT, s)) // else s.toInt

    override def visitStrFromInt(node: StrFromInt, ctx: Ctx): Term =
      val i = node.int.accept(this, ctx)
      // If the integer is negative this operator returns the empty string.
      smt.mkTerm(Kind.ITE, smt.mkTerm(Kind.LT, i, 0), // if i < 0
        smt.mkTerm(Kind.STRING_CONCAT, "-",
          smt.mkTerm(Kind.STRING_FROM_INT, smt.mkTerm(Kind.SUB, 0, i))), // then "-" + (0 - i).toString
        smt.mkTerm(Kind.STRING_FROM_INT, i)) // else i.toString

    override def visitArraySelect(node: ArraySelect, ctx: Ctx): Term =
      val a = node.array.accept(this, ctx)
      val i = node.index.accept(this, ctx)
      // Given an index sort `I` and element sort `E`, an array sort `Array I E` is defined for every index `i` in `I`,
      // i.e., a total map from `I` to `E`. Here we simply set `I` to the integer sort.
      smt.mkTerm(Kind.SELECT, a, i)