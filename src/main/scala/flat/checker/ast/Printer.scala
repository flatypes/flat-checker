package flat.checker.ast

import flat.checker
import flat.regex.*
import org.apache.commons.text.StringEscapeUtils.escapeJava

object Printer:
  def ppProgram(program: Module): String =
    program.body.map(ppGlobalStmt).mkString("\n")

  private def ppGlobalStmt(stmt: GlobalStmt): String = stmt match
    case FunDef(f, params, returnParams, requires, ensures, locals, body) =>
      val sig = formatLine(s"def $f${ppParamGroup(params)} returns ${ppParamGroup(returnParams)}", 0)
      val pre = formatLine(s"requires ${ppExpr(requires)}", 1)
      val post = formatLine(s"ensures ${ppExpr(ensures)}", 1)
      val local = locals.map(d => formatLine(s"var ${ppDecl(d)}", 1)).mkString
      sig + pre + post + "begin\n" + local + ppStmtBody(body, 1) + "end\n"

  private def formatLine(s: String, level: Int): String = "  " * level + s + "\n"

  private def ppParamGroup(params: List[Decl]): String =
    "(" + params.map(ppDecl).mkString(", ") + ")"

  private def ppDecl(decl: Decl): String =
    s"${decl.name}: ${ppSort(decl.sort)}"

  def ppStmt(stmt: Stmt, level: Int = 0): String = stmt match
    case Assign(x, e) => formatLine(s"$x = ${ppExpr(e)}", level)
    case Havoc(xs) => formatLine(s"havoc ${xs.mkString(", ")}", level)
    case Assert(e) => formatLine(s"assert ${ppExpr(e)}", level)
    case Assume(e) => formatLine(s"assume ${ppExpr(e)}", level)
    case ShowType(e) => formatLine(s"show-type ${ppExpr(e)}", level)
    case IfStmt(e, thenBody, elseBody) =>
      formatLine(s"if ${ppExpr(e)} then", level) + ppStmtBody(thenBody, level + 1) + ppElse(elseBody, level)
    case While(e, body) =>
      formatLine(s"while ${ppExpr(e)} do", level) + ppStmtBody(body, level + 1)
    case Break() => formatLine("break", level)
    case Continue() => formatLine("continue", level)
    case Return() => formatLine("return", level)

  def ppStmtBody(body: List[Stmt], level: Int = 0): String =
    body.map(ppStmt(_, level)).mkString

  private def ppElse(body: List[Stmt], level: Int): String = body match
    case List(IfStmt(e, thenBody, elseBody)) =>
      formatLine(s"else if ${ppExpr(e)} then", level) + ppStmtBody(thenBody, level + 1) + ppElse(elseBody, level)
    case _ => formatLine("else", level) + ppStmtBody(body, level + 1)

  type FreshInfo = Map[String, (String, Int)]

  def ppExpr(expr: Expr)(using freshNames: FreshInfo = Map.empty): String = renderExpr(expr)(using freshNames)._1

  private def ppExprSeq(exprs: List[Expr])(using freshNames: FreshInfo): String =
    exprs.map(ppExpr(_)).mkString(", ")

  private object Precedence extends Enumeration:
    type Precedence = Value
    val Lowest, LogicalOR, LogicalAND, BitwiseOR, BitwiseXOR, BitwiseAND, Equality, Relational, BitwiseShift,
    Additive, Multiplicative, Prefix, Highest = Value

  import Precedence.*

  private type Rendered = (String, Precedence)

  private def renderExpr(expr: Expr)(using freshNames: FreshInfo): Rendered = expr match
    case Const(n: Int) => (n.toString, Highest)
    case Const(b: Boolean) => (b.toString, Highest)
    case Const(c: Char) => ("'" + escapeJava(c.toString) + "'", Highest)
    case Const(s: String) => ("\"" + escapeJava(s) + "\"", Highest)
    case FreshVar(i) =>
      freshNames.get(i) match
        case Some((x, k)) => (x + flat.util.renderSubscript(k), Highest)
        case None => (s"?$i", Highest)
    case Var(x) => (x, Highest)
    case Global(decl) => (decl.name, Highest)
    case Lambda(params, e) => (s"λ ${ppParamGroup(params)}, ${ppExpr(e)}", Lowest)
    case Apply(e, es) => renderApply(renderExpr(e), ppExprSeq(es))

    // special operations
    case RefinedBy(e, domain) => renderInfixL("∈", Relational, renderExpr(e), (ppDomain(domain), Highest))
    case Ite(e, e1, e2) =>
      val (s, condLevel) = renderExpr(e)
      val cond = if condLevel == Lowest then "(" + s + ")" else s
      val (s1, thenLevel) = renderExpr(e1)
      val thenValue = if thenLevel == Lowest then "(" + s1 + ")" else s1
      val (elseValue, _) = renderExpr(e2)
      (s"if $cond then $thenValue else $elseValue", Lowest)
    case Forall(params, e) => (s"∀ ${ppParamGroup(params)}, ${ppExpr(e)}", Lowest)
    case Exists(params, e) => (s"∃ ${ppParamGroup(params)}, ${ppExpr(e)}", Lowest)

    // prefix operations
    case Not(e) => renderPrefix("¬", renderExpr(e))
    case Negate(e) => renderPrefix("-", renderExpr(e))

    // infixL operations
    case RelExpr(op, e1, e2) => renderInfixL(op.toString, Relational, renderExpr(e1), renderExpr(e2))
    case Add(e1, e2) => renderInfixL("+", Additive, renderExpr(e1), renderExpr(e2))
    case Sub(e1, e2) => renderInfixL("-", Additive, renderExpr(e1), renderExpr(e2))
    case Mul(e1, e2) => renderInfixL("*", Multiplicative, renderExpr(e1), renderExpr(e2))
    case BitAnd(e1, e2) => renderInfixL("&", BitwiseAND, renderExpr(e1), renderExpr(e2))
    case BitOr(e1, e2) => renderInfixL("|", BitwiseOR, renderExpr(e1), renderExpr(e2))
    case BitXor(e1, e2) => renderInfixL("^", BitwiseXOR, renderExpr(e1), renderExpr(e2))
    case BitShL(e1, e2) => renderInfixL("<<", BitwiseShift, renderExpr(e1), renderExpr(e2))
    case BitShR(e1, e2) => renderInfixL(">>", BitwiseShift, renderExpr(e1), renderExpr(e2))
    case SetIntersect(e1, e2) => renderInfixL("∩", BitwiseAND, renderExpr(e1), renderExpr(e2))
    case SetUnion(e1, e2) => renderInfixL("∪", BitwiseOR, renderExpr(e1), renderExpr(e2))
    case SetMinus(e1, e2) => renderInfixL("-", Additive, renderExpr(e1), renderExpr(e2))

    // infixR operations
    case StringConcat(e1, e2) => renderInfixR("⧺", Additive, renderExpr(e1), renderExpr(e2))
    case SeqConcat(e1, e2) => renderInfixR("⧺", Additive, renderExpr(e1), renderExpr(e2))

    // infix chained operations
    case And(bs) => renderInfixChained("∧", LogicalAND, bs.map(renderExpr))
    case Or(bs) => renderInfixChained("∨", LogicalOR, bs.map(renderExpr))

    // slice expressions
    case CharAt(e, e1) => renderSlice(renderExpr(e), ppExpr(e1))
    case Substring(e, e1, e2) => renderSlice(renderExpr(e), ppExpr(e1), ppExpr(e2))
    case SeqGet(e, e1) => renderSlice(renderExpr(e), ppExpr(e1))
    case SeqSlice(e, e1, e2) => renderSlice(renderExpr(e), ppExpr(e1), ppExpr(e2))
    case MapGet(e, e1) => renderSlice(renderExpr(e), ppExpr(e1))

    // length operations
    case StringLength(e) => ("|" + ppExpr(e) + "|", Highest)
    case SeqLength(e) => ("|" + ppExpr(e) + "|", Highest)
    case SetSize(e) => ("|" + ppExpr(e) + "|", Highest)
    case MapSize(e) => ("|" + ppExpr(e) + "|", Highest)

    // constructor expressions
    case TupleOf(es) => ("(" + ppExprSeq(es) + ")", Highest)
    case SeqOf(es) => ("[" + ppExprSeq(es) + "]", Highest)
    case SetOf(es) => ("{" + ppExprSeq(es) + "}", Highest)
    case MapOf(es) =>
      val items = es.map:
        case TupleOf(List(e1, e2)) => s"${ppExpr(e1)}: ${ppExpr(e2)}"
        case e => ppExpr(e)
      ("{" + items.mkString(", ") + "}", Highest)

    // other operations
    case _ => tryExtractMember(expr.productPrefix) match
      case Some(member) =>
        val es = expr.productIterator.toList.asInstanceOf[List[Expr]]
        val attribute = renderSelect(renderExpr(es.head), member)
        if es.tail.isEmpty then attribute else renderApply(attribute, ppExprSeq(es.tail))
      case None =>
        val args = expr.productIterator.toList.map:
          case e: Expr => ppExpr(e)
          case other => other.toString
        renderApply((expr.productPrefix, Highest), args.mkString(", "))

  private def renderApply(funArg: Rendered, arg: String): Rendered =
    val (f, funLevel) = funArg
    val fun = if funLevel < Highest then "(" + f + ")" else f
    (s"$fun($arg)", Highest)

  private def renderPrefix(op: String, arg: Rendered): Rendered =
    val (s, argLevel) = arg
    val operand = if argLevel < Prefix then "(" + s + ")" else s
    (s"$op$operand", Prefix)

  private def renderInfixL(op: String, level: Precedence, leftArg: Rendered, rightArg: Rendered): Rendered =
    val (s1, leftLevel) = leftArg
    val left = if leftLevel < level then "(" + s1 + ")" else s1
    val (s2, rightLevel) = rightArg
    val right = if rightLevel <= level then "(" + s2 + ")" else s2
    (s"$left $op $right", level)

  private def renderInfixR(op: String, level: Precedence, leftArg: Rendered, rightArg: Rendered): Rendered =
    val (s1, leftLevel) = leftArg
    val left = if leftLevel <= level then "(" + s1 + ")" else s1
    val (s2, rightLevel) = rightArg
    val right = if rightLevel < level then "(" + s2 + ")" else s2
    (s"$left $op $right", level)

  private def renderInfixChained(op: String, level: Precedence, args: List[Rendered]): Rendered =
    val ss = args.map { (s, argLevel) => if argLevel < level then "(" + s + ")" else s }
    (ss.mkString(s" $op "), level)

  private def renderSlice(valueArg: Rendered, indices: String*): Rendered =
    val (s, valueLevel) = valueArg
    val value = if valueLevel < Highest then "(" + s + ")" else s
    val index = indices.mkString(":")
    (s"$value[$index]", Highest)

  private def renderSelect(valueArg: Rendered, attr: String): Rendered =
    val (s, valueLevel) = valueArg
    val value = if valueLevel < Highest then "(" + s + ")" else s
    (s"$value.$attr", Highest)

  private val memberCategories = List("Int", "Char", "String", "Tuple", "Seq", "Set", "Map")

  private def tryExtractMember(s: String): Option[String] =
    val i = memberCategories.indexWhere(s.startsWith)
    if i >= 0 then
      val s1 = s.drop(memberCategories(i).length)
      assert(s1.head.isUpper)
      Some(s"${s1.head.toLower}${s1.tail}")
    else
      None

  def ppSort(sort: Sort): String = sort match
    case TopSort => "Top"
    case NoSort => "No"
    case IntSort => "Int"
    case BoolSort => "Bool"
    case CharSort => "Char"
    case StringSort => "String"
    case UnitSort => "Unit"
    case TupleSort(ss) => "(" + ppSortSeq(ss) + ")"
    case SeqSort(s) => s"Seq[${ppSort(s)}]"
    case SetSort(s) => s"Set[${ppSort(s)}]"
    case MapSort(s1, s2) => s"Map[${ppSort(s1)}, ${ppSort(s2)}]"
    case FunSort(ss, s) => s"(${ppSortSeq(ss)}) → ${ppSort(s)}"

  private def ppSortSeq(sorts: List[Sort]): String = sorts.map(ppSort).mkString(", ")

  def ppDomain(domain: Domain): String = domain match
    case r: RegEx => ppRE(r)
    case TopDomain => "Top"
    case ProductDomain(ds) => "(" + ds.map(ppDomain).mkString(", ") + ")"
    case SeqDomain(d) => s"Seq[${ppDomain(d)}]"
    case SetDomain(d) => s"Set[${ppDomain(d)}]"
    case _ => "<OTHER>"

  def ppRE(re: RegEx): String =
    val s = ppREImpl(re)
    if s.length > 80 then s.take(80) + "..." else s

  import RegEx.*

  private def ppREImpl(re: RegEx): String = re match
    case RENone => "∅"
    case RENull => "ε"
    case RELit(cs) =>
      if cs.isEmpty then "∅"
      else if cs.isSingleton then cs.head.toString
      else if cs.isFull then "."
      else
        val (ranges, pos) = cs.toSMT
        val ss = ranges.map:
          case c: Char => c.toString
          case (c1, c2) => s"$c1-$c2"
        "[" + (if pos then "" else "^") + ss.mkString + "]"
    case REConcat(r1, r2) =>
      val s1 = if r1.isInstanceOf[REUnion] then "(" + ppREImpl(r1) + ")" else ppREImpl(r1)
      val s2 = if r2.isInstanceOf[REUnion] then "(" + ppREImpl(r2) + ")" else ppREImpl(r2)
      s1 + s2
    case REUnion(r1, r2) => // right associative
      val s1 = if r1.isInstanceOf[REUnion] then "(" + ppREImpl(r1) + ")" else ppREImpl(r1)
      val s2 = ppREImpl(r2)
      s"$s1|$s2"
    case REStar(r) =>
      val s = ppREImpl(r)
      if s.length == 1 then s + "*" else "(" + s + ")" + "*"
