package flat.checker

import flat.checker.VC.*
import flat.checker.ast.*
import flat.regex.RegEx
import flat.regex.RegEx.*
import flat.util.renderSubscript
import flat.{checker, regex}
import org.apache.commons.text.StringEscapeUtils.escapeJava

object Printer:
  def ppType(typ: Type): String = typ match
    case AnyType => "Any"
    case NoType => "Nothing"
    case IntType => "Int"
    case BoolType => "Bool"
    case LangType(r) => ppRE(r)
    case TupleType(ts) =>
      val ss = ts.map(ppType)
      paren(ss.mkString(", "))
    case ArrayType(t) =>
      val s = ppType(t)
      s"Array[$s]"
    case FunType(ts, t) =>
      val ss = ts.map(ppType)
      val left = if ss.length == 1 then ss.head else paren(ss.mkString(", "))
      val right = ppType(t)
      s"$left => $right"

  private inline def paren(s: String): String = "(" + s + ")"

  def ppRE(re: RegEx): String = re match
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
      val s1 = if r1.isInstanceOf[REUnion] then paren(ppRE(r1)) else ppRE(r1)
      val s2 = if r2.isInstanceOf[REUnion] then paren(ppRE(r2)) else ppRE(r2)
      s1 + s2
    case REUnion(r1, r2) => // right associative
      val s1 = if r1.isInstanceOf[REUnion] then paren(ppRE(r1)) else ppRE(r1)
      val s2 = ppRE(r2)
      s"$s1|$s2"
    case REStar(r) =>
      val s = ppRE(r)
      if s.length == 1 then s + "*" else paren(s) + "*"

  def ppExpr(expr: Expr): String = expr match
    case Const(i: Int) => i.toString
    case Const(b: Boolean) => b.toString
    case Const(s: String) => "\"" + escapeJava(s) + "\""
    case GlobalRef(name) => name
    case Var(name) =>
      if name.contains('@') then
        val Array(x, ver) = name.split('@')
        x + renderSubscript(ver.toInt)
      else name
    case TupleExpr(es) =>
      val ss = es.map(ppExpr)
      paren(ss.mkString(", "))
    case TypeTest(e, t) =>
      val s1 = if getLevel(e) <= Level.CMP then paren(ppExpr(e)) else ppExpr(e)
      val s2 = ppType(t)
      s"$s1 ∈ $s2"
    case And(e1, e2) => // right associative
      val s1 = if getLevel(e1) <= Level.AND then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) < Level.AND then paren(ppExpr(e2)) else ppExpr(e2)
      s"$s1 && $s2"
    case Or(e1, e2) => // right associative
      val s1 = if getLevel(e1) <= Level.OR then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) < Level.OR then paren(ppExpr(e2)) else ppExpr(e2)
      s"$s1 || $s2"
    case Not(e) =>
      val s = if getLevel(e) < Level.UNARY then paren(ppExpr(e)) else ppExpr(e)
      s"!$s"
    case Ite(e, e1, e2) =>
      val s = if getLevel(e) <= Level.ITE then paren(ppExpr(e)) else ppExpr(e)
      val s1 = if getLevel(e1) <= Level.ITE then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) < Level.ITE then paren(ppExpr(e2)) else ppExpr(e2)
      s"if $s then $s1 else $s2"
    case Cmp(op, e1, e2) => // not associative
      val s1 = if getLevel(e1) <= Level.CMP then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) <= Level.CMP then paren(ppExpr(e2)) else ppExpr(e2)
      s"$s1 $op $s2"
    case Negate(e) =>
      val s = if getLevel(e) < Level.UNARY then paren(ppExpr(e)) else ppExpr(e)
      s"-$s"
    case Arith(op, e1, e2) => // left associative
      val s1 = if getLevel(e1) < Level.CMP then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) <= Level.CMP then paren(ppExpr(e2)) else ppExpr(e2)
      s"$s1 $op $s2"
    case Concat(e1, e2) => // right associative
      val s1 = if getLevel(e1) <= Level.CMP then paren(ppExpr(e1)) else ppExpr(e1)
      val s2 = if getLevel(e2) < Level.CMP then paren(ppExpr(e2)) else ppExpr(e2)
      s"$s1 ⧺ $s2"
    case Length(e) =>
      val s = ppExpr(e)
      s"|$s|"
    case CharAt(e, ei) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val si = ppExpr(ei)
      s"$s[$si]"
    case Substr(e, ei, ej) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val si = if ei == Const(0) then "" else ppExpr(ei)
      val sj = if ej == Length(e) then "" else ppExpr(ej)
      s"$s[$si:$sj]"
    case PrefixOf(et, e) =>
      val s1 = if getLevel(et) <= Level.CMP then paren(ppExpr(et)) else ppExpr(et)
      val s2 = if getLevel(e) <= Level.CMP then paren(ppExpr(e)) else ppExpr(e)
      s"$s1 ⊑ $s2"
    case SuffixOf(et, e) =>
      val s1 = if getLevel(et) <= Level.CMP then paren(ppExpr(et)) else ppExpr(et)
      val s2 = if getLevel(e) <= Level.CMP then paren(ppExpr(e)) else ppExpr(e)
      s"$s1 ⊒ $s2"
    case InfixOf(et, e) =>
      val s1 = if getLevel(et) <= Level.CMP then paren(ppExpr(et)) else ppExpr(et)
      val s2 = if getLevel(e) <= Level.CMP then paren(ppExpr(e)) else ppExpr(e)
      s"$s1 in $s2"
    case Find(e, et) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val st = ppExpr(et)
      s"$s.find($st)"
    case Split(e, et) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val st = ppExpr(et)
      s"$s.split($st)"
    case Reverse(e) =>
      val s = ppExpr(e)
      s"rev($s)"
    case StrToCode(e) =>
      val s = ppExpr(e)
      s"toCode($s)"
    case StrFromCode(e) =>
      val s = ppExpr(e)
      s"fromCode($s)"
    case StrToInt(e) =>
      val s = ppExpr(e)
      s"toInt($s)"
    case StrFromInt(e) =>
      val s = ppExpr(e)
      s"fromInt($s)"
    case ArraySelect(e, ei) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val si = ppExpr(ei)
      s"$s[$si]"
    case Apply(e, es) =>
      val s = if getLevel(e) < Level.APPLY then paren(ppExpr(e)) else ppExpr(e)
      val ss = es.map(ppExpr)
      s + paren(ss.mkString(", "))

  private object Level extends Enumeration:
    val ITE, OR, AND, CMP, ADD, UNARY, APPLY = Value

  private def getLevel(expr: Expr): Level.Value = expr match
    case _: Ite => Level.ITE
    case _: Or => Level.OR
    case _: And => Level.AND
    case _: Cmp | _: StrTest | _: TypeTest => Level.CMP
    case _: Arith | _: Concat => Level.ADD
    case _: Not | _: Negate => Level.UNARY
    case _ => Level.APPLY

  def ppProgram(program: Program): String = program.body.map(ppStmt(_)).mkString

  private def ppStmt(stmt: Stmt, indentLevel: Int = 0): String =
    val indents = "  " * indentLevel
    stmt match
      case Assign(x, e) =>
        val s = ppExpr(e)
        indents + s"$x = $s" + "\n"
      case Assert(e) =>
        val s = ppExpr(e)
        indents + s"assert $s" + "\n"
      case Return() =>
        indents + "return" + "\n"
      case IfStmt(e, b1, b2) =>
        val s = ppExpr(e)
        val ss1 = b1.map(ppStmt(_, indentLevel + 1))
        val thenPart = (indents + s"if $s then" + "\n" :: ss1).mkString
        val ss2 = b2.map(ppStmt(_, indentLevel + 1))
        val elsePart = if ss2.isEmpty then "" else (indents + "else" + "\n" :: ss2).mkString
        thenPart + elsePart
      case While(e, b, es) =>
        val s = ppExpr(e)
        val inv = es.map(e => indents + "  " + s"inv ${ppExpr(e)}" + "\n").mkString
        val body = b.map(ppStmt(_, indentLevel + 1)).mkString
        indents + s"while $s do" + "\n" + inv + body
      case Break() =>
        indents + "break" + "\n"
      case ShowType(e) =>
        val s = ppExpr(e)
        indents + s"infer $s" + "\n"

  def ppVC(vc: VC): String = vc match
    case True => "⊤"
    case HasType(e, t, _) =>
      val se = ppExpr(e)
      val st = ppType(t)
      s"$se ∈ $st"
    case InferType(e, _) =>
      val se = ppExpr(e)
      s"$se ∈ ?"
    case Goal(e, _) => ppExpr(e)
    case LAnd(vc1, vc2) =>
      val s1 = if vc1.isInstanceOf[LImp] then paren(ppVC(vc1)) else ppVC(vc1)
      val s2 = if vc2.isInstanceOf[LImp] then paren(ppVC(vc2)) else ppVC(vc2)
      s"$s1 ∧ $s2"
    case LImp(e, vc) =>
      val s1 = ppExpr(e)
      val s2 = ppVC(vc)
      s"$s1 ⇒ $s2"

  def ppCtx(ctx: PrfCtx): String =
    val ss = ctx.premises.map(ppExpr)
    if ss.isEmpty then "⊤" else ss.mkString(" ∧ ")