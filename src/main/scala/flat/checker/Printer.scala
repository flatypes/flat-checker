package flat.checker

import flat.Ops.CmpOp.*
import flat.checker.ast.*
import flat.regex.RegEx
import flat.regex.RegEx.*
import flat.util.renderSubscript
import flat.{checker, regex}
import org.apache.commons.text.StringEscapeUtils.escapeJava

object Printer:
  def ppType(typ: Type): String = typ match
    case TopType => "Any"
    case NoType => "Nothing"
    case IntSort => "Int"
    case BoolSort => "Bool"
    case StrSort => "String"
    case UnitSort => "Unit"
    case TupleSort(ts) =>
      val elems = ts.map(ppType)
      paren(elems.mkString(", "))
    case ArraySort(s) =>
      val elem = ppType(s)
      s"Array[$elem]"
    case FunSort(ts, t) =>
      val ss = ts.map(ppType)
      val left = if ss.length == 1 then ss.head else paren(ss.mkString(", "))
      val right = ppType(t)
      s"$left => $right"
    case LangType(re) => ppRE(re)
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
    case rt: RefinedType =>
      val typ = ppType(rt.typ)
      val cond = ppExpr(rt.cond)
      s"$typ{$cond}"

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

  def ppExpr(expr: Expr): String = expr.accept(ExprPrinter)(using ())

  private object ExprPrinter extends ExprVisitor[Unit, String]:
    def visitConst(node: Const)(using ctx: Unit): String =
      node.value match
        case i: Int => i.toString
        case b: Boolean => b.toString
        case s: String => "\"" + escapeJava(s) + "\""

    def visitGlobalRef(node: GlobalRef)(using ctx: Unit): String = node.name

    def visitVar(node: Var)(using ctx: Unit): String =
      val name = node.name
      if name.contains('@') then
        val Array(x, ver) = name.split('@')
        x + renderSubscript(ver.toInt)
      else name

    def visitTupleExpr(node: TupleExpr)(using ctx: Unit): String =
      val ss = node.elems.map(_.accept(this))
      paren(ss.mkString(", "))

    def visitTypeTest(node: TypeTest)(using ctx: Unit): String =
      val s = node.value.accept(this)
      val s1 = if getLevel(node.value) <= Level.CMP then paren(s) else s
      val s2 = ppType(node.typ)
      s"$s1 ∈ $s2"

    def visitAnd(node: And)(using ctx: Unit): String =
      // Special case: _ op _ op _
      (node.left, node.right) match
        case (Cmp(op1@(LE | LT), e1, e), Cmp(op2@(LE | LT), e3, e2)) if e3 == e =>
          val value = e.accept(this)
          val left = e1.accept(this)
          val right = e2.accept(this)
          return s"$left $op1 $value $op2 $right"
        case _ =>

      // right associative
      val s1 = node.left.accept(this)
      val left = if getLevel(node.left) <= Level.AND then paren(s1) else s1
      val s2 = node.right.accept(this)
      val right = if getLevel(node.right) < Level.AND then paren(s2) else s2
      s"$left && $right"

    def visitOr(node: Or)(using ctx: Unit): String =
      // right associative
      val s1 = node.left.accept(this)
      val left = if getLevel(node.left) <= Level.AND then paren(s1) else s1
      val s2 = node.right.accept(this)
      val right = if getLevel(node.right) < Level.AND then paren(s2) else s2
      s"$left || $right"

    def visitNot(node: Not)(using ctx: Unit): String =
      val s = node.value.accept(this)
      val value = if getLevel(node.value) < Level.UNARY then paren(s) else s
      s"!$value"

    def visitIte(node: Ite)(using ctx: Unit): String =
      val s = node.cond.accept(this)
      val cond = if getLevel(node.cond) <= Level.ITE then paren(s) else s
      val s1 = node.thenValue.accept(this)
      val thenValue = if getLevel(node.thenValue) <= Level.ITE then paren(s1) else s1
      val s2 = node.elseValue.accept(this)
      val elseValue = if getLevel(node.elseValue) < Level.ITE then paren(s2) else s2
      s"if $cond then $thenValue else $elseValue"

    def visitCmp(node: Cmp)(using ctx: Unit): String =
      // not associative
      val s1 = node.left.accept(this)
      val left = if getLevel(node.left) <= Level.CMP then paren(s1) else s1
      val s2 = node.right.accept(this)
      val right = if getLevel(node.right) <= Level.CMP then paren(s2) else s2
      s"$left ${node.op} $right"

    def visitNegate(node: Negate)(using ctx: Unit): String =
      val s = node.value.accept(this)
      val value = if getLevel(node.value) < Level.UNARY then paren(s) else s
      s"-$value"

    def visitArith(node: Arith)(using ctx: Unit): String =
      // left associative
      val s1 = node.left.accept(this)
      val left = if getLevel(node.left) < Level.CMP then paren(s1) else s1
      val s2 = node.right.accept(this)
      val right = if getLevel(node.right) <= Level.CMP then paren(s2) else s2
      s"$left ${node.op} $right"

    def visitConcat(node: Concat)(using ctx: Unit): String =
      // right associative
      val s1 = node.left.accept(this)
      val left = if getLevel(node.left) <= Level.CMP then paren(s1) else s1
      val s2 = node.right.accept(this)
      val right = if getLevel(node.right) < Level.CMP then paren(s2) else s2
      s"$left ⧺ $right"

    def visitLength(node: Length)(using ctx: Unit): String =
      val str = node.str.accept(this)
      s"|$str|"

    def visitCharAt(node: CharAt)(using ctx: Unit): String =
      val s = node.str.accept(this)
      val str = if getLevel(node.str) < Level.APPLY then paren(s) else s
      val idx = node.idx.accept(this)
      s"$str[$idx]"

    def visitSubstr(node: Substr)(using ctx: Unit): String =
      val s = node.str.accept(this)
      val str = if getLevel(node.str) < Level.APPLY then paren(s) else s
      val startIdx = if node.startIdx == Const(0) then "" else node.startIdx.accept(this)
      val endIdx = if node.endIdx == Length(node.str) then "" else node.endIdx.accept(this)
      s"$str[$startIdx:$endIdx]"

    def visitPrefixOf(node: PrefixOf)(using ctx: Unit): String =
      val s1 = node.prefix.accept(this)
      val prefix = if getLevel(node.prefix) <= Level.CMP then paren(s1) else s1
      val s2 = node.str.accept(this)
      val str = if getLevel(node.str) <= Level.CMP then paren(s2) else s2
      s"$prefix ⊑ $str"

    def visitSuffixOf(node: SuffixOf)(using ctx: Unit): String =
      val s1 = node.suffix.accept(this)
      val suffix = if getLevel(node.suffix) <= Level.CMP then paren(s1) else s1
      val s2 = node.str.accept(this)
      val str = if getLevel(node.str) <= Level.CMP then paren(s2) else s2
      s"$suffix ⊒ $str"

    def visitInfixOf(node: InfixOf)(using ctx: Unit): String =
      val s1 = node.infix.accept(this)
      val infix = if getLevel(node.infix) <= Level.CMP then paren(s1) else s1
      val s2 = node.str.accept(this)
      val str = if getLevel(node.str) <= Level.CMP then paren(s2) else s2
      s"$infix in $str"

    def visitFind(node: Find)(using ctx: Unit): String =
      val s = node.str.accept(this)
      val str = if getLevel(node.str) < Level.APPLY then paren(s) else s
      val pat = node.pat.accept(this)
      s"$str.find($pat)"

    def visitSplit(node: Split)(using ctx: Unit): String =
      val s = node.str.accept(this)
      val str = if getLevel(node.str) < Level.APPLY then paren(s) else s
      val sep = node.sep.accept(this)
      s"$str.split($sep)"

    def visitReverse(node: Reverse)(using ctx: Unit): String =
      val str = node.str.accept(this)
      s"rev($str)"

    def visitStrToCode(node: StrToCode)(using ctx: Unit): String =
      val chr = node.chr.accept(this)
      s"toCode($chr)"

    def visitStrFromCode(node: StrFromCode)(using ctx: Unit): String =
      val code = node.code.accept(this)
      s"fromCode($code)"

    def visitStrToInt(node: StrToInt)(using ctx: Unit): String =
      val str = node.str.accept(this)
      s"toInt($str)"

    def visitStrFromInt(node: StrFromInt)(using ctx: Unit): String =
      val int = node.int.accept(this)
      s"fromInt($int)"

    def visitStrIn(node: StrIn)(using ctx: Unit): String =
      val s = node.str.accept(this)
      val str = if getLevel(node.str) < Level.CMP then paren(s) else s
      val re = ppRE(node.re)
      s"$str ∈ $re"

    def visitArrSelect(node: ArrSelect)(using ctx: Unit): String =
      val s = node.arr.accept(this)
      val arr = if getLevel(node.arr) < Level.APPLY then paren(s) else s
      val idx = node.idx.accept(this)
      s"$arr[$idx]"

    def visitDictExpr(node: DictExpr)(using ctx: Unit): String =
      if node.items.isEmpty then "{}"
      else
        val ss = node.items.take(3).map: (key, value) =>
          val ks = key.accept(this)
          val vs = value.accept(this)
          s"$ks: $vs"
        "{" + ss.mkString(", ") + (if node.items.length > 3 then ", ..." else "") + "}"

    def visitDictContainsKey(node: DictContainsKey)(using ctx: Unit): String =
      val s1 = node.key.accept(this)
      val key = if getLevel(node.key) <= Level.CMP then paren(s1) else s1
      val s2 = node.dict.accept(this)
      val dict = if getLevel(node.dict) <= Level.CMP then paren(s2) else s2
      s"$key in $dict"

    def visitDictSelect(node: DictSelect)(using ctx: Unit): String =
      val s = node.dict.accept(this)
      val dict = if getLevel(node.dict) < Level.APPLY then paren(s) else s
      val key = node.key.accept(this)
      s"$dict[$key]"

    def visitApply(node: Apply)(using ctx: Unit): String =
      val s = node.fun.accept(this)
      val fun = if getLevel(node.fun) < Level.APPLY then paren(s) else s
      val args = node.args.map(_.accept(this))
      fun + paren(args.mkString(", "))

    private object Level extends Enumeration:
      val ITE, OR, AND, CMP, ADD, UNARY, APPLY = Value

    private def getLevel(expr: Expr): Level.Value = expr match
      case _: Ite => Level.ITE
      case _: Or => Level.OR
      case _: And => Level.AND
      case _: Cmp | _: StrTest | _: TypeTest | _: DictContainsKey => Level.CMP
      case _: Arith | _: Concat => Level.ADD
      case _: Not | _: Negate => Level.UNARY
      case _ => Level.APPLY

  def ppStmt(stmt: Stmt): String = stmt.accept(StmtPrinter)(using 0)

  private object StmtPrinter extends StmtVisitor[Int, String]:
    def visitSkip(node: Skip)(using level: Int): String =
      ("  " * level) + "skip" + "\n"

    def visitSeqStmt(node: SeqStmt)(using level: Int): String =
      val first = node.first.accept(this)
      val second = node.second.accept(this)
      first + second

    def visitAssign(node: Assign)(using level: Int): String =
      val value = ppExpr(node.value)
      ("  " * level) + s"${node.id} = $value" + "\n"

    def visitAssert(node: Assert)(using level: Int): String =
      val cond = ppExpr(node.cond)
      ("  " * level) + s"assert $cond" + "\n"

    def visitAssume(node: Assume)(using level: Int): String =
      val cond = ppExpr(node.cond)
      ("  " * level) + s"assume $cond" + "\n"

    def visitShowType(node: ShowType)(using level: Int): String =
      val value = ppExpr(node.value)
      ("  " * level) + s"infer $value" + "\n"

    def visitIfStmt(node: IfStmt)(using level: Int): String =
      val cond = ppExpr(node.cond)
      val thenBody = node.thenBody.accept(this)(using level + 1)
      val elsePart = node.elseBody match
        case Skip() => ""
        case s => "else" + "\n" + s.accept(this)(using level + 1)
      ("  " * level) + s"if $cond then" + "\n" + thenBody + elsePart

    def visitWhile(node: While)(using level: Int): String =
      val cond = ppExpr(node.cond)
      val invariants = node.invariants.map(e => ("  " * (level + 1)) + s"inv ${ppExpr(e)}" + "\n").mkString
      val body = node.body.accept(this)(using level + 1)
      ("  " * level) + s"while $cond do" + "\n" + invariants + body

    def visitBreak(node: Break)(using level: Int): String =
      ("  " * level) + "break" + "\n"

    def visitReturn(node: Return)(using level: Int): String =
      ("  " * level) + "return" + "\n"
