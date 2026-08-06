package flat.checker.flan

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.flan.tpd.*

object Show extends LazyLogging:
  // Types and sorts
  extension (sort: Sort)
    def show: String = sort match
      case NullSort => "null"
      case BoolSort => "Bool"
      case IntSort => "Int"
      case CharSort => "Char"
      case SeqSort(CharSort) => "String"
      case SeqSort(s) => s"Seq[${s.show}]"
      case SetSort(s) => s"Set[${s.show}]"
      case MapSort(k, v) => s"Map[${k.show}, ${v.show}]"
      case TupleSort(ss) => ss.map(_.show).mkString("(", ", ", ")")
      case FunSort(ss, s) => ss.map(_.show).mkString("(", ", ", ")") + " -> " + s.show
      case UnionSort(s1, s2) => s"${s1.show} | ${s2.show}"
      case NoSort => "?"

  extension (typ: NormType)
    def show: String = typ match
      case NormType(s, None) => s.show
      case NormType(s, Some(e)) => s"${s.show}[${e.show}]"

  extension (varDecl: VarDecl)
    def show: String = s"${varDecl.name}: ${varDecl.typ.show}"

  // Expressions
  val LEVEL_TIGHTER = 0
  val LEVEL_PREFIX = 10
  val LEVEL_MUL = 20
  val LEVEL_ADD = 30
  val LEVEL_REL = 40
  val LEVEL_BIT = 50
  val LEVEL_BIT_AND = 55
  val LEVEL_BIT_OR = 60
  val LEVEL_AND = 70
  val LEVEL_OR = 80
  val LEVEL_ARROW = 90
  val LEVEL_LOOSER = 100

  private def getLevel(expr: Expr): Int = expr match
    case _: Negate | _: Not | _: BitNot => LEVEL_PREFIX
    case _: Mul | _: Div | _: Mod => LEVEL_MUL
    case _: Add | _: Sub | _: SeqConcat | _: SetDiff => LEVEL_ADD
    case _: Eq | _: Ne | _: Le | _: Lt | _: Subset => LEVEL_REL
    case _: BitXor | _: BitShL | _: BitShR => LEVEL_BIT
    case _: BitAnd | _: SetInter => LEVEL_BIT_AND
    case _: BitOr | _: SetUnion => LEVEL_BIT_OR
    case _: And => LEVEL_AND
    case _: Or => LEVEL_OR
    case _: Implies => LEVEL_ARROW
    case _: Ite => LEVEL_LOOSER
    case _ => LEVEL_TIGHTER

  val ASSOC_NONE = 0
  val ASSOC_LEFT = 1
  val ASSOC_RIGHT = 2

  private def showExpr(expr: Expr, paren: Boolean = false): String =
    val s = expr match
      // Constants and variables
      case Const(null) => "null"
      case Const(b: Boolean) => b.toString
      case Const(i: Int) => i.toString
      case Const(c: Char) => s"'${escapeChar(c)}'"
      case Const(s: String) => s"\"${escapeString(s)}\""
      case Var(x) => showVar(x)
      case MethodRef(f) => f

      // Functional
      case Apply(ef, es) => showTighter(ef) + es.map(showExpr(_)).mkString("(", ", ", ")")
      case Lambda(ps, e) => s"λ ${showParamList(ps)}, ${showExpr(e)}"

      // Universal
      case Eq(e1, e2) => showInfix("==", LEVEL_REL, ASSOC_NONE, e1, e2)
      case Ne(e1, e2) => showInfix("≠", LEVEL_REL, ASSOC_NONE, e1, e2)
      case Ite(e, e1, e2) => s"${showExpr(e)} ? ${showExpr(e1)} : ${showExpr(e2)}"

      // Bool
      case And(e1, e2) => showInfix("∧", LEVEL_AND, ASSOC_RIGHT, e1, e2)
      case Or(e1, e2) => showInfix("∨", LEVEL_OR, ASSOC_RIGHT, e1, e2)
      case Not(e) => showPrefix("¬", e)
      case Implies(e1, e2) => showInfix("⇒", LEVEL_ARROW, ASSOC_RIGHT, e1, e2)

      // Int arithmetic and relational
      case Negate(e) => showPrefix("-", e)
      case Add(e1, e2) => showInfix("+", LEVEL_ADD, ASSOC_LEFT, e1, e2)
      case Sub(e1, e2) => showInfix("-", LEVEL_ADD, ASSOC_LEFT, e1, e2)
      case Mul(e1, e2) => showInfix("*", LEVEL_MUL, ASSOC_LEFT, e1, e2)
      case Div(e1, e2) => showInfix("/", LEVEL_MUL, ASSOC_LEFT, e1, e2)
      case Mod(e1, e2) => showInfix("%", LEVEL_MUL, ASSOC_LEFT, e1, e2)
      case Le(e1, e2) => showInfix("≤", LEVEL_REL, ASSOC_NONE, e1, e2)
      case Lt(e1, e2) => showInfix("<", LEVEL_REL, ASSOC_NONE, e1, e2)

      // Int bitwise
      case BitAnd(e1, e2) => showInfix("&", LEVEL_BIT_AND, ASSOC_LEFT, e1, e2)
      case BitOr(e1, e2) => showInfix("|", LEVEL_BIT_OR, ASSOC_LEFT, e1, e2)
      case BitXor(e1, e2) => showInfix("^", LEVEL_BIT, ASSOC_LEFT, e1, e2)
      case BitNot(e) => showPrefix("~", e)
      case BitShL(e1, e2) => showInfix("<<", LEVEL_BIT, ASSOC_LEFT, e1, e2)
      case BitShR(e1, e2) => showInfix(">>", LEVEL_BIT, ASSOC_LEFT, e1, e2)

      // Char
      case CharToInt(e) => showMemberApply(e, "toInt")
      case CharFromInt(e) => showMemberApply(e, "toChar")
      case CharToString(e) => showMemberApply(e, "toString")

      // Seq
      case SeqLit(es) => es.map(showExpr(_)).mkString("[", ", ", "]")
      case SeqLength(e) => s"|${showExpr(e)}|"
      case SeqSelect(e, ei) => s"${showTighter(e)}[${showExpr(ei)}]"
      case SeqUpdate(e, ei, ev) => s"${showTighter(e)}[${showExpr(ei)} = ${showExpr(ev)}]"
      case SeqSlice(e, ei, ej) if ej == NoExpr || ej == SeqLength(e) => s"${showTighter(e)}[${showExpr(ei)}:]"
      case SeqSlice(e, ei, ej) => s"${showTighter(e)}[${showExpr(ei)}:${showExpr(ej)}]"
      case SeqConcat(e1, e2) => showInfix("++", LEVEL_ADD, ASSOC_LEFT, e1, e2)
      case SeqReverse(e) => showMemberApply(e, "reverse")
      case SeqIndexOf(e, et, Const(0)) => showMemberApply(e, "indexOf", et)
      case SeqIndexOf(e, et, ei) => showMemberApply(e, "indexOf", et, ei)
      case SeqContains(e, et) => showMemberApply(e, "contains", et)
      case SeqStartsWith(e, et) => showMemberApply(e, "startsWith", et)
      case SeqEndsWith(e, et) => showMemberApply(e, "endsWith", et)
      case SeqCount(e, ep) => showMemberApply(e, "count", ep)
      case SeqForall(e, ep) => showMemberApply(e, "forall", ep)

      // String
      case StrReplace(e, e1, e2) => showMemberApply(e, "replace", e1, e2)
      case StringSplit(e, et) => showMemberApply(e, "split", et)
      case StringTrim(e) => showMemberApply(e, "trim")
      case StringToLower(e) => showMemberApply(e, "toLower")
      case StringToUpper(e) => showMemberApply(e, "toUpper")
      case StringToInt(e) => showMemberApply(e, "toInt")
      case StrFromInt(e, _) => showMemberApply(e, "toString")
      case StrIsAscii(e) => showMemberApply(e, "isAscii")

      // Set
      case SetLit(es) => es.map(showExpr(_)).mkString("{", ", ", "}")
      case SetSize(e) => s"|${showExpr(e)}|"
      case SetContains(e, et) => showMemberApply(e, "contains", et)
      case Subset(e1, e2) => showInfix("⊆", LEVEL_REL, ASSOC_NONE, e1, e2)
      case SetUnion(e1, e2) => showInfix("∪", LEVEL_BIT_OR, ASSOC_LEFT, e1, e2)
      case SetInter(e1, e2) => showInfix("∩", LEVEL_BIT_AND, ASSOC_LEFT, e1, e2)
      case SetDiff(e1, e2) => showInfix("∖", LEVEL_ADD, ASSOC_LEFT, e1, e2)
      case SetForall(e, ep) => showMemberApply(e, "forall", ep)

      // Map
      case MapLit(eks, evs) =>
        (for (ek, ev) <- (eks zip evs) yield s"${showExpr(ek)}: ${showExpr(ev)}").mkString("{", ", ", "}")
      case MapKeys(e) => showMemberApply(e, "keys")
      case MapValues(e) => showMemberApply(e, "values")
      case MapItems(e) => showMemberApply(e, "items")
      case MapSize(e) => s"|${showExpr(e)}|"
      case MapContains(e, ek) => showMemberApply(e, "contains", ek)
      case MapSelect(e, ek) => s"${showTighter(e)}[${showExpr(ek)}]"
      case MapUpdate(e, ek, ev) => s"${showTighter(e)}[${showExpr(ek)} = ${showExpr(ev)}]"

      // Tuple
      case TupleExpr(es) => es.map(showExpr(_)).mkString("(", ", ", ")")
      case TupleSelect(i, e) => showMemberApply(e, "_" + (i + 1))

      // Domain
      case StringInLang(e, r) => s"${showExpr(e)} ∈ ${r.pp}"

      // Other
      case NoExpr => "?"

    if paren then s"($s)" else s

  private def showVar(name: String): String =
    name.split(':') match
      case Array(x) => x
      case Array(x, ver) if ver.forall(c => '0' <= c && c <= '9') =>
        x + ver.map(c => (c - '0' + '₀').toChar).mkString
      case _ => throw IllegalArgumentException(s"invalid variable name: $name")

  private def showParamList(ps: Seq[VarDecl]): String =
    ps.map(_.show).mkString("(", ", ", ")")

  private def showPrefix(op: String, expr: Expr): String =
    op + showExpr(expr, getLevel(expr) > LEVEL_PREFIX)

  private def showInfix(op: String, level: Int, assoc: Int, left: Expr, right: Expr): String =
    val leftLevel = getLevel(left)
    val rightLevel = getLevel(right)
    val s1 = showExpr(left, leftLevel > level || (leftLevel == level && assoc != ASSOC_LEFT))
    val s2 = showExpr(right, rightLevel > level || (rightLevel == level && assoc != ASSOC_RIGHT))
    s"$s1 $op $s2"

  private def showMemberApply(receiver: Expr, member: String, args: Expr*): String =
    val s1 = showExpr(receiver, getLevel(receiver) > LEVEL_TIGHTER)
    val s2 = if args.isEmpty then "" else args.map(showExpr(_)).mkString("(", ", ", ")")
    s"$s1.$member$s2"

  private def showTighter(expr: Expr): String =
    showExpr(expr, getLevel(expr) > LEVEL_TIGHTER)

  extension (expr: Expr)
    def show: String = showExpr(expr)
