package flat.checker.flan

import flat.checker.flan.tpd.*

object Show:
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

  extension (expr: Expr)
    def show: String = expr match
      case Const(null) => "null"
      case Const(b: Boolean) => b.toString
      case Const(i: Int) => i.toString
      case Const(c: Char) => "'" + escapeChar(c) + "'"
      case Const(s: String) => "\"" + escapeString(s) + "\""
      case Var(x) =>
        x.split(':') match
          case Array(name) => name
          case Array(name, ver) => name + ver.map(subscriptDigits)
          case _ => throw IllegalArgumentException(s"invalid variable name: $x")
      case MethodRef(f) => f
      case Apply(ef, es) => ef.show + es.map(_.show).mkString("(", ", ", ")")
      case Lambda(ps, e) => ps.map(_.show).mkString("(", ", ", ")") + " -> " + e.show
      // Basic
      case Eq(e1, e2) => s"(${e1.show} == ${e2.show})"
      case Ne(e1, e2) => s"(${e1.show} ≠ ${e2.show})"
      case Ite(e, e1, e2) => s"(${e.show} ? ${e1.show} : ${e2.show})"
      // Boolean operations
      case And(e1, e2) => s"(${e1.show} ∧ ${e2.show})"
      case Or(e1, e2) => s"(${e1.show} ∨ ${e2.show})"
      case Not(e) => s"¬(${e.show})"
      case Implies(e1, e2) => s"(${e1.show} ⇒ ${e2.show})"
      // Int operations
      case Negate(e) => s"-${e.show}"
      case Add(e1, e2) => s"(${e1.show} + ${e2.show})"
      case Sub(e1, e2) => s"(${e1.show} - ${e2.show})"
      case Mul(e1, e2) => s"(${e1.show} * ${e2.show})"
      case Le(e1, e2) => s"(${e1.show} ≤ ${e2.show})"
      case Lt(e1, e2) => s"(${e1.show} < ${e2.show})"
      case BitAnd(e1, e2) => s"(${e1.show} & ${e2.show})"
      case BitOr(e1, e2) => s"(${e1.show} | ${e2.show})"
      case BitXor(e1, e2) => s"(${e1.show} ^ ${e2.show})"
      case BitNot(e) => s"~(${e.show})"
      case BitShL(e1, e2) => s"${e1.show} << ${e2.show}"
      case BitShR(e1, e2) => s"${e1.show} >> ${e2.show}"
      // Char operations
      case CharToInt(c) => s"${c.show}.toInt"
      case CharFromInt(i) => s"${i.show}.toChar"
      case CharToString(c) => s"${c.show}.toString"
      // Sequence operations
      case SeqLit(es) => es.map(_.show).mkString("Seq(", ", ", ")")
      case SeqLength(s) => s"${s.show}.length"
      case SeqSelect(s, i) => s"${s.show}[${i.show}]"
      case SeqUpdate(s, i, e) => s"${s.show}.update(${i.show}, ${e.show})"
      case SeqSlice(s, i, NoExpr) => s"${s.show}.slice(${i.show})"
      case SeqSlice(s, i, j) => s"${s.show}.slice(${i.show}, ${j.show})"
      case SeqConcat(s1, s2) => s"(${s1.show} ++ ${s2.show})"
      case SeqReverse(s) => s"${s.show}.reverse"
      case SeqIndexOf(s, t, Const(0)) => s"${s.show}.indexOf(${t.show})"
      case SeqIndexOf(s, t, start) => s"${s.show}.indexOf(${t.show}, ${start.show})"
      case SeqContains(s, t) => s"${s.show}.contains(${t.show})"
      case SeqStartsWith(s, t) => s"${s.show}.startsWith(${t.show})"
      case SeqEndsWith(s, t) => s"${s.show}.endsWith(${t.show})"
      case SeqCount(e, et) => s"${e.show}.count(${et.show})"
      case SeqForall(s, ep) => s"${s.show}.forall(${ep.show})"
      // String operations
      case StringSplit(s, t) => s"${s.show}.split(${t.show})"
      case StringTrim(s) => s"${s.show}.trim"
      case StringToLower(s) => s"${s.show}.toLowerCase"
      case StringToUpper(s) => s"${s.show}.toUpperCase"
      case StringToInt(s) => s"${s.show}.toInt"
      case StringFromInt(i) => s"${i.show}.toString"
      // Set operations
      case SetLit(es) => es.map(_.show).mkString("Set(", ", ", ")")
      case SetSize(s) => s"|${s.show}|"
      case SetContains(s, t) => s"${t.show} ∈ ${s.show}"
      case Subset(s1, s2) => s"${s1.show} ⊆ ${s2.show}"
      case SetUnion(s1, s2) => s"${s1.show} ∪ ${s2.show}"
      case SetInter(s1, s2) => s"${s1.show} ∩ ${s2.show}"
      case SetDiff(s1, s2) => s"${s1.show} ∖ ${s2.show}"
      case SetForall(e, ep) => s"${e.show}.forall(${ep.show})"
      // Map operations
      case MapLit(eks, evs) =>
        (eks.map(_.show) zip evs.map(_.show)).map(_ + " = " + _).mkString("Map(", ", ", ")")
      case MapKeys(m) => s"${m.show}.keys"
      case MapValues(m) => s"${m.show}.values"
      case MapItems(m) => s"${m.show}.items"
      case MapSize(m) => s"|${m.show}|"
      case MapContains(m, k) => s"${k.show} ∈ ${m.show}"
      case MapSelect(m, k) => s"${m.show}[${k.show}]"
      case MapUpdate(m, k, v) => s"${m.show}.update(${k.show}, ${v.show})"
      // Tuple operations
      case TupleExpr(es) => es.map(_.show).mkString("(", ", ", "))")
      case TupleSelect(i, e) => s"${e.show}._${i + 1}"
      // Other
      case NoExpr => "?"

  extension (vd: VarDecl)
    def show: String = s"${vd.name}: ${vd.typ.sort.show}"

  private val subscriptDigits = Map(
    '0' -> '₀',
    '1' -> '₁',
    '2' -> '₂',
    '3' -> '₃',
    '4' -> '₄',
    '5' -> '₅',
    '6' -> '₆',
    '7' -> '₇',
    '8' -> '₈',
    '9' -> '₉'
  )