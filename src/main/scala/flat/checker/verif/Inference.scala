package flat.checker.verif

import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.tpd.*

enum Type:
  case TBool(dom: BoolSet)
  case TNat(dom: CountingRE)
  case TIndex(dom: IndexSet)
  case TChar(dom: CharSet)
  case TStr(dom: StrRE)
  case TStrSeq(dom: RegEx[StrRE])

import flat.checker.verif.Type.*

class Inference:
  def infer(expr: Expr): Type = expr match
    // Str Operations
    case StrConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStr(r1), TStr(r2)) => TStr(r1 * r2)
        case _ => throw new Exception("Expected two string types for StrConcat")
    case StrLength(e) =>
      infer(e) match
        case TStr(r) => TNat(r.absLength)
        case _ => throw new Exception("Expected a string type for StrLength")
    case CharAt(e, Const(i: Int)) =>
      infer(e) match
        case TStr(r) => TChar(r.absAt(i))
        case _ => throw new Exception("Expected a string type for StrAt")
    case Substr(e, Const(i: Int), Const(j: Int)) =>
      infer(e) match
        case TStr(r) => TStr(r.absSlice(i, j))
        case _ => throw new Exception("Expected a string type for StrSubstring")
    case StrStartsWith(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) => TBool(r.absStartsWithStr(s))
        case _ => throw new Exception("Expected a string type for StrStartsWith")
    case StrEndsWith(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) => TBool(r.absEndsWithStr(s))
        case _ => throw new Exception("Expected a string type for StrEndsWith")
    case StrContains(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) => TBool(r.absContainsStr(s))
        case _ => throw new Exception("Expected a string type for StrContains")
    case StrIndexOf(e, Const(s: String), Const(0)) =>
      infer(e) match
        case TStr(r) => TIndex(r.absIndexOfStr(s))
        case _ => throw new Exception("Expected a string type for StrIndexOf")
    case StrSplit(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) => TStrSeq(r.absSplitStr(s))
        case _ => throw new Exception("Expected a string type for StrSplit")
    case StrCount(e, Const(s: String)) =>
      infer(e) match
        case TStr(r) => TNat(r.absCountStr(s))
        case _ => throw new Exception("Expected a string type for StrCount")
    case StrReverse(e) =>
      infer(e) match
        case TStr(r) => TStr(r.reverse)
        case _ => throw new Exception("Expected a string type for StrReverse")

    // Str Conversions

    // Seq Operations
    case SeqConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStrSeq(r1), TStrSeq(r2)) => TStrSeq(r1 * r2)
        case _ => throw new Exception("Expected two sequence types for SeqConcat")
    case SeqLength(e) =>
      infer(e) match
        case TStrSeq(r) => TNat(r.absLength)
        case _ => throw new Exception("Expected a sequence type for SeqLength")
    case SeqSelect(e, Const(i: Int)) =>
      infer(e) match
        case TStrSeq(r) => TStr(r.absAt(i))
        case _ => throw new Exception("Expected a sequence type for SeqAt")

    case other => throw new Exception(s"Type inference not implemented for expression: $other")