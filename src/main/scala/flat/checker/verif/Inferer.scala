package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Index.*
import flat.checker.domain.REOps.*
import flat.checker.domain.StrREOps.*
import flat.checker.domain.{*, given}
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

enum Type:
  case TBool(dom: BoolSet)
  case TNat(dom: CountingRE)
  case TIndex(dom: IndexSet)
  case TChar(dom: CharSet)
  case TStr(dom: StrRE)
  case TStrSeq(dom: RegEx[StrRE])

import flat.checker.verif.Type.*

class Inferer(goal: Goal) extends LazyLogging:
  def infer(expr: Expr): Type = expr match
    case Const(s: String) => TStr(RegEx.word(s.toList))
    case Var(x) =>
      goal.premises.reverseIterator.collectFirst { case StringInLang(Var(`x`), r) => TStr(r) }
        .getOrElse:
          logger.warn(s"Cannot infer ${expr.show}")
          TStr(RegEx.Lit(CharSet.full))

    // Seq Operations
    case SeqConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (TStr(r1), TStr(r2)) => TStr(r1 * r2)
        case (TStrSeq(r1), TStrSeq(r2)) => TStrSeq(r1 * r2)
        case _ => throw new Exception("Expected two sequence types for SeqConcat")
    case SeqLength(e) =>
      infer(e) match
        case TStr(r) => TNat(r.absLength)
        case TStrSeq(r) => TNat(r.absLength)
        case _ => throw new Exception("Expected a sequence type for SeqLength")
    case SeqSelect(e, Const(i: Int)) =>
      infer(e) match
        case TStr(r) => TChar(r.absAt(i))
        case TStrSeq(r) => TStr(r.absAt(i))
        case _ => throw new Exception("Expected a sequence type for SeqAt")
    case SeqSlice(e, ei, ej) =>
      (infer(e), inferIndex(ei, e), inferIndex(ej, e)) match
        case (TStr(r), i, j) => TStr(r.absSlice(i, j))
        case _ => throw new Exception(s"Cannot infer ${expr.show}")
    case SeqStartsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absStartsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqStartsWith")
    case SeqEndsWith(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absEndsWith(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqEndsWith")
    case SeqContains(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TBool(r.absContains(s.toList))
        case _ => throw new Exception("Expected a sequence type for SeqContains")
    case SeqIndexOf(e, et, Const(0)) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TIndex(r.absIndexOfStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqIndexOf")
    case StringSplit(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TStrSeq(r.absSplitStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqSplit")
    case SeqCount(e, et) =>
      (infer(e), et) match
        case (TStr(r), Const(s: String)) => TNat(r.absCountStr(s))
        case _ => throw new Exception("Expected a sequence type for SeqCount")
    case SeqReverse(e) =>
      infer(e) match
        case TStrSeq(r) => TStrSeq(r.reverse)
        case _ => throw new Exception("Expected a sequence type for SeqReverse")

    // String-specific
    case StringToLower(e) =>
      infer(e) match
        case TStr(r) => TStr(r.absMap(_.map(_.toLower)))
        case _ => throw new Exception("Expected a string type for StringToLower")
    case StringToUpper(e) =>
      infer(e) match
        case TStr(r) => TStr(r.absMap(_.map(_.toUpper)))
        case _ => throw new Exception("Expected a string type for StringToUpper")

    case other =>
      throw new Exception(s"Type inference not implemented for expression: $other")

  private def inferIndex(idx: Expr, seq: Expr): Index = idx match
    case Const(i: Int) if 0 <= i => Left(i)
    case SeqLength(`seq`) => Right(0)
    case Add(SeqLength(`seq`), Const(i: Int)) if 0 <= i => Right(i)
    case Sub(SeqLength(`seq`), Const(i: Int)) if 0 <= i => Right(i)
    case _ => throw new Exception(s"Cannot infer index ${idx.show} for ${seq.show}")