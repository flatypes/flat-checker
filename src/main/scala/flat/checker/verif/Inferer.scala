package flat.checker.verif

import com.typesafe.scalalogging.LazyLogging
import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*
import flat.regex.AOps.*
import flat.regex.*
import flat.regex.REAbsOps.*

class Inferer(goal: Goal) extends LazyLogging:
  def infer(expr: Expr): Domain = expr match
    // Constants and variables
    case Const(s: String) => RegEx.fromString(s)
    case Var(x) =>
      goal.premises.reverseIterator.collectFirst { case StringInLang(Var(`x`), r) => r }
        .getOrElse:
          logger.warn(s"Cannot infer ${expr.show}")
          RegEx.all

    // Char
    case CharToInt(e) => ???
    case CharFromInt(e) => ???
    case CharToString(e) => ???

    // Seq[Char]
    case SeqLit(es) => ???
    case SeqLength(e) =>
      infer(e) match
        case r: RegEx => r.absLength
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          Interval(0, Inf)
    case SeqSelect(e, ei) =>
      (infer(e), ei) match
        case (r: RegEx, Const(i: Int)) => r.absCharAt(i)
        case (r: RegEx, Sub(SeqLength(`e`), Const(k: Int))) => r.reverse.absCharAt(k - 1)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          CharSet.full
    case SeqUpdate(e, ei, ej) => ???
    case SeqSlice(e, ei, ej) =>
      (infer(e), ei, ej) match
        case (r: RegEx, Const(i: Int), Const(j: Int)) => r.absDrop(i).absTake(j - i)
        case (r: RegEx, Const(i: Int), SeqLength(e)) => r.absDrop(i)
        case (r: RegEx, Const(i: Int), Add(SeqLength(e), Const(k: Int))) if k > 0 => r.absDrop(i)
        case (r: RegEx, Const(i: Int), Sub(SeqLength(e), Const(k: Int))) if k > 0 => r.absDrop(i).absDropRight(k)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          RegEx.all
    case SeqConcat(e1, e2) =>
      (infer(e1), infer(e2)) match
        case (r1: RegEx, r2: RegEx) => r1 ++ r2
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          TopDomain
    case SeqReverse(e) =>
      infer(e) match
        case r: RegEx => r.reverse
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          TopDomain
    case SeqIndexOf(e, et, ei) => ???
    case SeqContains(e, et) =>
      (infer(e), et) match
        case (r: RegEx, Const(t: String)) if t.length == 1 => r.absContains(t.head)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          BoolSet.Top
    case SeqStartsWith(e, et) =>
      (infer(e), et) match
        case (r: RegEx, Const(t: String)) => r.absStartsWith(t)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          BoolSet.Top
    case SeqEndsWith(e, et) =>
      (infer(e), et) match
        case (r: RegEx, Const(t: String)) => r.reverse.absStartsWith(t)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          BoolSet.Top
    case SeqCount(e, et) =>
      (infer(e), et) match
        case (r: RegEx, Const(t: String)) if t.length == 1 => r.absCount(t.head)
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          Interval(0, Inf)
    case SeqForall(e, ep) => ???

    // String
    case StringSplit(e, et) => ???
    case StringTrim(e) => ???
    case StringToLower(e) =>
      infer(e) match
        case r: RegEx => r.absToLower
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          RegEx.all
    case StringToUpper(e) =>
      infer(e) match
        case r: RegEx => r.absToUpper
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          RegEx.all
    case StringToInt(e) => ???
      infer(e) match
        case r: RegEx => r.absToInt
        case _ =>
          logger.warn(s"Cannot infer ${expr.show}")
          Interval(0, Inf)
    case StringFromInt(e) => ???

    // Other
    case _ =>
      logger.warn(s"Cannot infer ${expr.show}")
      TopDomain
