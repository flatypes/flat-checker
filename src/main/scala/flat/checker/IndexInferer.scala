package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.util.tryAll

import scala.Function.unlift

final case class IndexShifted(base: AIndex, offset: Int):
  def +(k: Int): IndexShifted = IndexShifted(base, offset + k)

  def concretize(str: Expr): Expr =
    val eb = base match
      case AIndexL(k) => Const(k)
      case AIndexR(k) => SUB(Length(str), Const(k))
      case AIndexAt(t) => Find(str, Const(t))
    offset match
      case 0 => eb
      case k if k > 0 => ADD(eb, Const(k))
      case k /* k < 0 */ => SUB(eb, Const(k))

final case class IndexInterval(lb: Index, ub: Index)

type Index = AIndex | IndexShifted | IndexInterval

private def shift(index: Index, offset: Int): Index = index match
  case AIndexL(k) => AIndexL(k + offset)
  case AIndexR(k) => AIndexR(k - offset)
  case b: AIndex => IndexShifted(b, offset)
  case IndexShifted(b, k) => IndexShifted(b, k + offset)
  case IndexInterval(lb, ub) => IndexInterval(shift(lb, offset), shift(ub, offset))

class IndexInferer(using ctx: PrfCtx, config: Config) extends LazyLogging:
  def infer(idx: Expr, str: Expr): Index =
    convert(idx, str).getOrElse {
      idx match
        case Var(x) => solve(x, str)
        case Arith(ADD, Var(x), Const(k: Int)) => shift(solve(x, str), k)
        case Arith(SUB, Var(x), Const(k: Int)) => shift(solve(x, str), -k)
        case Arith(ADD, Find(es, Const(t: String)), Const(k: Int)) if es == str => shift(AIndexAt(t), k)
        case _ => throw UnsupportedOperationException(idx.toString)
    }

  private def convert(idx: Expr, str: Expr): Option[AIndex | IndexShifted] = idx match
    case Const(n: Int) if n >= 0 => Some(AIndexL(n))
    case Length(e) if e == str => Some(AIndexR(0))
    case Arith(SUB, Length(e), Const(n: Int)) if e == str && n > 0 => Some(AIndexR(n))
    case Find(e, Const("")) => Some(AIndexL(0))
    case Find(e, Const(t: String)) if e == str => Some(AIndexAt(t))
    case Arith(op, Find(e, Const(t: String)), Const(k: Int)) if e == str && k > 0 =>
      Some(IndexShifted(AIndexAt(t), if op == ADD then k else -k))
    case _ => None

  private def solve(x: String, str: Expr): Index =
    val constraints = ctx.collect(unlift {
      case cond: Cmp => Rewriter.push(Var(x), cond)
      case _ => None
    })
    logger.debug(s"infer $x: constraints: $constraints")

    val eqs = constraints.flatMap {
      case (EQ, e) => convert(e, str)
      case _ => None
    }
    if eqs.nonEmpty then
      require(eqs.length == 1, s"multiple EQ: $eqs")
      return eqs.head
    val lbs = constraints.flatMap {
      case (GE, e) => convert(e, str)
      case (GT, e) => convert(e, str) //.map(_ + 1)
      case _ => None
    }
    val ubs = constraints.flatMap {
      case (LE, e) => convert(e, str)
      case (LT, e) => convert(e, str) //.map(_ - 1)
      case _ => None
    }
    val common = lbs.toSet & ubs.toSet
    if common.nonEmpty then
      require(common.size == 1, s"multiple EQ: $common")
      return common.head

    logger.debug(s"lbs: $lbs  ubs: $ubs")
    val lb = tryAll(
      () => lbs.find(i => i.isInstanceOf[IndexShifted] && i.asInstanceOf[IndexShifted].base.isInstanceOf[AIndexAt]),
      () => lbs.filter {
        case IndexShifted(AIndexL(_), _) => true
        case _ => false
      }.maxByOption(_.asInstanceOf[IndexShifted].offset),
      () => lbs.minByOption(_.asInstanceOf[IndexShifted].offset)
    ).getOrElse(AIndexL(0))
    val ub = tryAll(
      () => ubs.find(i => i.isInstanceOf[IndexShifted] && i.asInstanceOf[IndexShifted].base.isInstanceOf[AIndexAt]),
      () => ubs.filter {
        case IndexShifted(AIndexL(_), _) => true
        case _ => false
      }.minByOption(_.asInstanceOf[IndexShifted].offset),
      () => ubs.maxByOption(_.asInstanceOf[IndexShifted].offset)
    ).getOrElse(AIndexR(1))
    logger.debug(s"lb: $lb  ub: $ub")
    IndexInterval(lb, ub)