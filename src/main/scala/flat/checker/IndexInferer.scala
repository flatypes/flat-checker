package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.checker.core.ArithOp.*
import flat.regex.*
import flat.util.tryAll

import scala.Function.unlift

final case class IndexShifted(base: BasicIndex, offset: Int) extends Index

final case class IndexInterval(lb: Index, ub: Index) extends Index

extension (index: Index)
  def shift(offset: Int): Index =
    index match
      case IndexL(k) => IndexL(k + offset)
      case IndexR(k) => IndexR((k - offset) max 0)
      case b: BasicIndex => if offset == 0 then b else IndexShifted(b, offset)
      case IndexShifted(b, k) => if k + offset == 0 then b else IndexShifted(b, k + offset)
      case IndexInterval(lb, ub) => IndexInterval(lb.shift(offset), ub.shift(offset))

  def getOffset: Int = index match
    case IndexShifted(_, k) => k
    case _ => 0

class IndexInferer(using ctx: PrfCtx, config: Config) extends LazyLogging:
  def infer(idx: Expr, str: Expr): Index =
    convert(idx, str).getOrElse {
      idx match
        case Var(x) => solve(x, str)
        case Arith(ADD, Var(x), Const(k: Int)) => solve(x, str).shift(k)
        case Arith(SUB, Var(x), Const(k: Int)) => solve(x, str).shift(-k)
        case Arith(ADD, Find(es, Const(t: String)), Const(k: Int)) if es == str => IndexAt(t).shift(k)
        case _ => throw UnsupportedOperationException(idx.toString)
    }

  private def convert(idx: Expr, str: Expr): Option[Index] = idx match
    case Const(n: Int) if n >= 0 => Some(IndexL(n))
    case Length(e) if e == str => Some(IndexR(0))
    case Arith(SUB, Length(e), Const(n: Int)) if e == str && n > 0 => Some(IndexR(n))
    case Find(e, Const("")) => Some(IndexL(0))
    case Find(e, Const(t: String)) if e == str => Some(IndexAt(t))
    case Arith(op, Find(e, Const(t: String)), Const(k: Int)) if e == str && k > 0 =>
      Some(IndexShifted(IndexAt(t), if op == ADD then k else -k))
    case _ => None

  private def solve(x: String, str: Expr): Index =
    val constraints = ctx.collect(unlift {
      case cond: Cmp => Rewriter.push(Var(x), cond)
      case _ => None
    })

    val eqs = constraints.flatMap {
      case (EQ, e) => convert(e, str)
      case _ => None
    }.distinct
    if eqs.nonEmpty then
      require(eqs.length == 1, s"multiple EQ: $eqs")
      return eqs.head
    val lbs = constraints.flatMap {
      case (GE, e) => convert(e, str)
      case (GT, e) => convert(e, str).map(_.shift(1))
      case _ => None
    }
    val ubs = constraints.flatMap {
      case (LE, e) => convert(e, str)
      case (LT, e) => convert(e, str).map(_.shift(-1))
      case _ => None
    }
    val common = lbs.toSet & ubs.toSet
    if common.nonEmpty then
      require(common.size == 1, s"multiple EQ: $common")
      return common.head

    val lb = tryAll(
      () => lbs.find(i => i.isInstanceOf[IndexAt] ||
        i.isInstanceOf[IndexShifted] && i.asInstanceOf[IndexShifted].base.isInstanceOf[IndexAt]),
      () => lbs.filter {
        case IndexShifted(IndexL(_), _) => true
        case _ => false
      }.maxByOption(_.asInstanceOf[IndexShifted].offset),
      () => lbs.minByOption(_.getOffset)
    ).getOrElse(IndexL(0))
    val ub = tryAll(
      () => ubs.find(i => i.isInstanceOf[IndexAt] ||
        i.isInstanceOf[IndexShifted] && i.asInstanceOf[IndexShifted].base.isInstanceOf[IndexAt]),
      () => ubs.filter {
        case IndexShifted(IndexL(_), _) => true
        case _ => false
      }.minByOption(_.asInstanceOf[IndexShifted].offset),
      () => ubs.maxByOption(_.getOffset)
    ).getOrElse(IndexR(1))
    IndexInterval(lb, ub)