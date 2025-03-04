package flat.checker

enum Bound:
  case NegInf
  case Fin(value: Int)
  case PosInf

  def asInt: Int = asInstanceOf[Fin].value

  override def toString: String = this match
    case NegInf => "-∞"
    case Fin(k) => k.toString
    case PosInf => "∞"

object Bound extends Ordering[Bound]:
  def compare(x: Bound, y: Bound): Int =
    (x, y) match
      case (NegInf, NegInf) => 0
      case (NegInf, _) => -1
      case (Fin(k1), Fin(k2)) => k1 compare k2
      case (Fin(_), PosInf) => -1
      case (PosInf, PosInf) => 0
      case _ => 1

  given Conversion[Int, Bound] = Bound.Fin.apply
