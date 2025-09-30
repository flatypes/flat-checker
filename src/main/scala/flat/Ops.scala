package flat

object Ops:
  enum CmpOp:
    case EQ
    case NE
    case LE
    case LT
    case GE
    case GT

    def eval(n1: Int, n2: Int): Boolean = this match
      case EQ => n1 == n2
      case NE => n1 != n2
      case LE => n1 <= n2
      case LT => n1 < n2
      case GE => n1 >= n2
      case GT => n1 > n2

    override def toString: String = this match
      case EQ => "="
      case NE => "≠"
      case LE => "≤"
      case LT => "<"
      case GE => "≥"
      case GT => ">"