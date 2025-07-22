package flat

object Ops:
  enum CmpOp:
    case EQ
    case NE
    case LE
    case LT
    case GE
    case GT

    override def toString: String = this match
      case EQ => "="
      case NE => "≠"
      case LE => "≤"
      case LT => "<"
      case GE => "≥"
      case GT => ">"