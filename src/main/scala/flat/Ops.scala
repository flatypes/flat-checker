package flat

object Ops:
  enum CmpOp:
    case EQ
    case NE
    case LE
    case LT
    case GE
    case GT

    /** Concrete semantics: `n1 op n2`. */
    def eval(n1: Int, n2: Int): Boolean = this match
      case EQ => n1 == n2
      case NE => n1 != n2
      case LE => n1 <= n2
      case LT => n1 < n2
      case GE => n1 >= n2
      case GT => n1 > n2

    /** Negation operation: `x neg_op y` iff `!(x op y)`. */
    def negation: CmpOp = this match
      case EQ => NE
      case NE => EQ
      case LE => GT
      case LT => GE
      case GE => LT
      case GT => LE

    /** Reverse operation: `x rev_op y` iff `y op x`. */
    def reverse: CmpOp = this match
      case EQ => EQ
      case NE => NE
      case LE => GE
      case LT => GT
      case GE => LE
      case GT => LT

    override def toString: String = this match
      case EQ => "="
      case NE => "≠"
      case LE => "≤"
      case LT => "<"
      case GE => "≥"
      case GT => ">"