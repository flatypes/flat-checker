package flat.checker

enum Sort:
  case Top
  case Bot
  case Int
  case Bool
  case Char
  case String
  case Array(elem: Sort)
  case Fun(args: Seq[Sort], returns: Sort)

  def :<:(that: Sort): Boolean =
    (this, that) match
      case (Bot, _) | (_, Top) => true
      case (Fun(xs, x), Fun(ys, y)) => (ys zip xs).forall(_ :<: _) && x :<: y
      case _ => this == that