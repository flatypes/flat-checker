package flat.checker

enum Sort:
  case Top
  case Bot
  case I
  case B
  case S
  case Tuple(elems: Seq[Sort])
  case Array(elem: Sort)
  case Fun(args: Seq[Sort], returns: Sort)

  /** Sub-sorting relation. `s1 :<: s2` checks if `s1` is a sub-sort of `s2`. */
  def :<:(lower: Sort): Boolean =
    // Note: this operator, ending with a colon, is right-associative
    (lower, this) match
      case (_, Top) | (Bot, _) => true
      case (Fun(xs, x), Fun(ys, y)) => (ys zip xs).forall(_ :<: _) && x :<: y
      case _ => lower == this
