package flat.checker.domain

enum Index:
  case Left(i: Int)
  case Right(i: Int)
  case First(word: List[Any], start: Int = 0, offset: Int = 0)
  case Range(start: Index, end: Index)

  def shift(n: Int): Index = this match
    case Left(i) => Left(i + n)
    case Right(i) => Right(i + n)
    case First(word, start, offset) => First(word, start, offset + n)
    case _ => this
