package flat.checker.domain

enum Index:
  case Left(i: Int)
  case Right(i: Int)
  case First(word: List[Any], offset: Int = 0)
  case UnknownIndex