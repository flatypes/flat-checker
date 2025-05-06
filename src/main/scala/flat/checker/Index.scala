package flat.checker

import flat.checker.core.*
import flat.checker.core.CmpOp.*

sealed trait AbsIndex:
  def str: Expr

  def toExpr: Expr

  def +(k: Int): AbsIndex

  def -(k: Int): AbsIndex = this + (-k)

sealed trait Index extends AbsIndex:
  def +(k: Int): Index

  override def -(k: Int): Index = this + (-k)

enum Direction:
  case L
  case R

final case class IndexAt(str: Expr, direction: Direction, index: Int) extends Index:
  require(index >= 0, s"negative index: $index")

  import Direction.*

  def toExpr: Expr = direction match
    case L => Const(index)
    case R => mkSub(StrLen(str), index)

  def +(k: Int): Index = direction match
    case L => copy(index = index + k)
    case R => copy(index = 0 max (index - k))

final case class IndexOf(str: Expr, char: Char, offset: Int = 0) extends Index:
  def toExpr: Expr = StrFind(str, char.toString)

  def +(k: Int): Index = copy(offset = offset + k)

final case class IndexRange(from: Index, to: Index) extends AbsIndex:
  require(from.str == to.str)

  def str = from.str

  def toExpr: Expr = throw UnsupportedOperationException()

  def +(k: Int): IndexRange = IndexRange(from + k, to + k)
