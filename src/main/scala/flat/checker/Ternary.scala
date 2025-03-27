package flat.checker

enum Ternary:
  case Bot
  case True
  case False
  case Maybe

  def isEmpty: Boolean = this == Bot

  def isBoolean: Boolean =
    this == True || this == False

  def asBoolean: Boolean =
    require(isBoolean)
    this match
      case True => true
      case False => false
      case _ => assert(false)

  /** Test if this set contains a Boolean value. */
  def contains(value: Boolean): Boolean =
    this match
      case Bot => false
      case True => value
      case False => !value
      case Maybe => true

  def &&(that: Ternary): Ternary =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (False, _) | (_, False) => False
      case (True, bs) => bs
      case (bs, True) => bs
      case _ => Maybe

  def ||(that: Ternary): Ternary =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (True, _) | (_, True) => True
      case (False, b) => b
      case (b, False) => b
      case _ => Maybe

  def unary_! : Ternary =
    this match
      case Bot => Bot
      case True => False
      case False => True
      case Maybe => Maybe

  infix def iff(that: Ternary): Ternary =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (True, True) | (False, False) => True
      case (True, False) | (False, True) => False
      case _ => Maybe

  override def toString: String =
    this match
      case Bot => "⊥"
      case True => "true"
      case False => "false"
      case Maybe => "Bool"

object Ternary:
  def fromBoolean(value: Boolean): Ternary =
    if value then True else False

  given Conversion[Boolean, Ternary] = fromBoolean

given AbsDom[Ternary]:
  import Ternary.*

  def top: Ternary = Maybe

  def bot: Ternary = Bot

  def subElement(b1: Ternary, b2: Ternary): Boolean =
    (b1, b2) match
      case (_, Maybe) => true
      case (Bot, _) => true
      case (True, True) | (False, False) => true
      case _ => false

  def join(b1: Ternary, b2: Ternary): Ternary =
    (b1, b2) match
      case (Bot, b) => b
      case (b, Bot) => b
      case (True, True) => True
      case (False, False) => False
      case _ => Maybe

  def meet(b1: Ternary, b2: Ternary): Ternary =
    (b1, b2) match
      case (Maybe, b) => b
      case (b, Maybe) => b
      case (True, True) => True
      case (False, False) => False
      case _ => Bot

  def widen(b1: Ternary, b2: Ternary): Ternary = join(b1, b2)
