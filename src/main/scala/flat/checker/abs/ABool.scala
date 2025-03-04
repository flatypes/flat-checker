package flat.checker.abs

import scala.annotation.targetName

enum ABool:
  case Bot
  case True
  case False
  case Top

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
      case Top => true

  @targetName("and")
  def &&(that: ABool): ABool =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (False, _) | (_, False) => False
      case (True, bs) => bs
      case (bs, True) => bs
      case _ => Top

  @targetName("or")
  def ||(that: ABool): ABool =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (True, _) | (_, True) => True
      case (False, b) => b
      case (b, False) => b
      case _ => Top

  @targetName("not")
  def unary_! : ABool =
    this match
      case Bot => Bot
      case True => False
      case False => True
      case Top => Top

  infix def iff(that: ABool): ABool =
    (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      case (True, True) | (False, False) => True
      case (True, False) | (False, True) => False
      case _ => Top

  override def toString: String =
    this match
      case Bot => "⊥"
      case True => "true"
      case False => "false"
      case Top => "Bool"

object ABool:
  def fromBoolean(value: Boolean): ABool =
    if value then True else False

  def from(tvl: Option[Boolean]): ABool =
    tvl match
      case Some(b) => fromBoolean(b)
      case None => Top

given AbsDom[ABool]:
  import ABool.*

  def top: ABool = Top
  def bot: ABool = Bot

  def subElement(b1: ABool, b2: ABool): Boolean =
    (b1, b2) match
      case (_, Top) => true
      case (Bot, _) => true
      case (True, True) | (False, False) => true
      case _ => false

  //    @targetName("meet")
  //    def &(b2: ABool): ABool =
  //      (b1, b2) match
  //        case (Top, b) => b
  //        case (b, Top) => b
  //        case (True, True) => True
  //        case (False, False) => False
  //        case _ => Bot
  def join(b1: ABool, b2: ABool): ABool =
    (b1, b2) match
      case (Bot, b) => b
      case (b, Bot) => b
      case (True, True) => True
      case (False, False) => False
      case _ => Top
  
  def widen(b1: ABool, b2: ABool): ABool = join(b1, b2)
