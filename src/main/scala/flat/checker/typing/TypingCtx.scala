package flat.checker.typing

import flat.checker.domain.StrRE
import flat.checker.flan.tpd.{Expr, Type}
import flat.checker.flan.untpd

sealed trait Info:
  val id: untpd.Ident

// Globals
final case class MethodInfo(id: untpd.Ident, params: List[(String, Type)], returnType: Type,
                            requires: List[Expr], ensures: List[Expr], localCtx: TypingCtx) extends Info:
  def paramNames: List[String] = params.map(_._1)

  def paramTypes: List[Type] = params.map(_._2)

final case class ConstInfo(id: untpd.Ident, sort: Type, value: Expr) extends Info

final case class LangInfo(id: untpd.Ident, regEx: StrRE) extends Info

final case class TypeInfo(id: untpd.Ident, typ: Type) extends Info

// Locals
final case class ValInfo(id: untpd.Ident, typ: Type) extends Info

final case class VarInfo(id: untpd.Ident, typ: Type) extends Info

final case class TypingCtx(uri: String,
                           stack: List[Map[String, Info]] = List(Map.empty),
                           conds: List[Expr] = Nil):
  def isEmpty: Boolean = stack.isEmpty

  def get(name: String): Option[Info] = stack.head.get(name)

  def define(name: String, info: Info): TypingCtx =
    val head = stack.head + (name -> info)
    copy(stack = head :: stack.tail)

  def lookup(name: String): Option[Info] =
    stack.collectFirst { case m if m.contains(name) => m(name) }

  def assume(cond: Expr): TypingCtx = copy(conds = cond :: conds)

  def pushed: TypingCtx = copy(stack = Map.empty :: stack)
