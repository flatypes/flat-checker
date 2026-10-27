package flat.checker.typing

import flat.checker.domain.{CharRE, StrRE}
import flat.checker.flan.tpd.{Expr, Type}
import flat.checker.flan.untpd

sealed trait Info:
  val id: untpd.Ident

// Globals
final case class MethodInfo(id: untpd.Ident, params: List[(String, Type)], returnType: Type,
                            requires: List[Expr], ensures: List[Expr], scope: Map[String, Info]) extends Info:
  def paramNames: List[String] = params.map(_._1)

  def paramTypes: List[Type] = params.map(_._2)

final case class ConstInfo(id: untpd.Ident, sort: Type, value: Expr) extends Info

final case class LangInfo(id: untpd.Ident, regEx: CharRE) extends Info

final case class TypeInfo(id: untpd.Ident, typ: Type) extends Info

// Locals
final case class ValInfo(id: untpd.Ident, typ: Type) extends Info

final case class VarInfo(id: untpd.Ident, typ: Type) extends Info

final case class TypingCtx(uri: String,
                           stack: List[Map[String, Info]] = List(Map.empty),
                           notNone: Set[String] = Set.empty):
  def get(name: String): Option[Info] = stack.head.get(name)

  def lookup(name: String): Option[Info] =
    stack.collectFirst { case m if m.contains(name) => m(name) }

  def scope: Map[String, Info] = stack.head

  def declare(name: String, info: Info): TypingCtx =
    copy(stack = stack.head + (name -> info) :: stack.tail)

  def pushed: TypingCtx = copy(stack = Map.empty :: stack)