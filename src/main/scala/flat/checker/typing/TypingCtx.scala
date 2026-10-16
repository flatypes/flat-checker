package flat.checker.typing

import flat.checker.domain.StrRE
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

final case class LangInfo(id: untpd.Ident, regEx: StrRE) extends Info

final case class TypeInfo(id: untpd.Ident, typ: Type) extends Info

// Locals
final case class ValInfo(id: untpd.Ident, typ: Type) extends Info

final case class VarInfo(id: untpd.Ident, typ: Type) extends Info

final case class TypingCtx(uri: String,
                           stack: List[Map[String, Info]] = List(Map.empty),
                           conds: List[Expr] = Nil):
  def get(name: String): Option[Info] = stack.head.get(name)

  def lookup(name: String): Option[Info] =
    stack.collectFirst { case m if m.contains(name) => m(name) }

  def scope: Map[String, Info] = stack.head

  def declare(name: String, info: Info): TypingCtx =
    copy(stack = stack.head + (name -> info) :: stack.tail)

  def updatedWith(name: String)(pf: PartialFunction[Info, Info]): TypingCtx =
    val i = stack.indexWhere(_.contains(name))
    require(i >= 0, s"not found: $name")
    pf.lift(stack(i)(name)) match
      case Some(info) => copy(stack = stack.updated(i, stack(i) + (name -> info)))
      case None => this

  def pushed: TypingCtx = copy(stack = Map.empty :: stack)