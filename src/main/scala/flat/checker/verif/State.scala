package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.{Expr, NormType, VarDecl}
import flat.checker.verif.Simplifier.simplify

final case class MethodInfo(params: List[VarDecl], returns: VarDecl, requires: List[Expr], ensures: List[Expr]):
  def paramNames: List[String] = params.map(_.name)

final case class VarInfo(typ: Sort, value: Expr)

final case class PrfCtx(vars: Map[String, Sort] = Map.empty,
                        premises: List[Expr] = Nil):
  def add(value: Expr): PrfCtx = copy(premises = premises :+ value.simplify)

final case class State(methods: Map[String, MethodInfo] = Map.empty,
                       currentMethod: String = "",
                       types: Map[String, NormType] = Map.empty,
                       values: Map[String, Expr] = Map.empty,
                       freshCounts: Map[String, Int] = Map.empty,
                       ctx: PrfCtx = PrfCtx()):
  def fresh(name: String, sort: Sort): (String, State) =
    val version = freshCounts.getOrElse(name, 0)
    val freshName = versioned(name, version)
    (freshName, copy(
      freshCounts = freshCounts + (name -> (version + 1)),
      ctx = ctx.copy(vars = ctx.vars + (freshName -> sort))))

  def add(value: Expr): State = copy(ctx = ctx.add(value))

inline def versioned(name: String, version: Int): String = s"$name:$version"