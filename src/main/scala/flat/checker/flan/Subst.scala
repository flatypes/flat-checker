package flat.checker.flan

import flat.checker.flan.tpd.*

object Subst:
  private def substExpr(expr: Expr, m: Map[String, Expr], boundVars: Set[String]): Expr = expr match
    case Var(x) => if !boundVars.contains(x) && m.contains(x) then m(x) else expr
    case Lambda(ps, e) => Lambda(ps, substExpr(e, m, boundVars ++ ps.map(_.name)))
    case _ => expr.rebuild(substExpr(_, m, boundVars))

  extension (expr: Expr)
    def subst(x: String, e: Expr): Expr =
      substExpr(expr, Map(x -> e), Set.empty)

    def subst(xs: List[String], es: List[Expr]): Expr =
      require(xs.length == es.length, "Length of variable names and expressions must match")
      substExpr(expr, xs.zip(es).toMap, Set.empty)