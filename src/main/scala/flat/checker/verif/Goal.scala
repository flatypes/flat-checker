package flat.checker.verif

import flat.checker.flan.Show.show
import flat.checker.flan.tpd.*

final case class Goal(conclusion: Expr, premises: List[Expr], ctx: Map[String, Type]):
  @deprecated
  def have(sides: Expr*): Boolean = true

  def pp: String =
    val premiseStr = (for e <- premises yield s"  ${e.show}\n").mkString
    s"$premiseStr ⇒ ${conclusion.show}"