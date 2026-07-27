package flat.checker.domain

import flat.checker.domain.RegEx.*
import flat.checker.util.ExprPrettifier

object Prettifier:
  private class REPrettifier[T, D] extends ExprPrettifier[RegEx[T, D]]:
    private val LEVEL_COMP = 30
    private val LEVEL_PLUS = 40

    override protected def getLevel(r: RegEx[T, D]): Int = r match
      case REZero() | REOne() | RELit(_) => 0
      case REStar(_) => LEVEL_POSTFIX
      case REComp(_, _) => LEVEL_COMP
      case REPlus(_, _) => LEVEL_PLUS

    override def ppExpr(r: RegEx[T, D]): String = r match
      case REZero() => "∅"
      case REOne() => "ε"
      case RELit(a: CharSet) => ppCharSet(a)
      case RELit(r1: RegEx[_, _]) => "⌜" + r1.pp + "⌝"
      case RELit(d) => "⟨" + d.toString + "⟩"
      case REPlus(r1, r2) => ppInfix("|", LEVEL_PLUS, ASSOC_LEFT, r1, r2)
      case REComp(r1, r2) => ppInfix("", LEVEL_COMP, ASSOC_LEFT, r1, r2)
      case REStar(r1) => ppPostfix("*", r1)

    private def ppCharSet(a: CharSet): String =
      if a.isEmpty then "∅"
      else if a.isFull then "."
      else if a.isSingleton then a.chars.head.toString
      else a.toString

    override protected def ppInfixOp(op: String): String = op

  extension [T, D](r: RegEx[T, D])
    def pp: String =
      val prettifier = REPrettifier[T, D]()
      prettifier.ppExpr(r)