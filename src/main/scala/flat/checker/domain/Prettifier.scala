package flat.checker.domain

import flat.checker.domain.RegEx.*
import flat.checker.util.ExprPrettifier

object Prettifier:
  private class REPrettifier[A] extends ExprPrettifier[RegEx[A]]:
    private val LEVEL_COMP = 30
    private val LEVEL_PLUS = 40

    override protected def getLevel(r: RegEx[A]): Int = r match
      case Zero() | One() | Lit(_) => 0
      case Star(_) => LEVEL_POSTFIX
      case Comp(_, _) => LEVEL_COMP
      case Plus(_, _) => LEVEL_PLUS

    override def ppExpr(r: RegEx[A]): String = r match
      case Zero() => "∅"
      case One() => "ε"
      case Lit(a: CharSet) => ppCharSet(a)
      case Lit(r1: RegEx[_]) => "⌜" + r1.pp + "⌝"
      case Lit(d) => "⟨" + d.toString + "⟩"
      case Plus(r1, r2) => ppInfix("|", LEVEL_PLUS, ASSOC_LEFT, r1, r2)
      case Comp(r1, r2) => ppInfix("", LEVEL_COMP, ASSOC_LEFT, r1, r2)
      case Star(r1) => ppPostfix("*", r1)

    private def ppCharSet(a: CharSet): String =
      if a.isEmpty then "∅"
      else if a.isFull then "Σ"
      else if a.isSingleton then a.chars.head.toString
      else a.toString

    override protected def ppInfixOp(op: String): String = op

  extension [A](r: RegEx[A])
    def pp: String =
      val prettifier = REPrettifier[A]()
      prettifier.ppExpr(r)