package flat.checker.util

trait ExprPrettifier[E]:
  protected val LEVEL_POSTFIX = 10
  protected val LEVEL_PREFIX = 20

  protected def getLevel(e: E): Int

  // Associativity
  protected val ASSOC_NONE = 0
  protected val ASSOC_LEFT = 1
  protected val ASSOC_RIGHT = 2

  private val openParen: String = "("

  private val closeParen: String = ")"

  private inline def parenIf(cond: Boolean, s: String): String =
    if cond then openParen + s + closeParen else s

  def ppExpr(expr: E): String

  protected def ppPrefix(op: String, expr: E): String =
    op + parenIf(getLevel(expr) > LEVEL_PREFIX, ppExpr(expr))

  protected def ppPostfix(op: String, expr: E): String =
    parenIf(getLevel(expr) > LEVEL_POSTFIX, ppExpr(expr)) + op

  protected def ppInfixOp(op: String): String = " " + op + " "

  protected def ppInfix(op: String, level: Int, assoc: Int, left: E, right: E): String =
    val leftLevel = getLevel(left)
    val rightLevel = getLevel(right)
    val s1 = parenIf(leftLevel > level || (leftLevel == level && assoc != ASSOC_LEFT), ppExpr(left))
    val sOp = ppInfixOp(op)
    val s2 = parenIf(rightLevel > level || (rightLevel == level && assoc != ASSOC_RIGHT), ppExpr(right))
    s1 + sOp + s2