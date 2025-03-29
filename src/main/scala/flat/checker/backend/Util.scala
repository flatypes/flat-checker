package flat.checker.backend

object Util:
  private val unicodeSubscripts: String = "₀₁₂₃₄₅₆₇₈₉"

  def renderSubscript(k: Int): String =
    require(k >= 0)
    k.toString.map(c => unicodeSubscripts(c - '0'))
