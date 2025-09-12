package flat.regex

sealed trait AIndex

/** Represents the absolute index from left, i.e., `k` itself. */
final case class AIndexL(k: Int) extends AIndex:
  require(k >= 0, "negative index")

/**
 * Represents the absolute index from right, i.e., `s.length - k` for a string `s`.
 *
 * @note `IndexR(1)` represents the last index `s.length - 1`
 * @note `IndexR(0)` represents the end of the string (that is out of the bound)
 */
final case class AIndexR(k: Int) extends AIndex:
  require(k >= 0, "negative index")

/** Represents the relative index whose base is the first occurrence of the nonempty string `t`. */
final case class AIndexAt(t: String) extends AIndex:
  require(t.nonEmpty, "empty pattern")
