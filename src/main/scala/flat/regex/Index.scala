package flat.regex

trait Index

sealed trait BasicIndex extends Index

/** Represents the absolute index from left, i.e., `k` itself. */
final case class IndexL(k: Int) extends BasicIndex:
  require(k >= 0, "negative index")

/**
 * Represents the absolute index from right, i.e., `s.length - k` for a string `s`.
 *
 * @note `IndexR(1)` represents the last index `s.length - 1`
 * @note `IndexR(0)` represents the end of the string (that is out of the bound)
 */
final case class IndexR(k: Int) extends BasicIndex:
  require(k >= 0, "negative index")

/** Represents the relative index whose base is the first occurrence of the nonempty string `t`. */
final case class IndexAt(t: String) extends BasicIndex:
  require(t.nonEmpty, "empty pattern")
