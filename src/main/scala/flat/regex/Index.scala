package flat.regex

/** Abstract Index. */
trait Index

/** Basic Abstract Index. */
sealed trait BasicIndex extends Index

/** Absolute index from left: `k`. */
final case class IndexL(k: Int) extends BasicIndex:
  require(k >= 0, s"negative index $k")

/**
 * Absolute index from right: |s| - `k`.
 *
 * @note `IndexR(1)` is the last index |s| - 1
 * @note `IndexR(0)` is the end of the string |s|
 */
final case class IndexR(k: Int) extends BasicIndex:
  require(k >= 0, s"negative index $k")

/** Relative index that is the first occurrence of the nonempty pattern `t`, i.e., s.find(`t`). */
final case class IndexAt(t: String) extends BasicIndex:
  require(t.nonEmpty, "empty pattern")
