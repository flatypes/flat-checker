package flat

import scala.annotation.tailrec

object util:
  def cartesianProduct[T](left: Set[T], right: Set[T], p: (T, T) => T): Set[T] =
    for x1 <- left; x2 <- right yield p(x1, x2)

  def cartesianPower[T](set: Set[T], k: Int, p: (T, T) => T): Set[T] =
    require(k >= 1)
    if k == 1 then set
    else cartesianProduct(cartesianPower(set, k - 1, p), set, p)

  @tailrec
  def tryAll[T](attempts: List[() => Option[T]]): Option[T] = attempts match
    case Nil => None
    case first :: rest => first() match
      case some@Some(_) => some
      case None => tryAll(rest)

  def tryAll[T](attempts: (() => Option[T])*): Option[T] = tryAll(attempts.toList)

  private val unicodeSubscripts: String = "₀₁₂₃₄₅₆₇₈₉"

  def renderSubscript(k: Int): String =
    require(k >= 0)
    k.toString.map(c => unicodeSubscripts(c - '0'))

  private val mxBean =
    java.lang.management.ManagementFactory.getPlatformMXBean(classOf[java.lang.management.ThreadMXBean])

  /** Time an action and return the time elapsed in ms. */
  def time[R](f: => R): (Double, R) =
    val t0 = mxBean.getCurrentThreadCpuTime
    val r = f
    val t1 = mxBean.getCurrentThreadCpuTime
    ((t1 - t0) / 1.0e6, r)
