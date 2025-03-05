package flat.checker.abs

import flat.checker.abs.ReLang.*

object ReLangOps:
  def concat(r1: ReLang, r2: ReLang): ReLang = ReConcat(r1, r2)

  /** Given a string `s` in language `r` and an integer `k`,
   * return the estimation of `s.charAt(k)` as a CharSet, or `None` if `k` might be out of bounds. */
  def charAt(r: ReLang, k: Int): Option[CharSet] =
    var i = 0
    var l: Option[ReLang] = Some(r)
    while i <= k && l.isDefined do
      l = l.get.derivative(CharSet.full)
      i += 1
    l.map(_.first)

  def startsWith(r: ReLang, prefix: String): ABool =
    var i = 0
    var l: Option[ReLang] = Some(r)
    var matches = true
    while i < prefix.length && l.isDefined do
      val c = prefix.charAt(i)
      if matches && !(l.get.first.isSingleton && l.get.first.contains(c)) then
        matches = false
      l = l.get.derivative(CharSet.of(c))
      i += 1
    if l.isEmpty then ABool.False
    else if matches then ABool.True
    else ABool.Top

  def endsWith(r: ReLang, suffix: String): ABool = startsWith(r.reverse, suffix.reverse)