package flat.checker

import flat.checker.ReLang.*

object ReLangOps:
  def concat(r1: ReLang, r2: ReLang): ReLang = ReConcat(r1, r2)

  /** Given a string `s` in language `r` and an integer `k`,
   * return the estimation of `s.charAt(k)` as a CharSet, or `None` if `k` might be out of bounds. */
  def charAt(r: ReLang, k: Int): Option[CharSet] =
    if k < 0 then return charAt(r.reverse, -k - 1)

    var i = 0
    var l: Option[ReLang] = Some(r)
    while i < k && l.isDefined do
      l = l.get.derivative(CharSet.full)
      i += 1
    l match
      case Some(r) =>
        val cs = r.first
        if cs.isEmpty then None else Some(cs)
      case None => None

  def charAt(r: ReLang, fromIndex: Int, toIndex: Int): CharSet =
    require(fromIndex >= 0)
    var i = 0
    var l: Option[ReLang] = Some(r)
    while i < fromIndex && l.isDefined do
      l = l.get.derivative(CharSet.full)
      i += 1
    i = 0
    if toIndex >= 0 then
      var cs = CharSet.empty
      while i <= toIndex - fromIndex && l.isDefined do
        cs |= l.get.first
        l = l.get.derivative(CharSet.full)
        i += 1
      cs
    else // toIndex < 0
      l = l.map(_.reverse)
      while i < -toIndex - 1 && l.isDefined do
        l = l.get.derivative(CharSet.full)
      i += 1
      l.map(_.alphabet).getOrElse(CharSet.empty)

  def startsWith(r: ReLang, prefix: String): Ternary =
    var i = 0
    var l: Option[ReLang] = Some(r)
    var matches = true
    while i < prefix.length && l.isDefined do
      val c = prefix.charAt(i)
      if matches && !(l.get.first.isSingleton && l.get.first.contains(c)) then
        matches = false
      l = l.get.derivative(CharSet.of(c))
      i += 1
    if l.isEmpty then Ternary.False
    else if matches then Ternary.True
    else Ternary.Maybe

  def endsWith(r: ReLang, suffix: String): Ternary = startsWith(r.reverse, suffix.reverse)