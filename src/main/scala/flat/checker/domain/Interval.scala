package flat.checker.domain

final case class Interval(min: BigInt, max: BigInt):
  def isEmpty: Boolean = min > max

  def nonEmpty: Boolean = min <= max

  def contains(i: BigInt): Boolean = min <= i && i <= max

  def |(that: Interval): Interval = Interval(min min that.min, max max that.max)

  def &(that: Interval): Interval = Interval(min max that.min, max min that.max)

  def unary_- : Interval = Interval(-max, -min)

  def +(i: BigInt): Interval = Interval(min + i, max + i)

  def -(i: BigInt): Interval = Interval(min - i, max - i)

  def *(i: BigInt): Interval =
    if i >= 0 then Interval(min * i, max * i)
    else Interval(max * i, min * i)

  def /(i: BigInt): Interval =
    require(i != 0, "Division by zero")
    val candidates = Seq(min / i, max / i)
    Interval(candidates.min, candidates.max)

  def %(i: BigInt): Interval =
    require(i > 0, "Modulo by non-positive integer")
    val modMin = min % i
    val modMax = max % i
    if min >= 0 then
      if max - min >= i + 1 then Interval(0, i - 1)
      else if modMin <= modMax then Interval(modMin, modMax)
      else Interval(0, modMin max modMax)
    else if max <= 0 then
      if max - min >= i + 1 then Interval(-(i - 1), 0)
      else if modMin <= modMax then Interval(modMin, modMax)
      else Interval(modMin min modMax, 0)
    else
      val finalMin = if (0 - min) >= i - 1 then -(i - 1) else modMin
      val finalMax = if (max - 0) >= i - 1 then i - 1 else modMax
      Interval(finalMin, finalMax)
