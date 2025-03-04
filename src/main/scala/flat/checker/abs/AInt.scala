package flat.checker.abs

final case class AInt(range: Range, cnfIndex: Option[(List[ReLang], Int)])

object AInt:
  def fromRange(range: Range): AInt = AInt(range, None)

  def fromIndex(cnf: List[ReLang], k: Int): AInt =
    AInt(analysis.measureLength(ReLang.fromCNF(cnf.take(k))), Some(cnf, k))

given AbsDom[AInt]:
  import AInt.*

  def top: AInt = fromRange(Range.full)

  def bot: AInt = fromRange(Range.empty)

  def subElement(i1: AInt, i2: AInt): Boolean = i1.range :<: i2.range

  def join(i1: AInt, i2: AInt): AInt =
    if i1 == i2 then i1 else fromRange(i1.range | i2.range)

  def widen(i1: AInt, i2: AInt): AInt =
    if i1 == i2 then i1 else fromRange(i1.range ∇ i2.range)
