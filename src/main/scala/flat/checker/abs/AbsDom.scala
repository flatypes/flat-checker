package flat.checker.abs

trait AbsDom[T]:
  def top: T

  def bot: T

  def subElement(x: T, y: T): Boolean

  def join(x: T, y: T): T

  def widen(x: T, y: T): T

extension [T: AbsDom as dom](x: T)
  def :<:(y: T): Boolean = dom.subElement(x, y)
  def |(y: T): T = dom.join(x, y)
  def ∇(y: T): T = dom.widen(x, y)

def kleene[T: AbsDom as dom](f: (T => T) => T => T)(t: T): T =
  var acc: T = dom.bot

  def iter(t: T): T =
    if t :<: acc then acc
    else
      acc = dom.widen(acc, t)
      f(iter)(acc)

  iter(t)
