package flat.checker

trait Lattice[T]:
  def top: T

  def bot: T

  def subElement(x: T, y: T): Boolean

  def meet(x: T, y: T): T

  def join(x: T, y: T): T

trait AbsDom[T] extends Lattice[T]:
  def widen(x: T, y: T): T

extension [T: Lattice as lat](x: T)
  def :<:(y: T): Boolean = lat.subElement(x, y)
  def |(y: T): T = lat.join(x, y)
  def &(y: T): T = lat.meet(x, y)

extension [T: AbsDom as dom](x: T)
  def ∇(y: T): T = dom.widen(x, y)

def kleene[T: AbsDom as dom](f: (T => T) => T => T)(t: T): T =
  var acc: T = dom.bot

  def iter(t: T): T =
    if t :<: acc then acc
    else
      acc = dom.widen(acc, t)
      f(iter)(acc)

  iter(t)
