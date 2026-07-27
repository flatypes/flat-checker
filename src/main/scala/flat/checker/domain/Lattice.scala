package flat.checker.domain

/** Complemented Lattice */
trait Lattice[T]:
  def top: T

  def bot: T

  extension (x: T)
    def isEmpty: Boolean

    def nonEmpty: Boolean = !isEmpty

    /** Partial order. Tests if `x` ⊑ `y`. */
    def subsetOf(y: T): Boolean

    /** Join operation. */
    def |(y: T): T

    /** Meet operation. */
    def &(y: T): T

    /** Complement. */
    def unary_~ : T
