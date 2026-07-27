package flat.checker.domain

trait Domain[T, D] extends Lattice[D]:
  def mkSingleton(x: T): D

  extension (d: D)
    def contains(x: T): Boolean

    def representative: T

    def -(x: T): D
