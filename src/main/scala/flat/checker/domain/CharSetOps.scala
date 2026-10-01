package flat.checker.domain

object CharSetOps:
  extension (a: CharSet)
    def absIn(b: CharSet): BoolSet =
      if a.pos && b.pos then
        if a.chars.subsetOf(b.chars) then BoolSet.True
        else if (a.chars & b.chars).isEmpty then BoolSet.False
        else BoolSet.Full
      else throw UnsupportedOperationException("Cannot check absIn on a negative CharSet")
