package flat.checker.domain

import flat.checker.domain.REOps.absMap

object CharREOps:
  given Domain[Char, Set[Char]] = ???

  extension (r: CharRE)
    def absToLower: CharRE = r.absMap(???)
