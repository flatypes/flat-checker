package flat.checker.domain

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class CharSetTest extends AnyPropSpec, TableDrivenPropertyChecks:
  private val alphabet = Set('a', 'b', 'c', 'A', 'B', 'C')

  private val examples = Seq.from:
    for chars <- alphabet.subsets; pos <- Seq(true, false) yield CharSet(pos, chars)

  private val sets = Table("set", examples *)

  private val setPairs = Table(("left", "right"), (for a <- examples; b <- examples yield (a, b)) *)

  private val setsWithChars = Table(("set", "char"), (for a <- examples; c <- alphabet yield (a, c)) *)

  property("subset"):
    forAll(setPairs.filter { (a, b) => a.pos || !b.pos }):
      (a, b) =>
        val set1 = a.toFinSet(alphabet)
        val set2 = b.toFinSet(alphabet)
        assertResult(set1.subsetOf(set2))(a.subsetOf(b))

  property("union"):
    forAll(setPairs):
      (a, b) =>
        val set1 = a.toFinSet(alphabet)
        val set2 = b.toFinSet(alphabet)
        val actual = (a | b).toFinSet(alphabet)
        assertResult(set1 | set2)(actual)

  property("intersection"):
    forAll(setPairs):
      (a, b) =>
        val set1 = a.toFinSet(alphabet)
        val set2 = b.toFinSet(alphabet)
        val actual = (a & b).toFinSet(alphabet)
        assertResult(set1 & set2)(actual)

  property("complement"):
    forAll(sets):
      a =>
        val set = a.toFinSet(alphabet)
        val actual = (~a).toFinSet(alphabet)
        assertResult(alphabet -- set)(actual)

  property("contains"):
    forAll(setsWithChars):
      (a, c) =>
        val set = a.toFinSet(alphabet)
        assertResult(set.contains(c))(a.contains(c))

  property("remove"):
    forAll(setsWithChars):
      (a, c) =>
        val set = a.toFinSet(alphabet)
        val actual = (a - c).toFinSet(alphabet)
        assertResult(set - c)(actual)