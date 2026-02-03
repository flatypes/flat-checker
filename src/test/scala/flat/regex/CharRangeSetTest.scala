package flat.regex

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class CharRangeSetTest extends AnyPropSpec, TableDrivenPropertyChecks:
  private val set1 = CharRangeSet.of('0' -> '3', '5', '8' -> '9')
  private val chars = List('.', '/', ':', ';') ++ (for c <- '0' to '9' yield c)
  private val testChars = Table("c", chars *)

  property("contains"):
    forAll(testChars): c =>
      val actual = set1.contains(c)
      val expected = set1.toSet.contains(c)
      assert(actual == expected)

  private val sets = List(CharRangeSet.empty, CharRangeSet.of('A' -> 'Z')) ++
    (for c <- chars yield CharRangeSet.of(c)) ++
    (for c1 <- chars; c2 <- chars if c1 < c2 yield CharRangeSet.of(c1 -> c2))
  private val testSets = Table("set2", sets *)

  property("isSubsetOf"):
    forAll(testSets): set2 =>
      val actual = set1.isSubsetOf(set2)
      val expected = set1.toSet.subsetOf(set2.toSet)
      assert(actual == expected)

  property("union"):
    forAll(testSets): set2 =>
      val actual = (set1 | set2).toSet
      val expected = set1.toSet | set2.toSet
      assert(actual == expected)

  property("intersection"):
    forAll(testSets): set2 =>
      val actual = (set1 & set2).toSet
      val expected = set1.toSet & set2.toSet
      assert(actual == expected)

  property("minus"):
    forAll(testSets): set2 =>
      val actual = (set1 -- set2).toSet
      val expected = set1.toSet -- set2.toSet
      assert(actual == expected)
