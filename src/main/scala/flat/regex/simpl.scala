package flat.regex

import flat.regex.RegEx.*

object simpl:
  /** Longest common prefix. */
  private def lcp[T](xss: List[List[T]])(using eq: (T, T) => Boolean): List[T] = xss match
    case Nil => throw IllegalArgumentException("empty cases")
    case _ =>
      if xss.forall(_.nonEmpty) then
        val xs = xss.map(_.head)
        if xs.size == 1 || xs.tail.forall(eq(xs.head, _)) then xs.head :: lcp(xss.map(_.tail))
        else Nil
      else Nil

  /** Longest common suffix. */
  private def lcs[T](xss: List[List[T]])(using eq: (T, T) => Boolean): List[T] = xss match
    case Nil => throw IllegalArgumentException("empty cases")
    case _ => lcp(xss.map(_.reverse)).reverse

  extension (re: RegEx)
    def parts: List[RegEx] = re match
      case RENone => throw IllegalArgumentException(re.toString)
      case RENull => Nil
      case REConcat(r1, r2) => r1.parts ++ r2.parts
      case _ => List(re)

    def cases: List[RegEx] = re match
      case RENone => Nil
      case REUnion(r1, r2) => r1.cases ++ r2.cases
      case _ => List(re)

    infix def equalsRE(other: RegEx): Boolean =
      if re == other then true
      else (re, other) match
        case (_: REConcat, _: REConcat) =>
          val rs1 = re.parts
          val rs2 = other.parts
          rs1.length == rs2.length && rs1.zip(rs2).forall(_ equalsRE _)
        case (_: REUnion, _: REUnion) =>
          val rs1 = re.cases.sortBy(_.toString)
          val rs2 = other.cases.sortBy(_.toString)
          rs1.length == rs2.length && rs1.zip(rs2).forall(_ equalsRE _)
        case _ => false

    infix def equalsRE(regex: String): Boolean = re equalsRE REParser.parse(regex)

    def extractCommonFactor: RegEx =
      val rss = re.cases.map(_.parts)
      if rss.isEmpty then RENone
      else if rss.length == 1 then concat(rss.head)
      else
        val prefix = lcp(rss)(using _ equalsRE _)
        val k1 = prefix.length
        val suffix = lcs(rss.map(_.drop(k1)))(using _ equalsRE _)
        val k2 = suffix.length
        concat(prefix ++ List(union(rss.map(_.drop(k1).dropRight(k2)).map(concat))) ++ suffix)