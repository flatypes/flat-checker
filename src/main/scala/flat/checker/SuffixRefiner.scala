package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.checker.core.*
import flat.regex.*

class SuffixRefiner(using ctx: PrfCtx) extends LazyLogging:

  import CmpOp.*
  import RegExpr.*

  private val types = ctx.types

  def refineSuffix(baseStr: Expr, baseIdx: Expr, r0: RegExpr): RegExpr =
    var r = r0
    ctx.foreach {
      case Cmp(op@(EQ | NE), StrAt(es, ei), Const(s: String)) if es == baseStr && s.length == 1 =>
        for
          k <- Rewriter.tryGetConstDiff(ei, baseIdx)
          if k >= 0
        do r = RERefiner.refineByCharAt(r, k, (op, s.head))
      case _ =>
    }
    if r != r0 then
      logger.debug(s"refine $baseStr[$baseIdx:] $r0 as $r")
    r

  private def extractSuffixLang(str: Expr): Option[(Expr, RegExpr)] =
    ctx.collectFirst {
      case TypeTest(StrSlice(e1, ei, StrLen(e2)), LangType(r)) if e1 == str && e2 == str => (ei, r)
    }

  def refineBySomeCharNotEqual(re: RegExpr, c: Char): RegExpr = re match
    case REUnion(r1, r2) => mkUnion(List(r1, r2).filter(r => (REOps.alphabet(r) & CharSet(false, Set(c))).nonEmpty))
    case _ => re

  def refineByEqual(re: RegExpr, s: String): RegExpr =
    if s.isEmpty then
      if re.nullable then RENull else RENone
    else ???

  def refineByNotEqual(re: RegExpr, s: String): RegExpr = s.length match
    case 0 => RERefiner.refineByLen(re, (GT, 0))
    case 1 => RERefiner.refineByCharAt(re, 0, (NE, s.head))
    case _ if REOps.firstSet(re) == CharSet.of(s.head) =>
      mkConcat(fromChar(s.head), refineByNotEqual(REOps.drop(re, 1), s.tail))
    case _ => REOps.tryEnumerate(re) match
      case Some(words) => mkUnion(List.from(words - s).map(fromString))
      case None => re
