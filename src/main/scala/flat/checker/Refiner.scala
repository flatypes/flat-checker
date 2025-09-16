package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Config
import flat.Ops.CmpOp.*
import flat.checker.core.*
import flat.regex.*
import flat.regex.AOps.*
import flat.regex.RegEx.*

import scala.collection.mutable

object Refiner extends LazyLogging:
  private val m = mutable.HashMap.empty[Expr, RegEx]

  def refine(ctx: PrfCtx)(using config: Config): PrfCtx =
    m.clear()
    /* for
      x <- ctx.types.strVars
      e -> r <- refineByLength(x)(using ctx)
    do m(e) = r */
    ctx.foreach { cond => for e -> r <- refine(cond)(using ctx) do m(e) = r }
    if m.nonEmpty then
      for e -> r <- m do logger.debug(s"refine $e : $r")
    ctx ++ List.from(for e -> r <- m yield TypeTest(e, LangType(r)))

  private def refineByLength(x: String)(using ctx: PrfCtx): Option[(Expr, RegEx)] =
    // Collect length constraints
    val selected = Array.fill(ctx.assumptions.length)(false)
    val relatedVars = mutable.Set.empty[String]
    // 1. Select all `Cmp`s containing |x|
    for i <- ctx.assumptions.indices do
      ctx.assumptions(i) match
        case cmp@Cmp(op, _, _) if op != NE && cmp.collectFirst { case Length(Var(x)) if x == x => () }.isDefined =>
          selected(i) = true
          relatedVars ++= (cmp.collectVars - x)
        case _ =>
    // 2. Select all other `Cmp`s containing any related vars, until reaching the fixpoint
    var changed = relatedVars.nonEmpty
    while changed do
      changed = false
      for i <- ctx.assumptions.indices do
        if !selected(i) then
          ctx.assumptions(i) match
            case cmp@Cmp(op, _, _) if op != NE && (cmp.collectVars & relatedVars.toSet).nonEmpty =>
              selected(i) = true
              relatedVars ++= (cmp.collectVars - x)
              changed = true
            case _ =>
    val constraints = for
      i <- selected.indices.toList
      if selected(i)
    yield ctx.assumptions(i).asInstanceOf[Cmp]

    val solver = LPSolver(constraints)
    val (min, max) = solver.solve(Length(Var(x)))
    val interval = Interval(min.getOrElse(0), max.getOrElse(Inf))
    for r <- getNonEmptyLang(x) yield (Var(x) -> RERefiner.refineByLen(r, interval))

  private def refine(assumption: Expr)(using ctx: PrfCtx, config: Config): Option[(Expr, RegEx)] =
    assumption match
      case Cmp(op, Length(Var(x)), Const(n: Int)) =>
        for r <- getNonEmptyLang(x) yield (Var(x) -> RERefiner.refineByLen(r, (op, n)))
      case Cmp(op@(EQ | NE), CharAt(Var(x), ei), Const(s: String)) =>
        val c = ensureChar(s)
        for
          r <- getNonEmptyLang(x)
          reft <- refineByCharAt(Var(x), r, ei, op, c)
        yield reft
      case Cmp(op, Find(Var(x), Const(s: String)), Const(n: Int)) if s.length == 1 =>
        val c = ensureChar(s)
        for r <- getNonEmptyLang(x) yield Var(x) -> RERefiner.refineByIndexOf(r, c, (op, n))
      case Cmp(EQ, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield Var(x) -> refineByEqual(r, s)
      case Cmp(NE, Var(x), Const(s: String)) =>
        for r <- getNonEmptyLang(x) yield Var(x) -> refineByNotEqual(r, s)
      case _ => None

  private def getNonEmptyLang(varName: String)(using ctx: PrfCtx): Option[RegEx] =
    val r = m.get(varName) match
      case Some(r) => r
      case None => ctx.getLang(Var(varName))
    if r.isEmpty then None else Some(r)

  private def ensureChar(s: String): Char =
    require(s.length == 1)
    s.head

  private def refineByCharAt(str: Expr, lang: RegEx, idx: Expr, op: CmpOp, c: Char)
                            (using ctx: PrfCtx, config: Config): Option[(Expr, RegEx)] =
    val solution = for
      (base, r) <- ctx.lookupSuffixLang(str)
      k <- Rewriter.tryGetConstDiff(idx, base)
      if k >= 0
    yield Substr(str, base, Length(str)) -> RERefiner.refineByCharAt(r, k, (op, c))
    solution.orElse {
      val indexInferer = new IndexInferer
      indexInferer.infer(idx, str) match
        case IndexL(k) =>
          Some(str -> RERefiner.refineByCharAt(lang, k, (op, c)))
        case IndexR(k) if k > 0 =>
          Some(str -> RERefiner.refineByCharAt(lang.reverse, k - 1, (op, c)).reverse)
        case IndexInterval(_, _) if op == NE =>
          Some(str -> refineBySomeCharNotEqual(lang, c))
        case _ => None
    }

  private def refineBySomeCharNotEqual(re: RegEx, c: Char): RegEx = re match
    case REUnion(r1, r2) => union(List(r1, r2).filter(r => (r.alphabet & CharSet(false, Set(c))).nonEmpty))
    case _ => re

  private def refineByIsNull(re: RegEx): RegEx =
    if re.nullable then RegEx.RENull else RENone

  private def refineByEqual(re: RegEx, s: String): RegEx =
    if re.contains(s) then RegEx.fromString(s) else RENone

  private def refineByNotEqual(re: RegEx, s: String): RegEx = s.length match
    case 0 => RERefiner.refineByLen(re, (GT, 0))
    case 1 => RERefiner.refineByCharAt(re, 0, (NE, s.head))
    case _ if re.first == CharSet.of(s.head) =>
      concat(fromChar(s.head), refineByNotEqual(re.drop1, s.tail))
    case _ => AOps.tryEnumerate(re) match
      case Some(words) => union(List.from(words - s).map(fromString))
      case None => re