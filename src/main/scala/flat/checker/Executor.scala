//package flat.checker
//
//import com.typesafe.scalalogging.LazyLogging
//import flat.Ops.CmpOp.*
//import flat.checker.ast.*
//import flat.checker.ast.ExprSimplifier.*
//import flat.regex.CharSet
//import flat.{Config, Issuer}
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//
//final case class EvalCtx private(values: Map[String, Expr], versions: Map[String, Int]):
//  def apply(x: String): Expr = values(x)
//
//  def updated(x: String, v: Expr): EvalCtx = copy(values = values + (x -> v))
//
//  def havoc(x: String): EvalCtx =
//    require(!x.contains(':'))
//    val n = versions(x) + 1
//    copy(values = values + (x -> Var(s"$x:$n")), versions = versions + (x -> n))
//
//object EvalCtx:
//  def apply(programVars: Set[String]): EvalCtx =
//    EvalCtx(values = Map.from(for x <- programVars yield x -> Var(s"$x:0")),
//      versions = Map.from(for x <- programVars yield x -> 0))
//
///** Symbolic Executor */
//class Executor(funDef: FunDef)(using config: Config, issuer: Issuer) extends LazyLogging:
//  def exec(): Int =
//    Analyzer.guessInvariants(funDef.body)
//    val ctx = EvalCtx(funDef.programVars)
//    for ep <- funDef.requires do
//      prover.assume(eval(ep, ctx))
//    exec(funDef.body, ctx)
//    failed
//
//  private val loopStack = mutable.Stack.empty[While]
//
//  private def exec(body: List[Stmt], ctx: EvalCtx): Unit = body match
//    case Nil => // end of either a loop body or the whole function body
//      if loopStack.nonEmpty then checkInv(ctx) else checkPost(ctx)
//    case Assign(x, e) :: rest =>
//      if funDef.params.collectFirst { case Decl(`x`, _) => () }.isDefined then
//        throw RuntimeException("cannot assign to parameter")
//      exec(rest, ctx.updated(x, eval(e, ctx)))
//    case Assert(e) :: rest =>
//      val cond = eval(e, ctx)
//      check(cond, "assert", ctx)
//      if cond != Const(false) then
//        exec(rest, ctx)
//    case Assume(e) :: rest =>
//      prover.assume(eval(e, ctx))
//      exec(rest, ctx)
//    case ShowType(e) :: rest =>
//      exec(rest, ctx)
//    case Hint(_) :: rest =>
//      exec(rest, ctx)
//
//    case IfStmt(e, thenBody, elseBody) :: rest =>
//      val cond = eval(e, ctx)
//      // then branch
//      prover.push()
//      prover.assume(cond)
//      exec(thenBody ++ rest, ctx)
//      prover.pop()
//      // else branch
//      prover.push()
//      prover.assume(Not(cond))
//      exec(elseBody ++ rest, ctx)
//      prover.pop()
//
//    case (loop@While(e, loopBody)) :: rest =>
//      for ei <- loop.invariants do
//        val inv = eval(ei, ctx)
//        check(inv, "inv (on entry)", ctx)
//      // havoc loop variables
//      val ctx1 = Analyzer.collectModifiedVars(loopBody).foldLeft(ctx)(_.havoc(_))
//      val cond = eval(e, ctx1)
//      // execute loop body
//      prover.push()
//      prover.assume(cond)
//      for ei <- loop.invariants do
//        prover.assume(eval(ei, ctx1))
//      loopStack.push(loop)
//      exec(loopBody, ctx1)
//      val info = loopStack.pop()
//      prover.pop()
//      // execute rest
//      val es = Analyzer.collectBreakConds(loopBody)
//      val exit = mkOr(And(Not(cond) :: loop.invariants.toList.map(eval(_, ctx1))) :: es.map(eval(_, ctx1)))
//      // NOTE: may repeatedly prove side conditions
//      prover.assume(exit)
//      exec(rest, ctx1)
//
//    case Break() :: _ =>
//    case Continue() :: _ => checkInv(ctx)
//    case Return() :: _ => checkPost(ctx)
//
//  private def checkInv(ctx: EvalCtx): Unit =
//    val loopInfo = loopStack.top
//    for ei <- loopInfo.invariants do
//      val inv = eval(ei, ctx)
//      check(inv, "inv", ctx)
//
//  private def checkPost(ctx: EvalCtx): Unit =
//    for ep <- funDef.ensures do
//      val post = eval(ep, ctx)
//      check(post, "post", ctx)
//
//  private val verifier = new Verifier
//  private val prover = new verifier.Prover(using VarCtx(funDef.lCtx), funDef.sortingContext)
//  private var nextGoal = 1
//  private var failed = 0
//
//  private def check(cond: Expr, kind: String, ctx: EvalCtx): Unit =
//    logger.info("")
//    logger.info(s"Goal {}: {}\n{}", nextGoal, kind, verifier.ppGoal(prover.getCtx.premises, cond))
//    prover.prove(cond) match
//      case Left(_) => failed += 1
//      case _ =>
//    nextGoal += 1
//
//  private def eval(expr: Expr, ctx: EvalCtx): Expr =
//    expr.transform:
//      case Var(x) => ctx(x) // program variable
//      case And(bs) =>
//        val conjuncts = ListBuffer(eval(bs.head, ctx))
//        // eval bs(i) assuming bs(0), ..., bs(i-1)
//        prover.push()
//        for b <- bs.tail do
//          prover.assume(conjuncts.last)
//          conjuncts += eval(b, ctx)
//        prover.pop()
//        And(conjuncts.toList)
//      case Implies(b1, b2) =>
//        val left = eval(b1, ctx)
//        // eval b2 assuming b1
//        prover.push()
//        prover.assume(b1)
//        val right = eval(b2, ctx)
//        prover.pop()
//        Implies(left, right)
//      case Ite(b, e1, e2) =>
//        val cond = eval(b, ctx)
//        // eval e1 assuming b
//        prover.push()
//        prover.assume(cond)
//        val thenValue = eval(e1, ctx)
//        prover.pop()
//        // eval e2 assuming not b
//        prover.push()
//        prover.assume(Not(cond))
//        val elseValue = eval(e2, ctx)
//        prover.pop()
//        Ite(cond, thenValue, elseValue)
//
//      case CharFromCode(e) => // 0 <= e <= 0xFFFF
//        val code = eval(e, ctx)
//        check(And(LE(Const(0), code), LE(code, Const(0xFFFF))), "code range", ctx)
//        CharFromCode(code)
//
//      case CharAt(e, ei) => // 0 <= ei < |e|
//        val str = eval(e, ctx)
//        val idx = eval(ei, ctx)
//        check(And(LE(Const(0), idx), LT(idx, StringLength(str))), "index", ctx)
//        CharAt(str, idx)
//      case Substring(es, ei, ej) => // 0 <= ei and 0 <= ej
//        val str = eval(es, ctx)
//        val startIdx = eval(ei, ctx)
//        startIdx match
//          case DefaultExpr(_) => // skip
//          case _ => check(LE(Const(0), startIdx), "index", ctx)
//        val endIdx = eval(ej, ctx)
//        endIdx match
//          case DefaultExpr(_) => // skip
//          case _ => check(LE(Const(0), endIdx), "index", ctx)
//        Substring(str, startIdx, endIdx)
//      case StringToInt(e, base) => // e is a string representing an integer in the given base
//        val str = eval(e, ctx)
//        check(StringForall(str, Lambda(List(Decl("c", CharSort)), RefinedBy(Var("c"), CharSet.intDigit(base)))),
//          "pre (toInt)", ctx)
//        StringToInt(str, base)
//
//      case SeqGet(e, ei) => // 0 <= ei < |e|
//        val seq = eval(e, ctx)
//        val idx = eval(ei, ctx)
//        check(And(LE(Const(0), idx), LT(idx, SeqLength(seq))), "index", ctx)
//        SeqGet(seq, idx)
//      case SeqSlice(es, ei, ej) => // 0 <= ei and 0 <= ej
//        val seq = eval(es, ctx)
//        val startIdx = eval(ei, ctx)
//        startIdx match
//          case DefaultExpr(_) => // skip
//          case _ => check(LE(Const(0), startIdx), "index", ctx)
//        val endIdx = eval(ej, ctx)
//        endIdx match
//          case DefaultExpr(_) => // skip
//          case _ => check(LE(Const(0), endIdx), "index", ctx)
//        SeqSlice(seq, startIdx, endIdx)
//
//      case MapGet(e, ek) => // e contains key ek
//        val map = eval(e, ctx)
//        val key = eval(ek, ctx)
//        check(MapContains(map, key), "key", ctx)
//        MapGet(map, key)
//      case MapRemove(e, ek) => // e contains key ek
//        val map = eval(e, ctx)
//        val key = eval(ek, ctx)
//        check(MapContains(map, key), "key", ctx)
//        MapRemove(map, key)