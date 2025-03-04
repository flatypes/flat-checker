package flat.checker.frontend.python

import flat.checker.ast.StmtBlock
import flat.checker.frontend.python.ast.*
import flat.checker.{Issuer, Location, Sort, ast, SyntaxError, TypeError}
import org.apache.commons.text.StringEscapeUtils.unescapeJava

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class LocalContext(vars: Map[String, ast.Type], val returnType: ast.Type):
  def contains(name: String): Boolean = vars.contains(name)

  def get(name: String): Option[ast.Type] = vars.get(name)

  def updated(name: String, typ: ast.Type): LocalContext = LocalContext(vars + (name -> typ), returnType)

  def updated(items: Seq[(String, ast.Type)]): LocalContext = LocalContext(vars ++ items, returnType)

final class LocalChecker(override val issuer: Issuer, globalCtx: GlobalContext) extends AnnotChecker:
  def check(body: Seq[Stmt], ctx: LocalContext): ast.StmtBlock =
    val visitor = new StmtVisitor
    body.foldLeft(ctx) { (c, s) => s.accept(visitor, c) }
    visitor.done

  private class StmtVisitor extends NodeVisitor[LocalContext, LocalContext]:
    private val varBuf = ListBuffer.empty[(String, ast.Type)]
    private val stmtBuf = ListBuffer.empty[ast.Stmt]

    def done: ast.StmtBlock =
      val declares = for (x, t) <- varBuf yield ast.Declare(x, t)
      ast.StmtBlock(declares.toSeq ++ stmtBuf.toSeq)

    override def visitAssign(node: Assign, ctx: LocalContext): LocalContext =
      node.target match
        case Name(x) =>
          ctx.get(x) match
            case Some(t) => // write
              val e = node.value.accept(CheckMode, (t, ctx))
              stmtBuf += ast.Assign(x, e)
              ctx
            case None => // declare a new variable
              val (t, e) = node.value.accept(InferMode, ctx)
              varBuf += x -> t
              stmtBuf += ast.Assign(x, e)
              ctx.updated(x, t)
        case Subscript(receiver, index) if !index.isInstanceOf[Slice] =>
          val (t, e) = receiver.accept(InferMode, ctx)
          selectMember(t.toSort, "__setitem__") match
            case Some(m) =>
              val es = checkArgs(node.loc, Seq(index, node.value), m.required, m.optional, ctx)
              writeValue(receiver, m(e +: es), ctx)
              ctx
            case None =>
              issuer.report(AttributeError(receiver.loc, "__setitem__", t.show))
              ctx
        case _ =>
          issuer.report(UnsupportedFeature(node.target.loc))
          ctx

    @tailrec
    private def writeValue(target: Node, value: ast.Expr, ctx: LocalContext): LocalContext =
      target match
        case Name(x) =>
          stmtBuf += ast.Assign(x, value)
          ctx
        case Subscript(receiver, index) if !index.isInstanceOf[Slice] =>
          val (t, e) = receiver.accept(InferMode, ctx)
          selectMember(t.toSort, "__setitem__") match
            case Some(m) =>
              val (_, i) = index.accept(InferMode, ctx)
              writeValue(receiver, m(Seq(e, i, value)), ctx)
            case None =>
              issuer.report(AttributeError(receiver.loc, "__setitem__", t.show))
              ctx
        case _ => ctx

    override def visitAnnAssign(node: AnnAssign, ctx: LocalContext): LocalContext =
      node.target match
        case Name(x) =>
          ctx.get(x) match
            case Some(t) => // write
              issuer.report(TypeError(node.annotation.loc, Seq("annotation is not allowed for reassignment")))
              ctx
            case None => // declare a new variable
              val t = checkAnnot(node.annotation, globalCtx)
              val e = node.value.accept(CheckMode, (t, ctx))
              varBuf += x -> t
              stmtBuf += ast.Assign(x, e)
              ctx.updated(x, t)
        case _ =>
          issuer.report(UnsupportedFeature(node.target.loc))
          ctx

    override def visitAssert(node: Assert, ctx: LocalContext): LocalContext =
      val e = node.test.accept(CheckMode, (ast.BoolType, ctx))
      stmtBuf += ast.Assert(e)
      ctx

    override def visitPass(node: Pass, ctx: LocalContext): LocalContext = ctx

    private def extractLocals(stmtBlock: StmtBlock): Map[String, ast.Type] = stmtBlock.body.collect {
      case ast.Declare(x, t) => x -> t
    }.toMap

    override def visitIf(node: If, ctx: LocalContext): LocalContext =
      val e = node.test.accept(CheckMode, (ast.BoolType, ctx))
      val b1 = check(node.body, ctx)
      val b2 = check(node.orElse, ctx)
      val locals1 = extractLocals(b1)
      val locals2 = extractLocals(b2)
      val xs = (locals1.keySet & locals2.keySet).filter(x => locals1(x) == locals2(x))
      val m = Seq.from(for x <- xs yield x -> locals1(x))
      varBuf ++= m
      val thenBlock = ast.StmtBlock(b1.body)
      val elseBlock = ast.StmtBlock(b2.body)
      stmtBuf += ast.IfStmt(e, thenBlock, elseBlock)
      ctx.updated(m)

    override def visitWhile(node: While, ctx: LocalContext): LocalContext =
      val e = node.test.accept(CheckMode, (ast.BoolType, ctx))
      val block = check(node.body, ctx)
      stmtBuf += ast.While(e, block)
      ctx

    override def visitReturn(node: Return, ctx: LocalContext): LocalContext =
      node.value match
        case Some(expr) =>
          val e = expr.accept(CheckMode, (ctx.returnType, ctx))
          stmtBuf += ast.Return(e)
        case None => throw UnsupportedOperationException("missing return value")
      ctx

    override def visitExprStmt(node: ExprStmt, ctx: LocalContext): LocalContext =
      val (_, e) = node.expr.accept(InferMode, ctx)
      stmtBuf += ast.Assign(None, e)
      ctx

    override def visitDefault(node: Node, ctx: LocalContext): LocalContext =
      issuer.report(UnsupportedFeature(node.loc))
      ctx

  private def checkArgs(nodeLoc: Location, args: Seq[Expr], required: Seq[Sort],
                        optional: Seq[(Sort, ast.Expr)], ctx: LocalContext): Seq[ast.Expr] =
    if args.length < required.length then
      issuer.report(TypeError(nodeLoc, Seq(s"missing ${required.length - args.length} required positional arguments")))
    else if args.length > required.length + optional.length then
      issuer.report(TypeError(nodeLoc, Seq(
        s"too many arguments",
        s"expected: ${if optional.nonEmpty then "as most " else ""}${required.length + optional.length}",
        s"actual:   ${args.length}"
      )))
    for (arg, s) <- args zip (required ++ optional.map(_._1)) yield arg.accept(CheckMode, (s, ctx))

  private object InferMode extends NodeVisitor[LocalContext, (ast.Type, ast.Expr)]:
    override def visitConstant(node: Constant, ctx: LocalContext): (ast.Type, ast.Expr) =
      node.value match
        case v: Int => (ast.IntType, ast.Literal(v).copyLocation(node))
        case v: Boolean => (ast.BoolType, ast.Literal(v).copyLocation(node))
        case v: String => (ast.StringType, ast.Literal(unescapeJava(v)).copyLocation(node))

    override def visitListExpr(node: ListExpr, ctx: LocalContext): (ast.Type, ast.Expr) =
      node.values match
        case Seq() => (ast.ArrayType(ast.NoType), ast.apply(ast.Op.ARRAY_MK).copyLocation(node))
        case head +: tail =>
          val (t, e) = head.accept(this, ctx)
          val es = for value <- tail yield value.accept(CheckMode, (t, ctx))
          (ast.ArrayType(t), ast.Apply(ast.Op.ARRAY_MK, e +: es).copyLocation(node))

    import GItem.Func

    override def visitName(node: Name, ctx: LocalContext): (ast.Type, ast.Expr) =
      val x = node.id
      ctx.get(x) match
        case Some(t) => (t, ast.LocalRef(x).copyLocation(node))
        case None =>
          globalCtx.get(x) match
            case Some(Func(args, returns)) =>
              (ast.FunType(args.map(_._2), returns), ast.GlobalRef(x).copyLocation(node))
            case Some(_) =>
              issuer.report(TypeError(node.loc, Seq("expect a term")))
              (ast.NoType, ast.NoExpr)
            case None =>
              issuer.report(UndefinedName(node.loc))
              (ast.NoType, ast.NoExpr)

    override def visitUnaryOp(node: UnaryOp, ctx: LocalContext): (ast.Type, ast.Expr) =
      val (t, e) = node.operand.accept(this, ctx)
      selectMember(t.toSort, node.op.name) match
        case Some(m) =>
          assert(m.required.isEmpty && m.optional.isEmpty)
          (m.returns, m(Seq(e)).copyLocation(node))
        case None =>
          issuer.report(AttributeError(node.loc, node.op.name, t.show))
          (ast.NoType, ast.NoExpr)

    override def visitBinOp(node: BinOp, ctx: LocalContext): (ast.Type, ast.Expr) =
      checkInfix(node.loc, node.left, node.right, node.op, ctx)

    import OpBool.*

    override def visitBoolOp(node: BoolOp, ctx: LocalContext): (ast.Type, ast.Expr) =
      node.op match
        case And => // bool and bool
          val es = node.values.map(_.accept(CheckMode, (ast.BoolType, ctx)))
          (ast.BoolType, es.reduce(ast.apply(ast.Op.AND, _, _)))
        case Or => // bool or bool
          val es = node.values.map(_.accept(CheckMode, (ast.BoolType, ctx)))
          (ast.BoolType, es.reduce(ast.apply(ast.Op.OR, _, _)))

    import OpCompare.*

    override def visitCompare(node: Compare, ctx: LocalContext): (ast.Type, ast.Expr) =
      val es =
        for ((left, right), op) <- (node.left +: node.comparators.dropRight(1)) zip node.comparators zip node.ops
          yield
            val loc = Location(left.loc.doc, left.loc.start, right.loc.end)
            op match
              case In => checkInfix(loc, right, left, op, ctx)._2
              case NotIn => ast.apply(ast.Op.NOT, checkInfix(loc, right, left, op, ctx)._2)
              case _ => checkInfix(loc, left, right, op, ctx)._2
      (ast.BoolType, es.reduce(ast.apply(ast.Op.AND, _, _)))

    private def checkInfix(nodeLoc: Location, left: Expr, right: Expr, op: Op,
                           ctx: LocalContext): (ast.Type, ast.Expr) =
      val (t1, e1) = left.accept(this, ctx)
      selectMember(t1.toSort, op.name) match
        case Some(m) =>
          val es = checkArgs(nodeLoc, Seq(right), m.required, m.optional, ctx)
          (m.returns, m(e1 +: es).setLocation(nodeLoc))
        case None =>
          issuer.report(AttributeError(nodeLoc, op.name, t1.show))
          (ast.NoType, ast.NoExpr)

    override def visitCall(node: Call, ctx: LocalContext): (ast.Type, ast.Expr) =
      node.func match
        case Name(f) if !ctx.contains(f) && !globalCtx.contains(f) && builtInFuncs.contains(f) =>
          node.args match
            case Seq(arg) =>
              val (t, e) = arg.accept(this, ctx)
              selectMember(t.toSort, s"__${f}__") match
                case Some(m) =>
                  assert(m.required.isEmpty && m.optional.isEmpty)
                  (m.returns, m(Seq(e)).copyLocation(node))
                case None =>
                  issuer.report(AttributeError(node.loc, s"__${f}__", t.show))
                  (ast.NoType, ast.NoExpr)
            case other =>
              issuer.report(TypeError(node.loc, Seq(s"function $f takes exactly one argument")))
              (ast.NoType, ast.NoExpr)
        case Attribute(receiver, f) =>
          val (t, e) = receiver.accept(this, ctx)
          selectMember(t.toSort, f) match
            case Some(m) =>
              val es = checkArgs(node.loc, node.args, m.required, m.optional, ctx)
              (m.returns, m(e +: es).copyLocation(node))
            case None =>
              issuer.report(AttributeError(node.loc, f, t.show))
              (ast.NoType, ast.NoExpr)
        case expr =>
          val (te, e) = expr.accept(this, ctx)
          te match
            case ast.FunType(ts, t) =>
              val es = for (arg, tArg) <- node.args zip ts yield arg.accept(CheckMode, (tArg, ctx))
              (t, ast.Apply(e, es).copyLocation(node))
            case _ =>
              issuer.report(TypeMismatch(expr.loc, "Callable", te.show))
              (ast.NoType, ast.NoExpr)

    override def visitIfExp(node: IfExp, ctx: LocalContext): (ast.Type, ast.Expr) =
      val e = node.test.accept(CheckMode, (ast.BoolType, ctx))
      val (t1, e1) = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(CheckMode, (t1, ctx))
      (t1, ast.IfExpr(e, e1, e2).copyLocation(node))

    override def visitSubscript(node: Subscript, ctx: LocalContext): (ast.Type, ast.Expr) =
      val (t, e) = node.value.accept(this, ctx)
      selectMember(t.toSort, "__getitem__") match
        case Some(m) =>
          node.slice match
            case Slice(lower, upper, step) =>
              val m = selectMember(t.toSort, "__getitem_slice__").get
              if step.isDefined then
                issuer.report(SyntaxError(step.get.loc, Seq("step is not allowed")))
              val es = checkArgs(node.loc, Seq(lower.getOrElse(Constant(0)), upper.getOrElse(Constant(-1))),
                m.required, m.optional, ctx)
              (m.returns, m(e +: es).copyLocation(node))
            case index =>
              val es = checkArgs(node.loc, Seq(index), m.required, m.optional, ctx)
              (m.returns, m(e +: es).copyLocation(node))
        case None =>
          issuer.report(AttributeError(node.loc, "__getitem__", t.show))
          (ast.NoType, ast.NoExpr)

  private object CheckMode extends NodeVisitor[(ast.Type, LocalContext), ast.Expr]:
    override def visitListExpr(node: ListExpr, arg: (ast.Type, LocalContext)): ast.Expr =
      val (te, ctx) = arg
      te match
        case ast.ArrayType(t) =>
          val es = for value <- node.values yield value.accept(this, (t, ctx))
          ast.Apply(ast.Op.ARRAY_MK, es).copyLocation(node)
        case actual =>
          issuer.report(TypeMismatch(node.loc, "list", actual.show))
          ast.NoExpr

    override def visitIfExp(node: IfExp, ctx: (ast.Type, LocalContext)): ast.Expr =
      val e = node.test.accept(this, ctx)
      val e1 = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(this, ctx)
      ast.IfExpr(e, e1, e2).copyLocation(node)

    override def visitDefault(node: Node, arg: (ast.Type, LocalContext)): ast.Expr =
      val (expected, ctx) = arg
      val (actual, e) = node.accept(InferMode, ctx)
      if !(actual.toSort :<: expected.toSort) then
        issuer.report(TypeMismatch(node.loc, expected.show, actual.show))
      e
