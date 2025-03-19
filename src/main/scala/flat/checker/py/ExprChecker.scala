package flat.checker.py

import flat.checker.py.ast.*
import flat.checker.{Issuer, Location, Sort, ast}
import org.apache.commons.text.StringEscapeUtils.unescapeJava

type LCtx = Map[String, Int]

class ExprChecker(using issuer: Issuer, gCtx: GCtx, vm: VarManager):
  def inferType(expr: Expr, ctx: LCtx): (ast.Type, ast.Expr) = expr.accept(InferMode, ctx)

  def checkType(expr: Expr, expected: ast.Type, ctx: LCtx): ast.Expr = expr.accept(CheckMode, (expected, ctx))

  private object InferMode extends NodeVisitor[LCtx, (ast.Type, ast.Expr)]:
    override def visitConstant(node: Constant, ctx: LCtx): (ast.Type, ast.Expr) =
      node.value match
        case v: Int => (ast.intType, ast.Const(v).copyLocation(node))
        case v: Boolean => (ast.boolType, ast.Const(v).copyLocation(node))
        case v: String => (ast.strType, ast.Const(unescapeJava(v)).copyLocation(node))

    override def visitTupleExpr(node: TupleExpr, ctx: LCtx): (ast.Type, ast.Expr) =
      val (ts, es) = (for value <- node.values yield value.accept(this, ctx)).unzip
      (ast.TupleType(ts), ast.TupleExpr(es))

    override def visitName(node: Name, ctx: LCtx): (ast.Type, ast.Expr) =
      val x = node.id
      ctx.get(x) match
        case Some(id) => (vm.getType(id), ast.Var(id).copyLocation(node))
        case None =>
          gCtx.get(x) match
            case Some(info: FunInfo) =>
              (info.funType, ast.GlobalRef(x).copyLocation(node))
            case Some(info: TypeInfo) =>
              issuer.report(TypeError("expect a term, but found type", node.loc))
              (ast.NoType, ast.NoExpr)
            case None =>
              issuer.report(Undefined(node.asIdent))
              (ast.NoType, ast.NoExpr)

    override def visitSubscript(node: Subscript, ctx: LCtx): (ast.Type, ast.Expr) =
      node.index match
        case Slice(l, u) =>
          val args = Seq(l.getOrElse(Constant(0))) ++ u.toSeq
          checkMemberCall(node.value, "__getitem_slice__", args, node.loc, ctx)
        case arg: Expr =>
          checkMemberCall(node.value, "__getitem__", Seq(arg), node.loc, ctx)

    private def checkMemberCall(receiver: Expr, member: String, args: Seq[Expr],
                                nodeLoc: Location, ctx: LCtx): (ast.Type, ast.Expr) =
      val (t, e) = receiver.accept(this, ctx)
      selectMember(t.toSort, member) match
        case Some(m) =>
          val es = checkArgs(nodeLoc, args, m.required, m.optional, ctx)
          (m.returns, m(e +: es).setLocation(nodeLoc))
        case None =>
          issuer.report(NoAttribute(t.show, member, nodeLoc))
          (ast.NoType, ast.NoExpr)

    override def visitCall(node: Call, ctx: LCtx): (ast.Type, ast.Expr) =
      node.func match
        case Name(f) if !ctx.contains(f) && !gCtx.contains(f) =>
          node.args match
            case Seq(arg) =>
              val (t, e) = arg.accept(this, ctx)
              selectMember(t.toSort, s"__${f}__") match
                case Some(m) =>
                  assert(m.required.isEmpty && m.optional.isEmpty)
                  (m.returns, m(Seq(e)).copyLocation(node))
                case None =>
                  issuer.report(NoAttribute(t.show, s"__${f}__", node.loc))
                  (ast.NoType, ast.NoExpr)
            case other =>
              issuer.report(TypeError(s"function $f takes exactly one argument", node.loc))
              (ast.NoType, ast.NoExpr)
        case Attribute(receiver, f) =>
          checkMemberCall(receiver, f, node.args, node.loc, ctx)
        case expr =>
          val (te, e) = expr.accept(this, ctx)
          te match
            case ast.FunType(ts, t) =>
              val es = for (arg, tArg) <- node.args zip ts yield arg.accept(CheckMode, (tArg, ctx))
              (t, ast.Apply(e, es).copyLocation(node))
            case _ =>
              issuer.report(TypeMismatch("Callable", te.show, expr.loc))
              (ast.NoType, ast.NoExpr)

    private def checkArgs(nodeLoc: Location, args: Seq[Expr], required: Seq[Sort],
                          optional: Seq[(Sort, ast.Expr)], ctx: LCtx): Seq[ast.Expr] =
      if args.length < required.length then
        issuer.report(TypeError(s"missing ${required.length - args.length} required positional arguments", nodeLoc))
      else if args.length > required.length + optional.length then
        issuer.report(TypeError(
          s"""too many arguments
             |expected: ${if optional.nonEmpty then "as most " else ""}${required.length + optional.length}"
             |actual:   ${args.length}"
             |""".stripMargin, nodeLoc))
      for (arg, s) <- args zip (required ++ optional.map(_._1)) yield arg.accept(CheckMode, (s, ctx))

    override def visitIfExp(node: IfExp, ctx: LCtx): (ast.Type, ast.Expr) =
      val e = node.test.accept(CheckMode, (ast.boolType, ctx))
      val (t1, e1) = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(CheckMode, (t1, ctx))
      (t1, ast.Ite(e, e1, e2).copyLocation(node))

  private object CheckMode extends NodeVisitor[(ast.Type, LCtx), ast.Expr]:
    override def visitIfExp(node: IfExp, ctx: (ast.Type, LCtx)): ast.Expr =
      val e = node.test.accept(this, ctx)
      val e1 = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(this, ctx)
      ast.Ite(e, e1, e2).copyLocation(node)

    override def visitDefault(node: Node, arg: (ast.Type, LCtx)): ast.Expr =
      val (expected, ctx) = arg
      val (actual, e) = node.accept(InferMode, ctx)
      if !(actual.toSort :<: expected.toSort) then
        issuer.report(TypeMismatch(expected.show, actual.show, node.loc))
      e
