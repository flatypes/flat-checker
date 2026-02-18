package flat.checker.py

import flat.checker.ast as ir
import flat.checker.py.Type.*
import flat.checker.py.ast.*
import flat.{Issuer, Location}
import org.apache.commons.text.StringEscapeUtils.unescapeJava

import scala.collection.mutable.ListBuffer

type LCtx = Map[String, String]

class ExprChecker(out: ListBuffer[ir.Stmt])(using issuer: Issuer, gCtx: GCtx, vm: VarManager):
  def inferType(expr: Expr, ctx: LCtx, ignorePre: Boolean = false): (Type, ir.Expr) =
    expr.accept(InferMode(using ignorePre), ctx)

  def checkType(expr: Expr, expected: Type, ctx: LCtx, ignorePre: Boolean = false): ir.Expr =
    expr.accept(CheckMode(using ignorePre), (expected, ctx))

  private val annotChecker = new AnnotChecker

  import annotChecker.*

  private class InferMode(using ignorePre: Boolean) extends NodeVisitor[LCtx, (Type, ir.Expr)]:
    override def visitConstant(node: Constant, ctx: LCtx): (Type, ir.Expr) =
      node.value match
        case v: Int => (IntType, ir.Const(v).copyLocation(node))
        case v: Boolean => (BoolType, ir.Const(v).copyLocation(node))
        case v: String => (StringType, ir.Const(unescapeJava(v)).copyLocation(node))
        case null =>
          issuer.report(TypeError(s"unsupported constant type: ${node.value.getClass}", node.loc))
          (NoType, ir.NoExpr)

    override def visitTupleExpr(node: TupleExpr, ctx: LCtx): (Type, ir.Expr) =
      val (ts, es) = (for value <- node.values yield value.accept(this, ctx)).unzip
      (TupleType(ts.toList), ir.TupleOf(es.toList).copyLocation(node))

    override def visitDictExpr(node: DictExpr, ctx: LCtx): (Type, ir.Expr) =
      if node.keys.isEmpty then
        (MapType(NoType, NoType), ir.MapOf(Nil).copyLocation(node))
      else
        var t = NoType
        val items = for (k, v) <- node.keys zip node.values yield
          val (tk, ek) = k.accept(this, ctx)
          val (tv, ev) = v.accept(this, ctx)
          t = MapType(tk, tv)
          ir.TupleOf(List(ek, ev)).copyLocation(node)
        (t, ir.MapOf(items.toList).copyLocation(node))

    override def visitName(node: Name, ctx: LCtx): (Type, ir.Expr) =
      val x = node.id
      ctx.get(x) match
        case Some(id) =>
          val t = vm.getType(id)
          (t, ir.Var(id).copyLocation(node))
        case None =>
          gCtx.get(x) match
            case Some(info: FunInfo) =>
              val decl = ir.Decl(x, info.funType.toSort)
              (info.funType, ir.Global(decl).copyLocation(node))
            case Some(info: TypeInfo) =>
              issuer.report(TypeError("expect a term, but found type", node.loc))
              (NoType, ir.NoExpr)
            case None =>
              issuer.report(Undefined(node.asIdent))
              (NoType, ir.NoExpr)

    override def visitSubscript(node: Subscript, ctx: LCtx): (Type, ir.Expr) =
      node.index match
        case Slice(l, u) =>
          val args = Seq(l.getOrElse(Constant(0).setLocation(node.index.loc))) ++ u.toSeq
          checkMemberCall(node.value, "__getitem_slice__", args, node.loc, ctx)
        case arg: Expr =>
          checkMemberCall(node.value, "__getitem__", Seq(arg), node.loc, ctx)

    private def checkMemberCall(receiver: Expr, member: String, args: Seq[Expr],
                                nodeLoc: Location, ctx: LCtx): (Type, ir.Expr) =
      val (t, e) = receiver.accept(this, ctx)
      selectMember(t.base, member) match
        case Some(m) =>
          val es = checkArgs(nodeLoc, args, m.required, m.optional, ctx)
          if !ignorePre && m.preCond.isDefined then
            out += ir.Assert(m.applyPre(e +: es).fillLocation(nodeLoc))
          val value = m.sideEffect match
            case Some(f) =>
              val y = vm.declare(m.returns)
              out += ir.Assign(y, m.apply(e +: es).setLocation(nodeLoc))
              e match
                case ir.Var(x) =>
                  out += ir.Assign(x, f.apply(e +: es).setLocation(nodeLoc))
                case _ =>
                  issuer.report(Unsupported("in-place update on non-variable", receiver.loc))
              ir.Var(y)
            case None => m.apply(e +: es).setLocation(nodeLoc)
          (m.returns, value)
        case None =>
          issuer.report(NoAttribute(t.show, member, nodeLoc))
          (NoType, ir.NoExpr)

    override def visitCall(node: Call, ctx: LCtx): (Type, ir.Expr) =
      node.func match
        case Name(f) if !ctx.contains(f) && !gCtx.contains(f) =>
          node.args match
            case Seq(obj, annot) if f == "isinstance" =>
              val t = checkAnnot(annot, gCtx)
              val (base, domain) = t.split
              val e = obj.accept(CheckMode(), (base, ctx))
              domain match
                case Some(d) =>
                  (BoolType, ir.RefinedBy(e, d).fillLocation(node.loc))
                case _ =>
                  (BoolType, ir.Const(true).copyLocation(node))

            case Seq(e1, e2) if f == "implies" =>
              val premise = e1.accept(CheckMode(), (BoolType, ctx))
              val conclusion = e2.accept(CheckMode(), (BoolType, ctx))
              (BoolType, ir.mkImplies(premise, conclusion).copyLocation(node))
            case Seq(fun, arr) if f == "map" =>
              val (tf, ef) = fun.accept(InferMode(), ctx)
              tf match
                case FunType(List(t1), t2) =>
                  val ea = arr.accept(CheckMode(), (ListType(t1), ctx))
                  val e = ir.SeqMap(ea, ef).copyLocation(node)
                  (ListType(t2), e)
                case other =>
                  issuer.report(TypeError(s"expected a function, but found $other", fun.loc))
                  (NoType, ir.NoExpr)
            case es if es.nonEmpty =>
              checkMemberCall(es.head, s"__${f}__", es.tail, node.loc, ctx)
            case _ =>
              issuer.report(TypeError(s"function $f takes arguments", node.loc))
              (NoType, ir.NoExpr)
        case Attribute(Name("int"), f) =>
          intModuleTable.get(f) match
            case Some(m) =>
              val es = checkArgs(node.loc, node.args, m.required, m.optional, ctx)
              if !ignorePre && m.preCond.isDefined then
                out += ir.Assert(m.applyPre(es).fillLocation(node.loc))
              (m.returns, m.apply(ir.NoExpr +: es).setLocation(node.loc))
            case None =>
              issuer.report(NoAttribute("int", f, node.loc))
              (NoType, ir.NoExpr)
        case Attribute(receiver, f) =>
          checkMemberCall(receiver, f, node.args, node.loc, ctx)
        case expr =>
          val (te, e) = expr.accept(this, ctx)
          te match
            case FunType(ts, t) =>
              val es = for (arg, tArg) <- node.args zip ts yield arg.accept(CheckMode(), (tArg, ctx))
              (t, ir.Apply(e, es.toList).copyLocation(node))
            case _ =>
              issuer.report(TypeMismatch("Callable", te.show, expr.loc))
              (NoType, ir.NoExpr)

    private def checkArgs(nodeLoc: Location, args: Seq[Expr], required: Seq[Type],
                          optional: Seq[(Type, ir.Expr)], ctx: LCtx): Seq[ir.Expr] =
      if args.length < required.length then
        issuer.report(TypeError(s"missing ${required.length - args.length} required positional arguments", nodeLoc))
      else if args.length > required.length + optional.length then
        issuer.report(TypeError(
          s"""too many arguments
             |expected: ${if optional.nonEmpty then "as most " else ""}${required.length + optional.length}"
             |actual:   ${args.length}"
             |""".stripMargin, nodeLoc))
      for (arg, s) <- args zip (required ++ optional.map(_._1)) yield arg.accept(CheckMode(), (s, ctx))

    override def visitIfExp(node: IfExp, ctx: LCtx): (Type, ir.Expr) =
      val e = node.test.accept(CheckMode(), (BoolType, ctx))
      val (t1, e1) = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(CheckMode(), (t1, ctx))
      (t1, ir.Ite(e, e1, e2).copyLocation(node))

  private class CheckMode(using ignorePre: Boolean) extends NodeVisitor[(Type, LCtx), ir.Expr]:
    override def visitIfExp(node: IfExp, ctx: (Type, LCtx)): ir.Expr =
      val e = node.test.accept(this, (BoolType, ctx._2))
      val e1 = node.body.accept(this, ctx)
      val e2 = node.orElse.accept(this, ctx)
      ir.Ite(e, e1, e2).copyLocation(node)

    override def visitDefault(node: Node, arg: (Type, LCtx)): ir.Expr =
      val (expected, ctx) = arg
      val (actual, e) = node.accept(InferMode(), ctx)
      val (actualBase, _) = actual.split
      val (expectedBase, expectedDomain) = expected.split
      if !(actualBase <= expectedBase) then
        issuer.report(TypeMismatch(expectedBase.show, actualBase.show, node.loc))
      for d <- expectedDomain do
        out += ir.Assert(ir.RefinedBy(e, d).fillLocation(node.loc))
      e
