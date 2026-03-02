package flat.checker.py

import flat.Issuer
import flat.Ops.CmpOp.*
import flat.checker.ast as ir
import flat.checker.py.Type.*
import flat.checker.py.ast.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class BodyChecker(using issuer: Issuer, gCtx: GCtx, returnType: Type, vm: VarManager):
  def checkBody(body: Seq[LocalStmt], ctx: LCtx)(using insideLoop: Boolean): (List[ir.Stmt], LCtx) =
    val out = ListBuffer.empty[ir.Stmt]
    val checker = Checker(out)
    val newCtx = body.foldLeft(ctx) { (c, s) => s.accept(checker, c) }
    (out.toList, newCtx)

  private class Checker(out: ListBuffer[ir.Stmt])(using insideLoop: Boolean) extends NodeVisitor[LCtx, LCtx]:
    private val annotChecker = new AnnotChecker

    import annotChecker.*

    private val exprChecker = ExprChecker(out)
    import exprChecker.*

    private def checkAssign(left: Expr, right: ir.Expr, typ: Type, ctx: LCtx): LCtx =
      left match
        case name@Name(x) =>
          ctx.get(x) match
            case Some(id) => // assignment
              out += ir.Assign(id, right)
            case None => // declaration
              val id = declare(name.asIdent, typ, ctx)
              out += ir.Assign(id, right)
              return ctx + (x -> id)
        case _ =>
          ???
      ctx

    override def visitAssign(node: Assign, ctx: LCtx): LCtx =
      node.target match
        case ListExpr(_) =>
          issuer.report(Unsupported("list", node.target.loc))
        case TupleExpr(values) =>
          val (t, e) = inferType(node.value, ctx)
          t.base match
            case StringType =>
              val id = vm.declare(StringType)
              out += ir.Assign(id, e)
              out += ir.Assert(ir.RelExpr(EQ,
                ir.StringLength(ir.Var(id)),
                ir.Const(values.length)).fillLocation(node.loc))
              var newCtx = ctx
              for i <- values.indices do
                newCtx = checkAssign(values(i),
                  ir.CharAt(ir.Var(id), ir.Const(i)).fillLocation(values(i).loc),
                  // NOTE: to skip checking the binder has type char
                  StringType, newCtx)
              return newCtx
            case TupleType(_) if node.value.isInstanceOf[TupleExpr] =>
              val tupleValues = node.value.asInstanceOf[TupleExpr].values
              if tupleValues.length != values.length then
                issuer.report(TypeError("sizes of tuple on both sides do not match", node.loc))
              var newCtx = ctx
              for (x, e) <- values.zip(tupleValues) do
                val (et, ev) = inferType(e, newCtx)
                newCtx = checkAssign(x, ev, et, newCtx)
              return newCtx
            case _ =>
              issuer.report(Unsupported("tuple", node.target.loc))
        case name@Name(x) =>
          ctx.get(x) match
            case Some(id) => // assignment
              val e = checkType(node.value, vm.getType(id), ctx)
              out += ir.Assign(id, e)
            case None => // declaration
              val (t, e) = inferType(node.value, ctx)
              val id = declare(name.asIdent, t, ctx)
              out += ir.Assign(id, e)
              return ctx + (x -> id)
        case Attribute(_, _) =>
          issuer.report(Unsupported("attribute", node.target.loc))
        case Subscript(value, index) =>
          index match
            case expr: Expr =>
              value match
                case name@Name(x) =>
                  ctx.get(x) match
                    case Some(id) =>
                      val call = Call(Attribute(name, "__setitem__"), Seq(expr, node.value))
                      val (_, e) = inferType(call, ctx)
                      ir.Assign(id, e)
                    case None =>
                      issuer.report(Undefined(name.asIdent))
                case _ =>
                  issuer.report(Unsupported("nested left value", node.target.loc))
            case _ =>
              issuer.report(Unsupported("slice", index.loc))
        case _ => throw UnknownError()
      ctx

    override def visitAnnAssign(node: AnnAssign, ctx: LCtx): LCtx =
      // regard this kind of assignment as new variable declaration
      ctx.get(node.ident.name) match
        case None =>
          val t = checkAnnot(node.annotation, gCtx)
          val id = declare(node.ident, t, ctx)
          for value <- node.init do
            val e = checkType(value, vm.getType(id), ctx)
            out += ir.Assign(id, e)
          ctx + (node.ident.name -> id)
        case Some(_) =>
          issuer.report(Redefined(node.ident))
          ctx

    private def declare(ident: Ident, typ: Type, ctx: LCtx): String = vm.declare(typ, ident)

    override def visitAssert(node: Assert, ctx: LCtx): LCtx =
      val e = checkType(node.test, BoolType, ctx)
      out += ir.Assert(e)
      ctx

    override def visitRaise(node: Raise, ctx: LCtx): LCtx =
      out += ir.Assert(ir.Const(false).fillLocation(node.loc))
      out += ir.Return()
      ctx

    override def visitPass(node: Pass, ctx: LCtx): LCtx =
      ctx

    override def visitIf(node: If, ctx: LCtx): LCtx =
      val e = checkType(node.test, BoolType, ctx)
      val (b1, ctx1) = checkBody(node.body, ctx)
      val (b2, ctx2) = checkBody(node.orElse, ctx1)
      out += ir.IfStmt(e, b1, b2)
      ctx2

    override def visitWhile(node: While, ctx: LCtx): LCtx =
      val e = checkType(node.test, BoolType, ctx)
      val (invNodes, realBody) = extractInv(node.body, Nil)
      val (b, newCtx) = checkBody(realBody, ctx)(using insideLoop = true)
      if b.last.isInstanceOf[ir.Break] then // this while loop is just an if-statement
        out += ir.IfStmt(e, b.dropRight(1), Nil)
        if invNodes.nonEmpty then
          issuer.report(TypeError("No loop invariant expected here", invNodes.head.loc))
      else
        val loop = ir.While(e, b)
        val inv = for expr <- invNodes yield checkType(expr, BoolType, ctx, ignorePre = true)
        loop.invariants ++= inv
        out += loop
      newCtx

    @tailrec
    private def extractInv(loopBody: Seq[LocalStmt], acc: List[Expr]): (List[Expr], Seq[LocalStmt]) =
      loopBody match
        case Nil => (acc, Seq.empty)
        case ExprStmt(Call(Name("inv"), es)) :: ss => extractInv(ss, acc ++ es)
        case ss => (acc, ss)

    override def visitFor(node: For, ctx: LCtx): LCtx =
      val bodyCtx = ctx.get(node.target.id) match
        case Some(id) => // assignment
          if vm.getType(id) != IntType then
            issuer.report(TypeMismatch("int", vm.getType(id).show, node.target.loc))
          ctx
        case None => // declaration
          val id = declare(node.target.asIdent, IntType, ctx)
          ctx + (node.target.id -> id)
      val i = ir.Var(node.target.id)
      // i = start
      out += ir.Assign(i.name, checkType(node.start, IntType, ctx))
      val (invNodes, realBody) = extractInv(node.body, Nil)
      val (b, newCtx) = checkBody(realBody, bodyCtx)(using insideLoop = true)
      val inv = for expr <- invNodes yield checkType(expr, BoolType, bodyCtx, ignorePre = true)
      // while i < end:
      //   ...
      //   i = i + step
      val loop = ir.While(
        ir.RelExpr(LT, i, checkType(node.end, IntType, ctx)).setLocation(node.end.loc),
        b :+ ir.Assign(i.name,
          ir.Add(i, checkType(node.step, IntType, ctx)).setLocation(node.step.loc)))
      loop.invariants ++= inv
      out += loop
      newCtx

    override def visitBreak(node: Break, ctx: LCtx): LCtx =
      if insideLoop then
        out += ir.Break()
      else
        issuer.report(SyntaxError("'break' outside loop", node.loc))
      ctx

    override def visitReturn(node: Return, ctx: LCtx): LCtx =
      if returnType == UnitType then
        if node.value.isDefined then
          issuer.report(TypeError("return value in function with no return type", node.loc))
        ir.mkUnit
      else
        val e = node.value match
          case Some(expr) =>
            val e = checkType(expr, returnType, ctx)
            out += ir.Assign("return", e)
          case None =>
            issuer.report(TypeError("missing return value", node.loc))
      out += ir.Return()
      ctx

    override def visitExprStmt(node: ExprStmt, ctx: LCtx): LCtx =
      node.expr match
        case Call(Name("show_type"), Seq(arg)) =>
          val e = checkType(arg, StringType, ctx)
          out += ir.ShowType(e)
          ctx
        case Call(Name("assume"), Seq(arg)) =>
          val e = checkType(arg, BoolType, ctx, ignorePre = true)
          out += ir.Assume(e)
          ctx
        case _ =>
          val (t, e) = inferType(node.expr, ctx)
          val freshId = vm.declare(t)
          out += ir.Assign(freshId, e)
          ctx

    override def visitBlock(node: Block, ctx: LCtx): LCtx =
      val (ss, newCtx) = checkBody(node.body, ctx)
      out ++= ss
      newCtx