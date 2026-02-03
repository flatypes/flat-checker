package flat.checker.py

import flat.Issuer
import flat.checker.ast
import flat.checker.ast.ArithOp.ADD
import flat.checker.ast.CmpOp.*
import flat.checker.py.ast.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class BodyChecker(using issuer: Issuer, gCtx: GCtx, returnType: ast.Type, vm: VarManager):
  def checkBody(body: Seq[LocalStmt], ctx: LCtx)(using insideLoop: Boolean): (List[ast.Stmt], LCtx) =
    val out = ListBuffer.empty[ast.Stmt]
    val checker = Checker(out)
    val newCtx = body.foldLeft(ctx) { (c, s) => s.accept(checker, c) }
    (out.toList, newCtx)

  private class Checker(out: ListBuffer[ast.Stmt])(using insideLoop: Boolean) extends NodeVisitor[LCtx, LCtx]:
    private val annotChecker = new AnnotChecker

    import annotChecker.*

    private val exprChecker = ExprChecker(out)
    import exprChecker.*

    private def checkAssign(left: Expr, right: ast.Expr, typ: ast.Type, ctx: LCtx): LCtx =
      left match
        case name@Name(x) =>
          ctx.get(x) match
            case Some(id) => // assignment
              out += ast.Assign(id, right)
            case None => // declaration
              val id = declare(name.asIdent, typ, ctx)
              out += ast.Assign(id, right)
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
            case ast.StrSort =>
              val id = vm.declare(ast.StrSort)
              out += ast.Assign(id, e)
              out += ast.Assert(ast.Cmp(EQ,
                ast.Length(ast.Var(id).withSort(ast.StrSort)),
                ast.Const(values.length)).fillLocation(node.loc))
              var newCtx = ctx
              for i <- values.indices do
                newCtx = checkAssign(values(i),
                  ast.CharAt(ast.Var(id).withSort(ast.StrSort), ast.Const(i)).fillLocation(values(i).loc),
                  // NOTE: to skip checking the binder has type char
                  ast.StrSort, newCtx)
              return newCtx
            case ast.TupleSort(_) if node.value.isInstanceOf[TupleExpr] =>
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
              out += ast.Assign(id, e)
            case None => // declaration
              val (t, e) = inferType(node.value, ctx)
              val id = declare(name.asIdent, t, ctx)
              out += ast.Assign(id, e)
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
                      ast.Assign(id, e)
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
            out += ast.Assign(id, e)
          ctx + (node.ident.name -> id)
        case Some(_) =>
          issuer.report(Redefined(node.ident))
          ctx

    private def declare(ident: Ident, typ: ast.Type, ctx: LCtx): String = vm.declare(typ, ident)

    override def visitAssert(node: Assert, ctx: LCtx): LCtx =
      val e = checkType(node.test, ast.BoolSort, ctx)
      out += ast.Assert(e)
      ctx

    override def visitRaise(node: Raise, ctx: LCtx): LCtx =
      out += ast.Assert(ast.Const(false).fillLocation(node.loc))
      out += ast.Return()
      ctx

    override def visitPass(node: Pass, ctx: LCtx): LCtx =
      ctx

    override def visitIf(node: If, ctx: LCtx): LCtx =
      val e = checkType(node.test, ast.BoolSort, ctx)
      val (b1, ctx1) = checkBody(node.body, ctx)
      val (b2, ctx2) = checkBody(node.orElse, ctx1)
      out += ast.IfStmt(e, ast.mkStmtList(b1), ast.mkStmtList(b2))
      ctx2

    override def visitWhile(node: While, ctx: LCtx): LCtx =
      val e = checkType(node.test, ast.BoolSort, ctx)
      val (invNodes, realBody) = extractInv(node.body, Nil)
      val (b, newCtx) = checkBody(realBody, ctx)(using insideLoop = true)
      if b.last.isInstanceOf[ast.Break] then // this while loop is just an if-statement
        out += ast.IfStmt(e, ast.mkStmtList(b.dropRight(1)), ast.Skip())
        if invNodes.nonEmpty then
          issuer.report(TypeError("No loop invariant expected here", invNodes.head.loc))
      else
        val loop = ast.While(e, ast.mkStmtList(b))
        val inv = for expr <- invNodes yield checkType(expr, ast.BoolSort, ctx, ignorePre = true)
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
          if vm.getType(id) != ast.IntSort then
            issuer.report(TypeMismatch("int", vm.getType(id).show, node.target.loc))
          ctx
        case None => // declaration
          val id = declare(node.target.asIdent, ast.IntSort, ctx)
          ctx + (node.target.id -> id)
      val i = ast.Var(node.target.id).withSort(ast.IntSort)
      // i = start
      out += ast.Assign(i.name, checkType(node.start, ast.IntSort, ctx))
      val (invNodes, realBody) = extractInv(node.body, Nil)
      val (b, newCtx) = checkBody(realBody, bodyCtx)(using insideLoop = true)
      val inv = for expr <- invNodes yield checkType(expr, ast.BoolSort, bodyCtx, ignorePre = true)
      // while i < end:
      //   ...
      //   i = i + step
      val loop = ast.While(ast.Cmp(LT, i, checkType(node.end, ast.IntSort, ctx)).setLocation(node.end.loc),
        ast.mkStmtList(b :+ ast.Assign(i.name,
          ADD(i, checkType(node.step, ast.IntSort, ctx)).setLocation(node.step.loc))))
      loop.invariants ++= inv
      out += loop
      newCtx

    override def visitBreak(node: Break, ctx: LCtx): LCtx =
      if insideLoop then
        out += ast.Break()
      else
        issuer.report(SyntaxError("'break' outside loop", node.loc))
      ctx

    override def visitReturn(node: Return, ctx: LCtx): LCtx =
      if returnType == ast.UnitSort then
        if node.value.isDefined then
          issuer.report(TypeError("return value in function with no return type", node.loc))
        ast.mkUnit
      else
        val e = node.value match
          case Some(expr) =>
            val e = checkType(expr, returnType, ctx)
            out += ast.Assign("return", e)
          case None =>
            issuer.report(TypeError("missing return value", node.loc))
      out += ast.Return()
      ctx

    override def visitExprStmt(node: ExprStmt, ctx: LCtx): LCtx =
      node.expr match
        case Call(Name("show_type"), Seq(arg)) =>
          val e = checkType(arg, ast.StrSort, ctx)
          out += ast.ShowType(e)
          ctx
        case Call(Name("assume"), Seq(arg)) =>
          val e = checkType(arg, ast.BoolSort, ctx, ignorePre = true)
          out += ast.Assume(e)
          ctx
        case Call(Name("hint"), Seq(arg)) =>
          val e = checkType(arg, ast.BoolSort, ctx, ignorePre = true)
          out += ast.Hint(e)
          ctx
        case _ =>
          val (t, e) = inferType(node.expr, ctx)
          val freshId = vm.declare(t)
          out += ast.Assign(freshId, e)
          ctx

    override def visitFunctionDef(node: FunctionDef, ctx: LCtx): LCtx =
      if node.ident.name.startsWith("lemma") then
        val argNames = node.args.map(_.ident.name)
        for
          i <- argNames.indices
          x = argNames(i)
          if argNames.take(i - 1).contains(x)
        do issuer.report(Redefined(node.args(i).ident))
        val argTypes = for arg <- node.args yield checkAnnot(arg.annotation, gCtx)
        val binders = List.from(for (Arg(a, _), t) <- node.args zip argTypes yield VarInfo(t, a))
        val lCtx = ctx ++ Map.from(for x <- argNames yield x -> x)
        vm.withBinder(binders):
          node.body match
            case Seq(Return(Some(e))) =>
              val vars = List.from(for (x, t) <- argNames zip argTypes yield ast.Var(x).withSort(t.base))
              val body = checkType(e, ast.BoolSort, lCtx, ignorePre = true)
              val lemma = ast.Forall(vars, body).copyLocation(node.ident)
              out += ast.Lemma(node.ident.name, lemma)
      else
        issuer.report(Unsupported("nested function definition", node.loc))
      ctx

    override def visitBlock(node: Block, ctx: LCtx): LCtx =
      val (ss, newCtx) = checkBody(node.body, ctx)
      out ++= ss
      newCtx