package flat.checker.py

import flat.Issuer
import flat.checker.ast.CmpOp.*
import flat.checker.py.ast.*
import flat.checker.{Sort, ast}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class BodyChecker(using issuer: Issuer, gCtx: GCtx, returnType: ast.Type, vm: VarManager):
  def checkBody(body: Seq[LocalStmt], ctx: LCtx, com: LCtx)(using insideLoop: Boolean): (List[ast.Stmt], LCtx) =
    val out = ListBuffer.empty[ast.Stmt]
    val checker = Checker(out)
    val newCtx = body.foldLeft(ctx) { (c, s) => s.accept(checker, (c, com)) }
    (out.toList, newCtx)

  private class Checker(out: ListBuffer[ast.Stmt])(using insideLoop: Boolean)
    extends NodeVisitor[(LCtx, LCtx), LCtx]:
    private val annotChecker = new AnnotChecker

    import annotChecker.*

    private val exprChecker = ExprChecker(out)
    import exprChecker.*

    private def checkAssign(left: Expr, right: ast.Expr, typ: ast.Type, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      left match
        case name@Name(x) =>
          ctx.get(x) match
            case Some(id) => // assignment
              out += ast.Assign(id, right)
            case None => // declaration
              val id = declare(name.asIdent, typ, com)
              out += ast.Assign(id, right)
              return ctx + (x -> id)
        case _ =>
          ???
      ctx

    override def visitAssign(node: Assign, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      node.target match
        case ListExpr(_) =>
          issuer.report(Unsupported("list", node.target.loc))
        case TupleExpr(values) =>
          val (t, e) = inferType(node.value, ctx)
          t.toSort match
            case Sort.S =>
              val id = vm.declare(ast.strType)
              out += ast.Assign(id, e)
              out += ast.Assert(ast.Cmp(EQ,
                ast.Length(ast.Var(id).withSort(Sort.S)),
                ast.Const(values.length)).fillLocation(node.loc))
              var newCtx = ctx
              for i <- values.indices do
                newCtx = checkAssign(values(i),
                  ast.CharAt(ast.Var(id).withSort(Sort.S), ast.Const(i)).fillLocation(values(i).loc),
                  // NOTE: to skip checking the binder has type char
                  ast.strType, (newCtx, com))
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
              val id = declare(name.asIdent, t, com)
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

    override def visitAnnAssign(node: AnnAssign, env: (LCtx, LCtx)): LCtx =
      // regard this kind of assignment as new variable declaration
      val (ctx, com) = env
      ctx.get(node.ident.name) match
        case None =>
          val t = checkAnnot(node.annotation, gCtx)
          val id = declare(node.ident, t, com)
          for value <- node.init do
            val e = checkType(value, vm.getType(id), ctx)
            out += ast.Assign(id, e)
          ctx + (node.ident.name -> id)
        case Some(_) =>
          issuer.report(Redefined(node.ident))
          ctx

    private def declare(ident: Ident, typ: ast.Type, com: LCtx): String =
      com.get(ident.name) match
        case Some(id) => // parallel declaration
          val t = vm.getType(id)
          if typ != t then
            issuer.report(TypeMismatch(t.show, typ.show, ident.loc))
          id
        case None => // normal declaration
          vm.declare(typ, ident)

    override def visitAssert(node: Assert, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      val e = checkType(node.test, ast.BoolType, ctx)
      out += ast.Assert(e)
      ctx

    override def visitPass(node: Pass, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      ctx

    override def visitIf(node: If, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      val e = checkType(node.test, ast.BoolType, ctx)
      val (b1, ctx1) = checkBody(node.body, ctx, com)
      val delta1 = ctx1 -- ctx.keySet
      val (b2, ctx2) = checkBody(node.orElse, ctx, com ++ delta1)
      out += ast.IfStmt(e, ast.mkStmtList(b1), ast.mkStmtList(b2))
      ctx ++ delta1.view.filterKeys(ctx2.contains)

    override def visitWhile(node: While, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      val e = checkType(node.test, ast.BoolType, ctx)
      val (invNodes, realBody) = extractInv(node.body, Nil)
      val (b, _) = checkBody(realBody, ctx, com)(using insideLoop = true)
      if b.last.isInstanceOf[ast.Break] then // this while loop is just an if-statement
        out += ast.IfStmt(e, ast.mkStmtList(b.dropRight(1)), ast.Skip())
        if invNodes.nonEmpty then
          issuer.report(TypeError("No loop invariant expected here", invNodes.head.loc))
      else
        val loop = ast.While(e, ast.mkStmtList(b))
        val inv = for expr <- invNodes yield checkType(expr, ast.BoolType, ctx)
        loop.invariants ++= inv
        out += loop
      ctx

    @tailrec
    private def extractInv(loopBody: Seq[LocalStmt], acc: List[Expr]): (List[Expr], Seq[LocalStmt]) =
      loopBody match
        case Nil => (acc, Seq.empty)
        case ExprStmt(Call(Name("inv"), es)) :: ss => extractInv(ss, acc ++ es)
        case ss => (acc, ss)

    override def visitBreak(node: Break, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      if insideLoop then
        out += ast.Break()
      else
        issuer.report(SyntaxError("'break' outside loop", node.loc))
      ctx

    override def visitReturn(node: Return, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      val e = node.value match
        case Some(expr) =>
          val e = checkType(expr, returnType, ctx)
          out += ast.Assign("return", e)
        case None =>
          if returnType != ast.unitType then
            issuer.report(TypeError("missing return value", node.loc))
          ast.mkUnit
      out += ast.Return()
      ctx

    override def visitExprStmt(node: ExprStmt, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      node.expr match
        case Call(Name("show_type"), Seq(arg)) =>
          val e = checkType(arg, ast.strType, ctx)
          out += ast.ShowType(e)
          ctx
        case _ =>
          val (t, e) = inferType(node.expr, ctx)
          val freshId = vm.declare(t)
          out += ast.Assign(freshId, e)
          ctx
