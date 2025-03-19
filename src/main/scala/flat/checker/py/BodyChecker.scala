package flat.checker.py

import flat.checker.ast.CmpOp.EQ
import flat.checker.ast.{Ident, StrAt, StrLen}
import flat.checker.py.ast.*
import flat.checker.{Issuer, Sort, ast}

import scala.collection.mutable.ListBuffer

class BodyChecker(using issuer: Issuer, gCtx: GCtx, returnType: ast.Type, vm: VarManager):
  private val annotChecker = new AnnotChecker
  import annotChecker.*

  private val exprChecker = new ExprChecker
  import exprChecker.*

  def checkBody(body: Seq[LocalStmt], ctx: LCtx, com: LCtx): (ast.Stmt, LCtx) =
    val out = ListBuffer.empty[ast.Stmt]
    val checker = Checker(out)
    val newCtx = body.foldLeft(ctx) { (c, s) => s.accept(checker, (c, com)) }
    (ast.StmtBlock(out.toSeq), newCtx)

  private class Checker(out: ListBuffer[ast.Stmt]) extends NodeVisitor[(LCtx, LCtx), LCtx]:
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
            case Sort.String =>
              val id = vm.declare(ast.strType)
              out += ast.Assign(id, e)
              out += ast.Assert(EQ(StrLen(ast.Var(id)), ast.Const(values.length)))
              var newCtx = ctx
              for i <- values.indices do
                newCtx = checkAssign(values(i), StrAt(ast.Var(id), ast.Const(i)), ast.charType, (newCtx, com))
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

    private def declare(ident: Ident, typ: ast.Type, com: LCtx): Int =
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
      val e = checkType(node.test, ast.boolType, ctx)
      out += ast.Assert(e)
      ctx

    override def visitPass(node: Pass, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      ctx

    override def visitIf(node: If, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      val e = checkType(node.test, ast.boolType, ctx)
      val (s1, ctx1) = checkBody(node.body, ctx, com)
      val delta1 = ctx1 -- ctx.keySet
      val (s2, ctx2) = checkBody(node.orElse, ctx, com ++ delta1)
      out += ast.IfStmt(e, s1, s2)
      ctx ++ delta1.view.filterKeys(ctx2.contains)

    override def visitWhile(node: While, env: (LCtx, LCtx)): LCtx =
      val (ctx, com) = env
      val e = checkType(node.test, ast.boolType, ctx)
      val (s, _) = checkBody(node.body, ctx, com)
      out += ast.While(e, s)
      ctx

    override def visitReturn(node: Return, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      val e = node.value match
        case Some(expr) => checkType(expr, returnType, ctx)
        case None =>
          if returnType != ast.unitType then
            issuer.report(TypeError("missing return value", node.loc))
          ast.mkUnit
      out += ast.Return(e)
      ctx

    override def visitExprStmt(node: ExprStmt, env: (LCtx, LCtx)): LCtx =
      val (ctx, _) = env
      val (t, e) = inferType(node.expr, ctx)
      val freshId = vm.declare(t)
      out += ast.Assign(freshId, e)
      ctx
