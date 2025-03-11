package flat.checker.py

import flat.checker.ast.Ident
import flat.checker.py.ast.*
import flat.checker.{Issuer, ast}

import scala.collection.mutable.ListBuffer

final case class TypeInfo(expansion: ast.Type, ident: Ident)

final case class FunInfo(funType: ast.FunType, ident: Ident)

final case class VarInfo(typ: ast.Type, ident: Ident)

final class VarManager:
  private val data = ListBuffer.empty[VarInfo]

  def declare(typ: ast.Type, ident: Ident): Int =
    data += VarInfo(typ, ident)
    data.length - 1

  def declare(typ: ast.Type): Int =
    data += VarInfo(typ, Ident(""))
    data.length - 1

  def getType(id: Int): ast.Type = data(id).typ

  def getTypes: Seq[ast.Type] = data.toSeq.map(_.typ)

type GCtx = Map[String, FunInfo | TypeInfo]

final class Transpiler:
  given issuer: Issuer = new Issuer

  private val annotChecker = new AnnotChecker
  import annotChecker.checkAnnot

  def transpile(tree: Seq[TopStmt]): Seq[ast.FunDef] =
    val initCtx: GCtx = Map.empty
    val finalCtx = tree.foldLeft(initCtx) { (c, s) => s.accept(FirstPass, c) }
    val out = ListBuffer.empty[ast.FunDef]
    val secondPass = SecondPass(out)
    for s <- tree do s.accept(secondPass, finalCtx)
    issuer.ensureNoError()
    out.toSeq

  private object FirstPass extends NodeVisitor[GCtx, GCtx]:
    override def visitTypeAlias(node: TypeAlias, ctx: GCtx): GCtx =
      val x = node.ident.name
      ctx.get(x) match
        case None =>
          val t = checkAnnot(node.value, ctx)
          ctx + (x -> TypeInfo(t, node.ident))
        case Some(conflict) =>
          issuer.report(Redefined(node.ident))
          ctx

    override def visitFunctionDef(node: FunctionDef, ctx: GCtx): GCtx =
      val f = node.ident.name
      ctx.get(f) match
        case None =>
          val argNames = node.args.map(_.ident.name)
          for
            i <- argNames.indices
            x = argNames(i)
            if argNames.take(i - 1).contains(x)
          do issuer.report(Redefined(node.args(i).ident))
          val argTypes = for arg <- node.args yield checkAnnot(arg.annotation, ctx)
          val returnType = checkAnnot(node.returns, ctx)
          ctx + (f -> FunInfo(ast.FunType(argTypes, returnType), node.ident))
        case Some(conflict) =>
          issuer.report(Redefined(node.ident))
          ctx

  private class SecondPass(out: ListBuffer[ast.FunDef]) extends NodeVisitor[GCtx, Unit]:
    override def visitTypeAlias(node: TypeAlias, ctx: GCtx): Unit = () // do nothing

    override def visitFunctionDef(node: FunctionDef, ctx: GCtx): Unit =
      val info = ctx(node.ident.name).asInstanceOf[FunInfo]

      given gCtx: GCtx = ctx

      given returnType: ast.Type = info.funType.returns

      given vm: VarManager = new VarManager

      val checker = new BodyChecker
      val lCtx = Map.from(for (Arg(a, _), t) <- node.args zip info.funType.args yield a.name -> vm.declare(t, a))
      val (s, _) = checker.checkBody(node.body, lCtx, Map.empty)
      val (paramTypes, varTypes) = vm.getTypes.splitAt(info.funType.args.length)
      out += ast.FunDef(node.ident, paramTypes, returnType, varTypes, s)
