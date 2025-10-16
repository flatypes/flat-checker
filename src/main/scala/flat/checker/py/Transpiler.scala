package flat.checker.py

import flat.Issuer
import flat.checker.ast
import flat.checker.ast.VarDef
import flat.checker.py.ast.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class TypeInfo(expansion: ast.Type, ident: Ident)

final case class FunInfo(funType: ast.FunType, ident: Ident)

final case class VarInfo(typ: ast.Type, ident: Ident)

final class VarManager:
  private val data = mutable.Map.empty[String, VarInfo]
  private var nextTmp = 0

  def declare(typ: ast.Type, ident: Ident): String =
    assert(!data.contains(ident.name))
    data(ident.name) = VarInfo(typ, ident)
    ident.name

  def declare(typ: ast.Type): String =
    nextTmp += 1
    val x = s"tmp-$nextTmp"
    data(x) = VarInfo(typ, Ident(x))
    x

  def getType(id: String): ast.Type = data(id).typ

  def getTypes: Map[String, ast.Type] = Map.from(for (x, VarInfo(t, _)) <- data yield x -> t)

type GCtx = Map[String, FunInfo | TypeInfo]

final class Transpiler:
  given issuer: Issuer = new Issuer

  private val annotChecker = new AnnotChecker

  import annotChecker.checkAnnot

  def transpile(tree: List[TopStmt]): ast.Module =
    val initCtx: GCtx = Map.empty
    val finalCtx = tree.foldLeft(initCtx) { (c, s) => s.accept(FirstPass, c) }
    val out = ListBuffer.empty[ast.FunDef]
    val secondPass = SecondPass(out)
    for s <- tree do s.accept(secondPass, finalCtx)
    issuer.ensureNoError()
    // TODO: check all expressions have location
    ast.Module(out.toList)

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
          val returnType = node.returns match
            case Some(Constant(null)) | None => ast.unitType
            case Some(annot) => checkAnnot(annot, ctx)
          ctx + (f -> FunInfo(ast.FunType(argTypes, returnType), node.ident))
        case Some(conflict) =>
          issuer.report(Redefined(node.ident))
          ctx

  private class SecondPass(out: ListBuffer[ast.FunDef]) extends NodeVisitor[GCtx, Unit]:
    override def visitTypeAlias(node: TypeAlias, ctx: GCtx): Unit = () // do nothing

    override def visitFunctionDef(node: FunctionDef, ctx: GCtx): Unit =
      val name = node.ident.name
      val vm = new VarManager
      val info = ctx(name).asInstanceOf[FunInfo]
      val params = List.from(for (Arg(a, _), t) <- node.args zip info.funType.args yield ast.VarDef(a.name, t))
      val returnType = info.funType.returns
      val returns = VarDef("return", returnType)

      val checker = BodyChecker(using gCtx = ctx, returnType = returnType, vm = vm)()
      val lCtx = Map.from(for (Arg(a, _), t) <- node.args zip info.funType.args yield a.name -> vm.declare(t, a))
      val (ss, _) = checker.checkBody(node.body, lCtx, Map.empty)(using insideLoop = false)
      val locals = List.from(for x -> t <- vm.getTypes.removedAll(node.args.map(_.ident.name)) yield VarDef(x, t))
      out += ast.FunDef(name, params, returns, locals, ast.mkStmtList(ss))
