package flat.checker.py

import flat.checker.Issuer
import flat.checker.backend.core
import flat.checker.backend.core.Ident
import flat.checker.py.ast.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class TypeInfo(expansion: core.Type, ident: Ident)

final case class FunInfo(funType: core.FunType, ident: Ident)

final case class VarInfo(typ: core.Type, ident: Ident)

final class VarManager:
  private val data = mutable.Map.empty[String, VarInfo]
  private var nextTmp = 0

  def declare(typ: core.Type, ident: Ident): String =
    assert(!data.contains(ident.name))
    data(ident.name) = VarInfo(typ, ident)
    ident.name

  def declare(typ: core.Type): String =
    nextTmp += 1
    val x = s"tmp-$nextTmp"
    data(x) = VarInfo(typ, Ident(x))
    x

  def getType(id: String): core.Type = data(id).typ

  def getTypes: Map[String, core.Type] = Map.from(for (x, VarInfo(t, _)) <- data yield x -> t)

type GCtx = Map[String, FunInfo | TypeInfo]

final class Transpiler:
  given issuer: Issuer = new Issuer

  private val annotChecker = new AnnotChecker
  import annotChecker.checkAnnot

  def transpile(tree: List[TopStmt]): List[core.Program] =
    val initCtx: GCtx = Map.empty
    val finalCtx = tree.foldLeft(initCtx) { (c, s) => s.accept(FirstPass, c) }
    val out = ListBuffer.empty[core.Program]
    val secondPass = SecondPass(out)
    for s <- tree do s.accept(secondPass, finalCtx)
    issuer.ensureNoError()
    // TODO: check all expressions have location
    out.toList

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
            case Some(Constant(null)) | None => core.unitType
            case Some(annot) => checkAnnot(annot, ctx)
          ctx + (f -> FunInfo(core.FunType(argTypes, returnType), node.ident))
        case Some(conflict) =>
          issuer.report(Redefined(node.ident))
          ctx

  private class SecondPass(out: ListBuffer[core.Program]) extends NodeVisitor[GCtx, Unit]:
    override def visitTypeAlias(node: TypeAlias, ctx: GCtx): Unit = () // do nothing

    override def visitFunctionDef(node: FunctionDef, ctx: GCtx): Unit =
      val info = ctx(node.ident.name).asInstanceOf[FunInfo]

      given gCtx: GCtx = ctx

      given returnType: core.Type = info.funType.returns

      given vm: VarManager = new VarManager

      val checker = new BodyChecker
      val lCtx = Map.from(for (Arg(a, _), t) <- node.args zip info.funType.args yield a.name -> vm.declare(t, a))
      val (s, _) = checker.checkBody(node.body, lCtx, Map.empty)(using insideLoop = false)
      out += core.Program(vm.getTypes + ("return" -> returnType), s)
