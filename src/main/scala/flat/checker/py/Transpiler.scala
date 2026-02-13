package flat.checker.py

import flat.Issuer
import flat.checker.ast as ir
import flat.checker.py.Type.{FunType, UnitType}
import flat.checker.py.ast.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class TypeInfo(expansion: Type, ident: Ident)

final case class FunInfo(funType: Type.FunType, ident: Ident)

final case class VarInfo(typ: Type, ident: Ident)

final class VarManager:
  private val data = mutable.Map.empty[String, VarInfo]
  private val binders = mutable.Stack.empty[(String, VarInfo)]
  private var nextTmp = 0

  def declare(typ: Type, ident: Ident): String =
    assert(!data.contains(ident.name), "Variable already declared: " + ident.name)
    data(ident.name) = VarInfo(typ, ident)
    ident.name

  def declare(typ: Type): String =
    nextTmp += 1
    val x = s"tmp-$nextTmp"
    data(x) = VarInfo(typ, Ident(x))
    x

  def withBinder[T](vars: List[VarInfo])(f: => T): T =
    for v <- vars do
      binders.push(v.ident.name -> v)
    val result = f
    for _ <- vars do
      binders.pop()
    result

  def getType(id: String): Type =
    binders.find(_._1 == id) match
      case Some((_, VarInfo(t, _))) => t
      case None => data(id).typ

  def getTypes: Map[String, Type] = Map.from(for (x, VarInfo(t, _)) <- data yield x -> t)

type GCtx = Map[String, FunInfo | TypeInfo]

final class Transpiler:
  given issuer: Issuer = new Issuer

  private val annotChecker = new AnnotChecker

  import annotChecker.checkAnnot

  def transpile(tree: List[TopStmt]): ir.Module =
    val initCtx: GCtx = Map.empty
    val finalCtx = tree.foldLeft(initCtx) { (c, s) => s.accept(FirstPass, c) }
    val out = ListBuffer.empty[ir.FunDef]
    val secondPass = SecondPass(out)
    for s <- tree do s.accept(secondPass, finalCtx)
    issuer.ensureNoError()
    // TODO: check all expressions have location
    ir.Module(out.toList)

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
            case Some(Constant(null)) | None => UnitType
            case Some(annot) => checkAnnot(annot, ctx)
          ctx + (f -> FunInfo(FunType(argTypes.toList, returnType), node.ident))
        case Some(conflict) =>
          issuer.report(Redefined(node.ident))
          ctx

  private class SecondPass(out: ListBuffer[ir.FunDef]) extends NodeVisitor[GCtx, Unit]:
    override def visitTypeAlias(node: TypeAlias, ctx: GCtx): Unit = () // do nothing

    override def visitFunctionDef(node: FunctionDef, ctx: GCtx): Unit =
      val name = node.ident.name
      val vm = new VarManager
      val info = ctx(name).asInstanceOf[FunInfo]

      val params = ListBuffer.empty[ir.Decl]
      val requires = ListBuffer.empty[ir.Expr]
      for (arg, t) <- node.args zip info.funType.args do
        val (base, refinement) = t.split
        val decl = ir.Decl(arg.ident.name, base.toSort)
        params += decl
        for d <- refinement do
          requires += ir.RefinedBy(ir.Var(decl.name)(decl.sort), d)

      val ensures = ListBuffer.empty[ir.Expr]
      val (returnBase, returnRefinement) = info.funType.ret.split
      for d <- returnRefinement do
        ensures += ir.RefinedBy(ir.Var("return")(returnBase.toSort), d).fillLocation(node.returns.get.loc)

      val checker = BodyChecker(using gCtx = ctx, returnType = info.funType.ret, vm = vm)()
      val lCtx = Map.from(for (Arg(a, _), t) <- node.args zip info.funType.args yield a.name -> vm.declare(t, a))
      val (ss, _) = checker.checkBody(node.body, lCtx)(using insideLoop = false)

      val locals = ListBuffer.empty[ir.Decl]
      for x -> t <- vm.getTypes.removedAll(node.args.map(_.ident.name)) do
        val decl = ir.Decl(x, t.toSort)
        locals += decl
        assert(t.split._2.isEmpty, "Local variables cannot have refinement types: " + x)
      out += ir.FunDef(name, params.toList, returnBase.toSort, requires.toList, ensures.toList,
        locals.toList, ir.mkStmtList(ss))
