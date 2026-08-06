package flat.checker.typing

import flat.checker.*
import flat.checker.domain.StrRE
import flat.checker.flan.tpd.*
import flat.checker.flan.{FunSort, Sort, TupleSort}
import org.eclipse.lsp4j.Range

import scala.collection.mutable.ListBuffer

sealed trait Info:
  val range: Range

final case class TypeInfo(typ: NormType)(val range: Range) extends Info

final case class LangInfo(regEx: StrRE)(val range: Range) extends Info

final case class ConstInfo(sort: Sort, value: Expr)(val range: Range) extends Info

final case class ParamInfo(name: String, typ: Sort)(val range: Range)

final case class MethodInfo(params: List[VarDecl], returnParams: List[VarDecl])(val range: Range) extends Info:
  def returnSort: Sort =
    returnParams.map(_.typ.sort) match
      case List(s) => s
      case ss => TupleSort(ss)

  def funSort: FunSort = FunSort(params.map(_.typ.sort), returnSort)

sealed trait Ctx:
  def lookup(name: String): Option[Info]

final case class GlobalCtx(types: Map[String, TypeInfo] = Map.empty,
                           langs: Map[String, LangInfo] = Map.empty,
                           consts: Map[String, ConstInfo] = Map.empty,
                           methods: Map[String, MethodInfo] = Map.empty) extends Ctx:
  def lookup(name: String): Option[Info] =
    types.get(name)
      .orElse(langs.get(name))
      .orElse(consts.get(name))
      .orElse(methods.get(name))

  def define(name: String, info: Info): GlobalCtx = info match
    case t: TypeInfo => copy(types = types + (name -> t))
    case l: LangInfo => copy(langs = langs + (name -> l))
    case c: ConstInfo => copy(consts = consts + (name -> c))
    case m: MethodInfo => copy(methods = methods + (name -> m))
    case _ => throw IllegalArgumentException(s"Cannot define ${info.getClass.getSimpleName}")

final case class VarInfo(normType: NormType, index: Int, isVal: Boolean = false)(val range: Range) extends Info

final case class LocalCtx(global: GlobalCtx,
                          currentMethod: String,
                          scopeStack: List[Map[String, VarInfo]] = List(Map.empty),
                          loopLevel: Int = 0) extends Ctx:
  def lookup(name: String): Option[Info] =
    scopeStack.collectFirst { case scope if scope.contains(name) => scope(name) }
      .orElse(global.lookup(name))

  def get(name: String): Option[VarInfo] =
    require(scopeStack.nonEmpty)
    scopeStack.head.get(name)

  def inLoop: Boolean = loopLevel > 0

  def info: MethodInfo = global.methods(currentMethod)

  def define(name: String, info: VarInfo): LocalCtx =
    require(scopeStack.nonEmpty)
    val scope = scopeStack.head + (name -> info)
    copy(scopeStack = scope :: scopeStack.tail)

  def updateType(name: String, typ: NormType): LocalCtx =
    val i = scopeStack.indexWhere(_.contains(name))
    require(i >= 0, s"Variable $name not found in any scope")
    val scope = scopeStack(i)
    val info = scope(name)
    copy(scopeStack = scopeStack.updated(i, scope + (name -> info.copy(normType = typ)(info.range))))

  def push: LocalCtx = copy(scopeStack = Map.empty :: scopeStack)

  def enterLoop: LocalCtx = copy(scopeStack = Map.empty :: scopeStack, loopLevel = loopLevel + 1)

final class VarStore:
  private val buf = ListBuffer.empty[VarDecl]

  def add(name: String, typ: NormType): Int =
    buf += VarDecl(name, typ)
    buf.length - 1

  def getName(index: Int): String = buf(index).name

  def toList: List[VarDecl] = buf.toList
