package flat.checker.typing

import flat.checker.*
import flat.checker.domain.StrRE
import flat.checker.flan.TypeOps.erase
import flat.checker.flan.tpd.*
import flat.checker.flan.untpd
import org.eclipse.lsp4j.Range

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Info:
  val range: Range

final case class TypeInfo(typ: Type)(val range: Range) extends Info

final case class LangInfo(regEx: StrRE)(val range: Range) extends Info

final case class ConstInfo(sort: Type, value: Expr)(val range: Range) extends Info

final case class MethodInfo(params: List[(untpd.Ident, Type)], returnParams: List[(untpd.Ident, Type)])
                           (val range: Range) extends Info:
  def paramInfos: List[(String, ValInfo)] = for (id, t) <- params yield id.name -> ValInfo(t.erase)(id.range)

  def returnInfos: List[(String, VarInfo)] = for (id, t) <- returnParams yield id.name -> VarInfo(t.erase)(id.range)

  def returnType: Type =
    returnParams.map(_._2) match
      case List(t) => t
      case ts => TupleType(ts)

  def funType: FunType = FunType(params.map(_._2), returnType)

final case class ValInfo(typ: Type)(val range: Range) extends Info

final case class VarInfo(typ: Type)(val range: Range) extends Info

final case class Local(scope: Map[String, Info], narrowedTypes: Map[String, Type],
                       isLoop: Boolean, nextFresh: Int = 0)

final case class Ctx(globalScope: Map[String, Info] = Map.empty, localStack: List[Local] = Nil):
  def getDefined(name: String): Option[Info] =
    localStack.headOption.flatMap(_.scope.get(name)).orElse(globalScope.get(name))

  def define(name: String, info: Info): Ctx = localStack match
    case Nil => copy(globalScope = globalScope + (name -> info))
    case current :: rest =>
      val updated = current.copy(scope = current.scope + (name -> info))
      copy(localStack = updated :: rest)

  def defineFreshVal(sort: Type): (String, Ctx) = localStack match
    case Nil => throw IllegalStateException("Cannot define fresh val in global scope")
    case current :: rest =>
      val freshName = s"fresh:${current.nextFresh}"
      val updated = current.copy(
        scope = current.scope + (freshName -> ValInfo(sort)(noRange)),
        nextFresh = current.nextFresh + 1)
      (freshName, copy(localStack = updated :: rest))

  def lookup(name: String): Option[Info] =
    localStack.collectFirst { case ctx if ctx.scope.contains(name) => ctx.scope(name) }
      .orElse(globalScope.get(name))

  def narrow(name: String, typ: Type): Ctx = localStack match
    case Nil => this
    case current :: rest =>
      val updated = current.copy(narrowedTypes = current.narrowedTypes + (name -> typ))
      copy(localStack = updated :: rest)

  def getNarrowedType(name: String): Option[Type] =
    localStack.collectFirst { case ctx if ctx.narrowedTypes.contains(name) => ctx.narrowedTypes(name) }

  def insideLoop: Boolean = localStack.exists(_.isLoop)

  def push(isLoop: Boolean = false): Ctx =
    copy(localStack = Local(Map.empty, Map.empty, isLoop) :: localStack)

@deprecated
final class VarStore:
  private val counts = mutable.Map.empty[String, Int]
  private val buf = ListBuffer.empty[VarDecl]

  def add(name: String, typ: Type): Int =
    counts.get(name) match
      case Some(n) =>
        val newName = s"${name}_$n"
        counts(name) = n + 1
        buf += VarDecl(newName, typ)
      case None =>
        counts(name) = 1
        buf += VarDecl(name, typ)
    buf.length - 1

  def getName(index: Int): String = buf(index).name

  def toList: List[VarDecl] = buf.toList
