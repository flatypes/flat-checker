package flat.checker

import flat.checker.backend.core
import flat.checker.backend.core.Type

final case class FunInfo(paramTypes: Seq[Type], returnType: Type, loc: Location)

final class GlobalContext(private val data: Map[String, FunInfo]):
  def lookup(name: String): Option[FunInfo] = data.get(name)

  def apply(name: String): FunInfo = data(name)

  def declare(name: String, info: FunInfo): GlobalContext =
    if data.contains(name) then
      throw IllegalArgumentException(s"function '$name' is already defined")
    GlobalContext(data + (name -> info))

object GlobalContext:
  val empty = GlobalContext(Map.empty)

final case class VarInfo(declaredType: Type, latestType: Type):
  def havoc: VarInfo = copy(latestType = declaredType)

final case class LocalContext private(currentFun: String, private val data: Seq[VarInfo]):
  def apply(varId: Int): VarInfo = data(varId)

  def update(varId: Int, typ: Type): LocalContext =
    val info = data(varId)
    copy(data = data.updated(varId, info.copy(latestType = typ)))

  def havoc: LocalContext = copy(data = data.map(_.havoc))

  def |(other: LocalContext): LocalContext =
    require(currentFun == other.currentFun, s"inconsistent context: function different")
    require(data.length == other.data.length,
      s"inconsistent stack: length different (${data.length} != ${other.data.length})")
    val joined = for (info1, info2) <- data zip other.data yield
      require(info1.declaredType == info2.declaredType, s"inconsistent stack: type different")
      info1.copy(latestType = info1.latestType | info2.latestType)
    copy(data = joined)

object LocalContext:
  def from(currentFun: String, localTypes: Seq[core.Type]): LocalContext =
    LocalContext(currentFun, for t <- localTypes yield VarInfo(t, t))
