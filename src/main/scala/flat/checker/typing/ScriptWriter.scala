package flat.checker.typing

import flat.checker.flan.tpd.{Script, Stmt, Type, VarDecl}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ScriptWriter:
  private val varStack = mutable.Stack(mutable.Map.empty[String, Int])
  private val varSorts = ListBuffer.empty[Type]
  private val blockStack = mutable.Stack(ListBuffer.empty[Stmt])

  def enter(): Unit =
    varStack.push(mutable.Map.empty[String, Int])
    blockStack.push(ListBuffer.empty[Stmt])

  def exit(): List[Stmt] =
    varStack.pop()
    blockStack.pop().toList

  def declare(name: String, sort: Type): Unit =
    val m = varStack.top
    require(!m.contains(name), s"Variable $name already declared in this scope")
    val index = varSorts.length
    varSorts += sort
    m(name) = index

  def fresh(sort: Type): String =
    val index = varSorts.length
    varSorts += sort
    val m = varStack.top
    val name = s"fresh:$index"
    m(name) = index
    name

  def getId(name: String): String =
    "v" + varStack.collectFirst { case m if m.contains(name) => m(name) }.get

  def isDefined(name: String): Boolean = varStack.exists(_.contains(name))

  def +=(stmt: Stmt): Unit =
    blockStack.top += stmt

  def ++=(stmts: List[Stmt]): Unit =
    blockStack.top ++= stmts

  def toScript: Script =
    Script(for (t, i) <- varSorts.zipWithIndex.toList yield VarDecl("v" + i, t), blockStack.head.toList)
