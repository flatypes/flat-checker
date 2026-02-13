package flat.checker.ast

import scala.collection.mutable.ListBuffer

final case class Module(body: List[GlobalStmt])

sealed trait GlobalStmt

final case class Decl(name: String, sort: Sort)

final case class FunDef(name: String, params: List[Decl], returnSort: Sort,
                        requires: List[Expr], ensures: List[Expr], locals: List[Decl], body: Stmt) extends GlobalStmt:
  lazy val lCtx: Map[String, Sort] =
    val paramCtx = Map.from(for param <- params yield param.name -> param.sort)
    val returnCtx = Map("return" -> returnSort)
    val localCtx = Map.from(for param <- locals yield param.name -> param.sort)
    paramCtx ++ returnCtx ++ localCtx

/** Local Statement. */
sealed trait Stmt:
  protected def children: List[Stmt]

  def traverse(pf: PartialFunction[Stmt, Unit]): Unit =
    pf.applyOrElse(this, _ => {})
    children.foreach(_.traverse(pf))

  def collect[T](pf: PartialFunction[Stmt, T]): List[T] =
    pf.lift.apply(this) match
      case Some(x) => List(x)
      case None => children.flatMap(_.collect(pf))

  def collectFirst[T](pf: PartialFunction[Stmt, T]): Option[T] =
    pf.lift.apply(this).orElse:
      children.collectFirst(Function.unlift(_.collectFirst(pf)))

  def toBlock: List[Stmt] = List(this)

final case class Skip() extends Stmt:
  protected def children: List[Stmt] = Nil

  override def toBlock: List[Stmt] = Nil

final case class SeqStmt(first: Stmt, second: Stmt) extends Stmt:
  protected def children: List[Stmt] = List(first, second)

  override def toBlock: List[Stmt] = first.toBlock ++ second.toBlock

def mkStmtList(body: List[Stmt]): Stmt = body match
  case Nil => Skip()
  case ss => ss.reduceRight(SeqStmt(_, _))

final case class Assign(id: String, value: Expr) extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Assert(cond: Expr) extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Assume(cond: Expr) extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Hint(cond: Expr) extends Stmt:
  protected def children: List[Stmt] = Nil

final case class ShowType(value: Expr) extends Stmt:
  protected def children: List[Stmt] = Nil

final case class IfStmt(cond: Expr, thenBody: Stmt, elseBody: Stmt) extends Stmt:
  protected def children: List[Stmt] = List(thenBody, elseBody)

final case class While(cond: Expr, body: Stmt) extends Stmt:
  var invariants: ListBuffer[Expr] = ListBuffer.empty

  protected def children: List[Stmt] = List(body)

final case class Break() extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Return() extends Stmt:
  protected def children: List[Stmt] = Nil