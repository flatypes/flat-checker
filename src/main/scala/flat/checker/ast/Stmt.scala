package flat.checker.ast

import scala.collection.mutable.ListBuffer

final case class Module(body: List[GlobalStmt])

sealed trait GlobalStmt

final case class Decl(name: String, sort: Sort)

final case class FunDef(name: String, params: List[Decl], returnSort: Sort, requires: List[Expr], ensures: List[Expr],
                        locals: List[Decl], body: List[Stmt]) extends GlobalStmt:
  lazy val lCtx: Map[String, Sort] =
    val paramCtx = Map.from(for param <- params yield param.name -> param.sort)
    val returnCtx = Map("return" -> returnSort)
    val localCtx = Map.from(for param <- locals yield param.name -> param.sort)
    paramCtx ++ returnCtx ++ localCtx

  def programVars: Set[String] = (params ++ locals).map(_.name).toSet + "return"

  def sortingContext: SortingContext = SortingContext(lCtx)

/** Local Statement. */
sealed trait Stmt:
  protected def children: List[Stmt]

  def traverse(pf: PartialFunction[Stmt, Unit]): Unit =
    pf.applyOrElse(this, _ => {})
    children.foreach(_.traverse(pf))

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

final case class IfStmt(cond: Expr, thenBody: List[Stmt], elseBody: List[Stmt]) extends Stmt:
  protected def children: List[Stmt] = thenBody ++ elseBody

final case class While(cond: Expr, body: List[Stmt]) extends Stmt:
  val invariants: ListBuffer[Expr] = ListBuffer.empty

  protected def children: List[Stmt] = body

final case class Break() extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Continue() extends Stmt:
  protected def children: List[Stmt] = Nil

final case class Return() extends Stmt:
  protected def children: List[Stmt] = Nil