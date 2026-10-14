package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.*

enum Signal:
  case NORMAL, BREAK, CONTINUE

final case class State(uri: String,
                       sortStack: List[Map[String, Type]] = List(Map.empty),
                       valueStack: List[Map[String, Expr]] = List(Map.empty),
                       symbolicVars: Map[String, Type] = Map.empty,
                       conds: List[Expr] = Nil,
                       freshNext: Map[String, Int] = Map.empty,
                       signal: Signal = Signal.NORMAL):
  def pushed: State = copy(valueStack = Map.empty :: valueStack)

  def getSort(name: String): Type =
    sortStack.collectFirst { case m if m.contains(name) => m(name) }
      .getOrElse(throw new RuntimeException(s"Variable $name not found in sort stack"))

  def getValue(name: String): Expr =
    valueStack.collectFirst { case m if m.contains(name) => m(name) }
      .getOrElse(throw new RuntimeException(s"Variable $name not found in value stack"))

  def declare(name: String, typ: Type): State =
    copy(sortStack = sortStack.head + (name -> typ) :: sortStack.tail)

  def assign(name: String, value: Expr): State =
    copy(valueStack = valueStack.head + (name -> value) :: valueStack.tail)

  def havoc(name: String): State =
    val newCount = freshNext.getOrElse(name, 0)
    val freshName = name + "$" + newCount
    copy(freshNext = freshNext + (name -> (newCount + 1)),
      symbolicVars = symbolicVars + (freshName -> getSort(name)),
      valueStack = valueStack.head + (name -> Var(freshName)) :: valueStack.tail)

  def havoc(names: Set[String]): State = names.foldLeft(this)(_.havoc(_))

  def assume(cond: Expr): State = copy(conds = cond :: conds)

  def assume(conds: List[Expr]): State = copy(conds = conds ++ this.conds)