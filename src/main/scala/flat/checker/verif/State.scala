package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.*

final case class MethodSig(params: List[(String, Type)], returnType: Type, pre: List[Expr], post: List[Expr]):
  def paramNames: List[String] = params.map(_._1)

  def paramTypes: List[Type] = params.map(_._2)

  def applyPre(args: List[Expr]): List[Expr] =
    require(args.length == params.length, "Argument length mismatch")
    val argsMap = paramNames.zip(args).toMap
    pre.map(_.subst(argsMap))

  def applyPost(args: List[Expr], rets: List[Expr]): List[Expr] =
    require(args.length == params.length, "Argument length mismatch")
    val argsMap = paramNames.zip(args).toMap
    post.map:
      case LambdaExpr(ps, b) if ps.length >= 2 =>
        rets match
          case List(e) =>
            b.subst(argsMap ++ (for ((y, _), i) <- ps.zipWithIndex yield y -> TupleSelect(i, e)()).toMap)
          case _ =>
            require(rets.length == ps.length, "Return length mismatch")
            b.subst(argsMap ++ (for ((y, _), e) <- ps.zip(rets) yield y -> e).toMap)
      case f =>
        rets match
          case List(e) => Apply(f, List(e)).subst(argsMap)
          case _ => Apply(f, List(TupleExpr(rets))).subst(argsMap)

enum Signal:
  case NEXT, BREAK, CONTINUE

final case class State(uri: String,
                       methods: Map[String, MethodSig],
                       currentMethod: Option[String] = None,
                       typeStack: List[Map[String, Type]] = List(Map.empty),
                       symbolicStack: List[Map[String, Expr]] = List(Map.empty),
                       symbolicVars: Map[String, Type] = Map.empty,
                       conds: List[Expr] = Nil,
                       counters: Map[String, Int] = Map.empty,
                       signal: Signal = Signal.NEXT):
  def getMethodSig: MethodSig =
    require(currentMethod.isDefined, "Current method is not defined")
    methods(currentMethod.get)

  def pushed: State = copy(symbolicStack = Map.empty :: symbolicStack)

  def getType(name: String): Type =
    typeStack.collectFirst { case m if m.contains(name) => m(name) }
      .getOrElse(throw new RuntimeException(s"Variable $name not found in sort stack"))

  def getSymbolicValue(name: String): Expr =
    symbolicStack.collectFirst { case m if m.contains(name) => m(name) }
      .getOrElse(throw new RuntimeException(s"Variable $name not found in value stack"))

  def declare(name: String, typ: Type): State =
    copy(typeStack = typeStack.head + (name -> typ) :: typeStack.tail)

  def declare(varsWithTypes: List[(String, Type)]): State =
    copy(typeStack = typeStack.head ++ varsWithTypes.toMap :: typeStack.tail)

  def assign(name: String, value: Expr): State =
    copy(symbolicStack = symbolicStack.head + (name -> value) :: symbolicStack.tail)

  def havoc(name: String): State =
    val newCount = counters.getOrElse(name, 0)
    val freshName = name + "$" + newCount
    copy(counters = counters + (name -> (newCount + 1)),
      symbolicVars = symbolicVars + (freshName -> getType(name)),
      symbolicStack = symbolicStack.head + (name -> Var(freshName)) :: symbolicStack.tail)

  def havoc(names: Set[String]): State = names.foldLeft(this)(_.havoc(_))