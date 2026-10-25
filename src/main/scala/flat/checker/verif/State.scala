package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.*

final case class MethodSig(params: List[(String, Type)], returnType: Type,
                           pre: List[Expr], post: List[Expr],
                           locals: List[(String, Type)]):
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
                       types: Map[String, Type] = Map.empty,
                       symbolicValues: Map[String, Expr] = Map.empty,
                       symbolicVars: Map[String, Type] = Map.empty,
                       conds: List[Expr] = Nil,
                       counters: Map[String, Int] = Map.empty,
                       signal: Signal = Signal.NEXT):
  def getMethodSig: MethodSig =
    require(currentMethod.isDefined, "Current method is not defined")
    methods(currentMethod.get)

  def getType(name: String): Type = types(name)

  def getSymbolicValue(name: String): Expr = symbolicValues(name)

  def declare(name: String, typ: Type): State =
    copy(types = types + (name -> typ))

  def declare(varsWithTypes: List[(String, Type)]): State =
    copy(types = types ++ varsWithTypes.toMap)

  def assign(name: String, value: Expr): State =
    copy(symbolicValues = symbolicValues + (name -> value))

  def havoc(name: String): State =
    val newCount = counters.getOrElse(name, 0)
    val freshName = name + "$" + newCount
    copy(counters = counters + (name -> (newCount + 1)),
      symbolicVars = symbolicVars + (freshName -> getType(name)),
      symbolicValues = symbolicValues + (name -> Var(freshName)))

  def havoc(names: Set[String]): State = names.foldLeft(this)(_.havoc(_))