package flat.checker.verif

import flat.checker.flan.*
import flat.checker.flan.tpd.*
import org.apache.commons.math.optimization.GoalType
import org.apache.commons.math.optimization.linear.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Try}

class LPSolver(constraints: List[Expr]):
  private val variables = ListBuffer.empty[Expr]

  private def lookup(expr: Expr): Int = variables.indexOf(expr) match
    case -1 =>
      variables += expr
      variables.length - 1
    case k => k

  private def encodeLinearConstraint(op: String, e1: Expr, e2: Expr): LinearConstraint =
    val m = mutable.Map.empty[Int, Double]
    var const = 0.0
    e1.summands.foreach:
      case Const(n: Int) => const -= n
      case Negate(e) => m += lookup(e) -> -1.0
      case e => m += lookup(e) -> 1.0
    e2.summands.foreach:
      case Const(n: Int) => const += n
      case Negate(e) => m += lookup(e) -> 1.0
      case e => m += lookup(e) -> -1.0
    val rel = op match
      case "==" => Relationship.EQ
      case "!=" => throw IllegalArgumentException(s"LPSolver does not support '!=' constraint: $e1 != $e2")
      case "<=" => Relationship.LEQ
      case "<" => const -= 1; Relationship.LEQ
    LinearConstraint(mkCoefficients(m.toMap), rel, const)

  private def mkCoefficients(m: Map[Int, Double]): Array[Double] =
    val a = Array.fill(variables.length)(0.0)
    for k <- m.keys do a(k) = m(k)
    a

  def solve(goal: Expr): (Option[Int], Option[Int]) =
    val linearConstraints = constraints.collect:
      case Eq(e1, e2) => encodeLinearConstraint("==", e1, e2)
      case Ne(e1, e2) => encodeLinearConstraint("!=", e1, e2)
      case Le(e1, e2) => encodeLinearConstraint("<=", e1, e2)
      case Lt(e1, e2) => encodeLinearConstraint("<", e1, e2)

    val m = mutable.Map.empty[Int, Double]
    var const = 0.0
    goal.summands.foreach:
      case Const(n: Int) => const += n
      case Negate(e) => m += lookup(e) -> -1.0
      case e => m += lookup(e) -> 1.0
    val objective = LinearObjectiveFunction(mkCoefficients(m.toMap), const)

    val minSolver = new SimplexSolver
    val min = Try(minSolver.optimize(objective, linearConstraints.asJava, GoalType.MINIMIZE, false)) match
      case Success(value) => Some(value.getValue.toInt)
      case _ => None
    val maxSolver = new SimplexSolver
    val max = Try(maxSolver.optimize(objective, linearConstraints.asJava, GoalType.MAXIMIZE, false)) match
      case Success(value) => Some(value.getValue.toInt)
      case _ => None
    (min, max)

  extension (expr: Expr)
    def summands: List[Expr] = expr match
      case Add(e1, e2) => e1.summands ++ e2.summands
      case Sub(e1, e2) => e1.summands ++ e2.summands.map(Negate(_))
      case Negate(e) => e.summands.map(Negate(_))
      case e => List(e)