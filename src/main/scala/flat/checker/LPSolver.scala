package flat.checker

import flat.Ops
import flat.Ops.CmpOp.*
import flat.checker.ExprOps.*
import flat.checker.core.*
import org.apache.commons.math.optimization.GoalType
import org.apache.commons.math.optimization.linear.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Try}

class LPSolver(constraints: List[Cmp]):
  private val variables = ListBuffer.empty[Expr]

  private def lookup(expr: Expr): Int = variables.indexOf(expr) match
    case -1 =>
      variables += expr
      variables.length - 1
    case k => k

  private val linearConstraints = ListBuffer.empty[LinearConstraint]

  // prepare linear constraints
  for Cmp(op, e1, e2) <- constraints do
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
      case EQ => Relationship.EQ
      case NE => throw IllegalArgumentException(s"LPSolver does not support '!=' constraint: $e1 != $e2")
      case LE => Relationship.LEQ
      case LT => const -= 1; Relationship.LEQ
      case GE => Relationship.GEQ
      case GT => const += 1; Relationship.GEQ
    linearConstraints += LinearConstraint(mkCoefficients(m.toMap), rel, const)

  private def mkCoefficients(m: Map[Int, Double]): Array[Double] =
    val a = Array.fill(variables.length)(0.0)
    for k <- m.keys do a(k) = m(k)
    a

  def solve(goal: Expr): (Option[Int], Option[Int]) =
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
