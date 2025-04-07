package flat.checker.backend

import org.apache.commons.math.optimization.GoalType.{MAXIMIZE, MINIMIZE}
import org.apache.commons.math.optimization.linear.*
import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Try}

class ApacheLPTest extends AnyFunSuite:
  test("goal x - y for 0 <= x < y"):
    // x - y
    val objective = LinearObjectiveFunction(Array(1.0, -1.0), 0)
    // 0 <= x < y
    val constraints = Seq(
      LinearConstraint(Array(1.0, 0), Relationship.GEQ, 0), // x + 0y >= 0
      LinearConstraint(Array(1.0, -1.0), Relationship.LEQ, -1) // x - y <= -1
    )
    val solver1 = new SimplexSolver
    val maxValue = solver1.optimize(objective, constraints.asJava, MAXIMIZE, false).getValue
    assert(maxValue == -1)
    val solver2 = new SimplexSolver
    val result = Try(solver2.optimize(objective, constraints.asJava, MINIMIZE, false))
    assert(result.isFailure)
    result match
      case Failure(ex) => assert(ex.getMessage == "unbounded solution")
      case _ => assert(false)

  test("goal x - y for x <= y - 1, !(x < y - 1)"):
    // x - y
    val objective = LinearObjectiveFunction(Array(1.0, -1.0), 0)
    // x - y <= -1, x - y >= -1
    val constraints = Seq(
      LinearConstraint(Array(1.0, -1.0), Relationship.LEQ, -1),
      LinearConstraint(Array(1.0, -1.0), Relationship.GEQ, -1)
    )
    val solver1 = new SimplexSolver
    val maxValue = solver1.optimize(objective, constraints.asJava, MAXIMIZE, false).getValue
    assert(maxValue == -1)
    val solver2 = new SimplexSolver
    val minValue = solver2.optimize(objective, constraints.asJava, MINIMIZE, false).getValue
    assert(minValue == -1)