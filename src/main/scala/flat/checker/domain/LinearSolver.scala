package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging
import flat.checker.domain.Prettifier.pp
import flat.checker.domain.RegEx.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class LinearSolver[T1, D1, T2, D2] extends LazyLogging:
  protected type InputRE = RegEx[T1, D1]
  protected type OutputRE = RegEx[T2, D2]

  protected def build(input: InputRE): (OutputRE, List[(OutputRE, InputRE)])

  def solve(input: InputRE): OutputRE =
    // X_i semantically encodes f(inputs(i - 1)), for 1 <= i
    val inputs = ListBuffer(input)
    // X_i = sum_{j} eqs(i)(j) * X_j, for 1 <= i
    // NOTE: X_0 = 1, hence eqs(i)(0) is the constant term of X_i
    val eqs: mutable.Map[Int, mutable.Map[Int, OutputRE]] = mutable.Map.empty

    // Step 1: collect equations
    var i = 1
    while i <= inputs.length do
      eqs(i) = mutable.Map.empty
      val (base, terms) = build(inputs(i - 1))
      eqs(i)(0) = base
      for (r, x) <- terms do
        val j = inputs.indexOf(x) match
          case -1 =>
            inputs += x
            inputs.length
          case k => k + 1
        eqs(i)(j) = eqs(i).getOrElse(j, REZero()) + r
      i += 1

    logger.debug("equations:\n{}\nwhere\n{}",
      (for (i, eq) <- eqs yield showEq(i, eq.toMap)).mkString("\n"),
      (for (x, i) <- inputs.zipWithIndex yield s"X_${i + 1} = f(${x.pp})").mkString("\n"))

    // Step 2: solve equations
    i = inputs.length
    while 1 <= i do
      // Apply Arden's rule (X = A * X + B => X = A.star * B) to eliminate X_i in the rhs of X_i
      if eqs(i).contains(i) then
        // X_i = sum_{j} eqs(i)(j) * X_j
        // => X_i = eqs(i)(i).star * (sum_{j < i} eqs(i)(j) * X_j)
        val star = eqs(i)(i).star
        for (j, r) <- eqs(i); if j < i do
          eqs(i)(j) = star * r
        eqs(i).remove(i)

      // Substitute X_i into other equations X_k for 1 <= k < i
      for k <- 1 until i do
        if eqs(k).contains(i) then
          // X_k = sum_{j != i} eqs(k)(j) * X_j + eqs(k)(i) * X_i
          //     = sum_{j != i} eqs(k)(j) * X_j + eqs(k)(i) * (sum_{j < i} eqs(i)(j) * X_j)
          //     = sum_{j != i} (eqs(k)(j) + eqs(k)(i) * eqs(i)(j)) * X_j
          for (j, r) <- eqs(i) do
            eqs(k)(j) = eqs(k).getOrElse(j, REZero()) + eqs(k)(i) * r
          eqs(k).remove(i)

      i -= 1

    // Finally, X_1 = eqs(1)(0) gives the result
    assert(eqs(1).keySet == Set(0))
    eqs(1)(0)

  private def showEq(eq: (Int, Map[Int, OutputRE])): String =
    val (i, coef) = eq
    val rhs = (for (j, rj) <- coef; if j > 0 yield s" + ${rj.pp} * X_$j").mkString
    s"X_$i = ${coef(0).pp}$rhs"