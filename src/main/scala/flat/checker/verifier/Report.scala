package flat.checker.verifier

import flat.checker.ast.Expr
import flat.{Diagnostic, DiagnosticSeverity, Location}

sealed trait Task:
  def loc: Location

sealed trait ProofTask extends Task:
  val cond: Expr

final case class SideTask(expr: Expr, cond: Expr, message: String) extends ProofTask:
  override def loc: Location = expr.loc

final case class PreTask(expr: Expr, cond: Expr) extends ProofTask:
  override def loc: Location = expr.loc

final case class PostTask(cond: Expr) extends ProofTask:
  override def loc: Location = cond.loc

final case class InvTask(cond: Expr, onEntry: Boolean) extends ProofTask:
  override def loc: Location = cond.loc

final case class AssertTask(cond: Expr) extends ProofTask:
  override def loc: Location = cond.loc

final case class InferTask(expr: Expr) extends Task:
  override def loc: Location = expr.loc

sealed trait Result

case object Verified extends Result

case object Unverified extends Result

case class Inferred(result: String) extends Result

final case class ReportItem(task: Task, state: State, result: Result)

final class Report(val items: List[ReportItem]):
  def noError: Boolean = items.forall(_.result != Unverified)

  def getDiagnostics: List[Diagnostic] = items.collect:
    case ReportItem(task@SideTask(_, _, message), _, Unverified) =>
      Diagnostic(task.loc, message, severity = DiagnosticSeverity.ERROR)
    case ReportItem(task: PreTask, _, Unverified) =>
      Diagnostic(task.loc, s"a precondition might not hold", severity = DiagnosticSeverity.ERROR)
    case ReportItem(task: PostTask, _, Unverified) =>
      Diagnostic(task.loc, s"postcondition might not hold on a return path", severity = DiagnosticSeverity.ERROR)
    case ReportItem(task: InvTask, _, Unverified) =>
      val when = if task.onEntry then "on entry" else "again after an iteration"
      Diagnostic(task.loc, s"invariant might not hold $when", severity = DiagnosticSeverity.ERROR)
    case ReportItem(task: AssertTask, _, Unverified) =>
      Diagnostic(task.loc, "assertion might not hold", severity = DiagnosticSeverity.ERROR)
    case ReportItem(task: InferTask, _, Inferred(result)) =>
      Diagnostic(task.loc, s"inferred: $result", severity = DiagnosticSeverity.INFO)

  def getFailureState(loc: Location): Option[State] = items.collectFirst:
    case ReportItem(task, state, Unverified) if task.loc == loc => state
