package flat.checker

import flat.checker.flan.Show.show
import flat.checker.flan.tpd.{FunType, Type}
import org.eclipse.lsp4j.DiagnosticSeverity.{Error, Warning}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticRelatedInformation, Location, Range}

import java.io.PrintStream
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

given Ordering[Range] with
  def compare(r1: Range, r2: Range): Int =
    if r1.getStart.getLine < r2.getStart.getLine then -1
    else if r1.getStart.getLine > r2.getStart.getLine then 1
    else if r1.getStart.getCharacter < r2.getStart.getCharacter then -1
    else if r1.getStart.getCharacter > r2.getStart.getCharacter then 1
    else 0

trait Reportable:
  def diagnostic: Diagnostic

class Reporter(val source: Source):
  private val buffer = ListBuffer.empty[Diagnostic]

  def hasError: Boolean = buffer.exists(_.getSeverity == Error)

  def printTo(out: PrintStream): Unit =
    for diagnostic <- buffer.sortBy(_.getRange) do
      val range = diagnostic.getRange
      val lineNoWidth = (range.getEnd.getLine + 1).toString.length
      val header = "-".repeat(lineNoWidth)
      val message = diagnostic.getMessage.getLeft
      val title = message.take(message.indexOf(':'))
      val position = s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1}"
      out.println(s"$header $title: ${source.uri}:$position")

      val blankLineNo = " ".repeat(lineNoWidth)
      var caretStart = 0
      for line <- range.getStart.getLine to range.getEnd.getLine do
        val lineNo = (line + 1).toString.padTo(lineNoWidth, ' ')
        val sourceLine = source.getLine(line)
        out.println(s"$lineNo |$sourceLine")
        caretStart = if line == range.getStart.getLine then range.getStart.getCharacter else 0
        val caretEnd = if line == range.getEnd.getLine then range.getEnd.getCharacter else sourceLine.length
        val caretLine = " ".repeat(caretStart) + "^" * (caretEnd - caretStart)
        out.println(s"$blankLineNo |$caretLine")

      val contentLines = message.drop(message.indexOf(':') + 1).trim.split('\n')
      for contentLine <- contentLines do
        val descLine = " ".repeat(caretStart) + contentLine
        out.println(s"$blankLineNo |$descLine")

  def report(reportable: Reportable): Unit =
    buffer += reportable.diagnostic

  // Syntax Errors
  def reportSyntaxError(desc: String, range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Syntax error: $desc", Error, "parser")
    buffer += diagnostic

  // Name Errors
  def reportNameRedefined(range: Range, prevRange: Range): Unit =
    val diagnostic = Diagnostic(range, s"Name Error: name is already defined", Error, "checker")
    diagnostic.setRelatedInformation(
      List(new DiagnosticRelatedInformation(Location(source.uri, prevRange), "previously defined here")).asJava)
    buffer += diagnostic

  def reportNameUndefined(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Name Error: name is not defined", Error, "checker")
    buffer += diagnostic

  def reportMemberNotFound(range: Range, member: String, typ: Type): Unit =
    val diagnostic = Diagnostic(range, s"Name Error: '$member' is not a member of ${typ.show}", Error, "checker")
    buffer += diagnostic

  def reportMissingTypeAnnot(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: missing type annotation", Error, "checker")
    buffer += diagnostic

  def reportNotAssignable(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: not assignable", Error, "checker")
    buffer += diagnostic

  def reportMissingReturnValue(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: missing return value", Error, "checker")
    buffer += diagnostic

  def reportBreakOutOfLoop(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Control Flow Error: break statement outside of loop", Error, "checker")
    buffer += diagnostic

  def reportContinueOutOfLoop(range: Range): Unit =
    val diagnostic = Diagnostic(range, s"Control Flow Error: continue statement outside of loop", Error, "checker")
    buffer += diagnostic

  // Type Errors
  def reportTypeMismatch(range: Range, expected: Type | String, actual: Type): Unit =
    val expectedStr = expected match
      case t: Type => t.show
      case s: String => s
    val diagnostic = Diagnostic(range, multiLineMessage(
      "Type Error: type mismatch",
      s"expected: $expectedStr",
      s"actual:   ${actual.show}"), Error, "checker")
    buffer += diagnostic

  def reportNotCallable(range: Range, typ: Type): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: value of type ${typ.show} is not callable", Error, "checker")
    buffer += diagnostic

  def reportNotTerm(range: Range, name: String): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: '$name' is not a term", Error, "checker")
    buffer += diagnostic

  def reportNotType(range: Range, name: String): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: '$name' is not a type", Error, "checker")
    buffer += diagnostic

  def reportNotLang(range: Range, name: String): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: '$name' is not a language", Error, "checker")
    buffer += diagnostic

  def reportMissingArgs(range: Range, funType: FunType, actual: Int): Unit =
    val diagnostic = Diagnostic(range, multiLineMessage(
      s"Type Error: missing ${funType.arity - actual} argument(s)",
      s"note: function has type ${funType.show}"), Error, "checker")
    buffer += diagnostic

  def reportTooManyArgs(range: Range, funType: FunType): Unit =
    val diagnostic = Diagnostic(range, multiLineMessage(
      s"Type Error: too many arguments (expected ${funType.arity})",
      s"note: function has type ${funType.show}"), Error, "checker")
    buffer += diagnostic

  def reportAmbiguousOverload(range: Range, member: String, typ: Type, funTypes: List[FunType]): Unit =
    val diagnostic = Diagnostic(range, multiLineMessage(
      "Type Error: ambiguous overload",
      s"note: overloaded alternatives of $member in $typ have types",
      funTypes.map(_.show)), Error, "checker")
    buffer += diagnostic

  def reportNoMatchingOverload(range: Range, member: String, typ: Type, funTypes: List[FunType],
                               argTypes: List[Type]): Unit =
    val argTypesStr = argTypes.map(_.show).mkString(", ")
    val diagnostic = Diagnostic(range, multiLineMessage(
      s"Type Error: none of the overloaded alternatives of $member in $typ with types",
      funTypes.map(_.show),
      s"match the argument types ($argTypesStr)"), Error, "checker")
    buffer += diagnostic

  def reportMissingTypeArgs(range: Range, typeConstr: String): Unit =
    val diagnostic = Diagnostic(range, s"Type Error: missing type argument(s) for $typeConstr", Error, "checker")
    buffer += diagnostic

  def reportTypesUnrelated(range: Range, left: Type, right: Type): Unit =
    val diagnostic = Diagnostic(range, multiLineMessage(
      "Type Warning: types are unrelated",
      s"left:  ${left.show}",
      s"right: ${right.show}"), Warning, "checker")
    buffer += diagnostic

  private def multiLineMessage(lines: (String | List[String])*): String =
    lines.flatMap:
      case line: String => List(line)
      case lines: List[String] => lines.map("  " + _)
    .mkString("\n")