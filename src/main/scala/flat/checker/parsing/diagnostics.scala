package flat.checker.parsing

import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Range}

class SyntaxError(val range: Range, val message: String)
  extends Diagnostic(range, s"Syntax Error: $message", DiagnosticSeverity.Error, "flat-checker")

final class ParenMismatch(range: Range, val close: String, val open: String)
  extends SyntaxError(range, s"closing parenthesis '$close' does not match opening parenthesis '$open'")

final class UnmatchedParen(range: Range, val close: String)
  extends SyntaxError(range, s"unmatched closing parenthesis '$close'")

final class UnclosedParen(range: Range, val open: String)
  extends SyntaxError(range, s"unclosed opening parenthesis '$open'")

final class BadIndent(range: Range, val spaces: Int)
  extends SyntaxError(range, s"indentation of $spaces spaces is not a multiple of $tabSize")

final class InvalidEscapeSeq(range: Range)
  extends SyntaxError(range, "invalid escape sequence")

final class InvalidCharLit(range: Range)
  extends SyntaxError(range, "invalid character literal")