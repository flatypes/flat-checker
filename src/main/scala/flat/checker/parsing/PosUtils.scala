package flat.checker.parsing

import org.eclipse.lsp4j.{Position, Range as Span}

import scala.util.parsing.input

object PosUtils:
  given Conversion[input.Position, Position] = p => Position(p.line - 1, p.column - 1)

  extension (pos: Position)
    def +(n: Int): Position = Position(pos.getLine, pos.getCharacter + n)

    def +(other: Position): Position =
      if other.getLine == 0 then Position(pos.getLine, pos.getCharacter + other.getCharacter)
      else Position(pos.getLine + other.getLine, other.getCharacter)

  given Conversion[Position, Span] = pos => Span(pos, pos + 1)
