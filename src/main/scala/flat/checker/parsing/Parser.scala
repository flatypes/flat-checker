package flat.checker.parsing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.flan.untpd.*
import flat.checker.parsing.PosUtils.{*, given}
import flat.checker.parsing.TokenType.*
import org.eclipse.lsp4j.Range as Span

import scala.util.parsing.input

class TokenReader(tokens: List[Token]) extends input.Reader[Token]:
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: input.Position = input.NoPosition

  override def rest: input.Reader[Token] = TokenReader(tokens.tail)

class Parser(uri: String, text: String)(using reporter: Reporter) extends ExprParsers, LazyLogging:
  override type Elem = Token

  // Tokens
  given Conversion[TokenType, Parser[Token]] = t => elem(t.toString, _.typ == t)

  given Conversion[String, Parser[Token]] = s => elem(s"'$s'", tk => tk.typ == KEYWORD && tk.value == s)

  // Module
  def module: Parser[Module] = topStmt.* ^^ (Module(_))

  private def ident: Parser[Ident] = IDENTIFIER ^^ { tk => Ident(tk.value)(tk.span) }

  // Top-level statements
  private def topStmt: Parser[TopStmt] = importStmt | typeAlias | valDef | langDef | funDef

  private def importStmt: Parser[Import] =
    "from" ~>! ident ~ ("import" ~> rep1sep(ident, ",")) <~ NEWLINE ^^ { case id ~ ids => Import(id, ids) }

  private def typeAlias: Parser[TypeAlias] =
    "type" ~>! ident ~ ("=" ~> typ) <~ NEWLINE ^^ { case id ~ t => TypeAlias(id, t) }

  private def valDef: Parser[ValDef] =
    "val" ~>! ident ~ (":" ~> typ).? ~ ("=" ~> expr) <~ NEWLINE ^^ { case id ~ t ~ e => ValDef(id, t, e) }

  private def langDef: Parser[LangDef] =
    "lang" ~>! ident ~ ("=" ~> pExpr) <~ NEWLINE ^^ { case id ~ e => LangDef(id, e) }

  private def funDef: Parser[FunDef] =
    "def" ~>! ident ~ params ~ ("->" ~> typ).? ~ (NEWLINE ^^^ Nil | block(stmt.+))
      ^^ { case id ~ ps ~ t ~ body => FunDef(id, ps, t, body)(id.range) }

  private def params: Parser[List[Param]] = "(" ~> repsep(param, ",") <~ ")"

  private def param: Parser[Param] = ident ~ (":" ~> typ) ^^ { case x ~ t => Param(x, t) }

  private def block[T](p: Parser[T]): Parser[T] = ":" ~> NEWLINE ~> INDENT ~> p <~ DEDENT

  // Types
  private def typ: Parser[Type] = infixRight("->" ^^^ mkArrowType, optType)

  private def mkArrowType(left: Type, right: Type): Type = left match
    case TupleType(ts) => FunType(ts, right)(left.range.getStart)
    case _ => FunType(List(left), right)(left.range.getStart)

  private def optType: Parser[Type] =
    postfix(genericType | typeRef | parenType | refinedType, "?" ^^ { tk => t => OptType(t)(tk.span.getEnd) })

  private def genericType: Parser[GenericType] =
    ident ~ ("[" ~> rep1sep(typ, ",")) ~ "]"
      ^^ { case ctor ~ args ~ tk => GenericType(ctor, args)(tk.span.getEnd) }

  private def typeRef: Parser[TypeRef] = IDENTIFIER ^^ { tk => TypeRef(tk.value)(tk.span) }

  private def parenType: Parser[Type] =
    "(" ~ repsep(typ, ",") ~ ")" ^^ {
      case _ ~ List(t) ~ _ => t
      case tk1 ~ ts ~ tk2 => TupleType(ts)(Span(tk1.span.getStart, tk2.span.getEnd))
    }

  private def refinedType: Parser[RefinedType] =
    "{" ~ param ~ ("|" ~> expr) ~ "}"
      ^^ { case tk1 ~ p ~ e ~ tk2 => RefinedType(p, e)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  // Expressions
  private def expr: Parser[Expr] = lambda | ite | implies

  private def lambda: Parser[Expr] =
    "lambda" ~ ident ~ ("->" ~> expr) ^^ { case tk ~ x ~ e => Lambda(x, e)(Span(tk.span.getStart, e.range.getEnd)) }

  private def ite: Parser[Expr] =
    implies ~ ("?" ~>! implies) ~ (":" ~> implies) ^^ { case e ~ e1 ~ e2 => Ite(e, e1, e2) }

  // Expressions: binary, unary
  private def binaryOp(op: Parser[Token]): Parser[(Expr, Expr) => Expr] =
    op ^^ { tk => (e1, e2) => BinaryExpr(e1, Ident(tk.value)(tk.span), e2) }

  private def implies: Parser[Expr] = infixRight(binaryOp("==>"), or)

  private def or: Parser[Expr] = infixRight(binaryOp("||"), and)

  private def and: Parser[Expr] = infixRight(binaryOp("&&"), relational)

  private def relational: Parser[Expr] =
    chainedRelational("<=" | "<") | chainedRelational(">=" | ">")
      | infixNonAssoc(binaryOp("==" | "!=" | "in" | notIn), bitOr)

  private def chainedRelational(op: Parser[Token]): Parser[Expr] =
    bitOr ~ (op ~ bitOr).+
      ^^ { case left ~ cmps => ChainedExpr(left, cmps.map { case tk ~ e => (Ident(tk.value)(tk.span), e) }) }

  private def notIn: Parser[Token] =
    "!" ~ "in" ^^ { case tk1 ~ tk2 => Token(KEYWORD, "!in", Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def bitOr: Parser[Expr] = infixLeft(binaryOp("|"), bitXor)

  private def bitXor: Parser[Expr] = infixLeft(binaryOp("^"), bitAnd)

  private def bitAnd: Parser[Expr] = infixLeft(binaryOp("&"), bitShift)

  private def bitShift: Parser[Expr] = infixLeft(binaryOp("<<" | ">>"), additive)

  private def additive: Parser[Expr] = infixLeft(binaryOp("+" | "-"), multiplicative)

  private def multiplicative: Parser[Expr] = infixLeft(binaryOp("*" | "/" | "%"), unary)

  private def unaryOp(op: Parser[Token]): Parser[Expr => Expr] =
    op ^^ { tk => e => UnaryExpr(Ident("prefix_" + tk.value)(tk.span), e) }

  private def unary: Parser[Expr] = prefix(unaryOp("!" | "~" | "-"), term)

  private def term: Parser[Expr] =
    postfix(intConst | strConst | boolConst | nullConst | termRef | listExpr | setExpr | mapExpr
      | parenExpr | size, fieldAccessOp | indexAccessOp | applyOp | sliceOp)

  private def intConst: Parser[IntConst] = INT ^^ { tk => IntConst(parseInt(tk.value))(tk.span) }

  private def parseInt(s: String): BigInt =
    if s.startsWith("0b") || s.startsWith("0B") then BigInt(s.drop(2).replace("_", ""), 2)
    else if s.startsWith("0o") || s.startsWith("0O") then BigInt(s.drop(2).replace("_", ""), 8)
    else if s.startsWith("0x") || s.startsWith("0X") then BigInt(s.drop(2).replace("_", ""), 16)
    else BigInt(s.replace("_", ""), 10)

  private def unescapeChar(s: String): Char =
    val (c, j) = unescape(s, 1)
    assert(j == s.length - 1)
    c

  private def unescape(s: String, i: Int): (Char, Int) =
    s.charAt(i) match
      case '\\' => s.charAt(i + 1) match
        case c@('\\' | '\'' | '"') => (c, i + 2)
        case 'a' => ('\u0007', i + 2)
        case 'b' => ('\b', i + 2)
        case 'f' => ('\f', i + 2)
        case 'n' => ('\n', i + 2)
        case 'r' => ('\r', i + 2)
        case 't' => ('\t', i + 2)
        case 'v' => ('\u000B', i + 2)
        case 'u' => (Integer.parseInt(s.substring(i + 2, i + 6), 16).toChar, i + 6)
      case c => (c, i + 1)

  private def strConst: Parser[StrConst] = STR ^^ { tk => StrConst(unescapeStr(tk.value))(tk.span) }

  private def unescapeStr(s: String): String =
    val sb = new StringBuilder
    var i = 1
    while i < s.length - 1 do
      val (c, j) = unescape(s, i)
      sb += c
      i = j
    sb.toString

  private def boolConst: Parser[BoolConst] = ("true" | "false") ^^ { tk => BoolConst(tk.value.toBoolean)(tk.span) }

  private def nullConst: Parser[NullConst] = "null" ^^ { tk => NullConst()(tk.span) }

  private def termRef: Parser[TermRef] = IDENTIFIER ^^ { tk => TermRef(tk.value)(tk.span) }

  private def listExpr: Parser[ListExpr] =
    "[" ~ repsep(expr, ",") ~ "]"
      ^^ { case tk1 ~ es ~ tk2 => ListExpr(es)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def setExpr: Parser[SetExpr] =
    "{" ~ repsep(expr, ",") ~ "}"
      ^^ { case tk1 ~ es ~ tk2 => SetExpr(es)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def mapExpr: Parser[MapExpr] =
    "{" ~ repsep(mapItem, ",") ~ "}"
      ^^ { case tk1 ~ items ~ tk2 => MapExpr(items)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def mapItem: Parser[(Expr, Expr)] = expr ~ (":" ~> expr) ^^ { case k ~ v => (k, v) }

  private def parenExpr: Parser[Expr] =
    "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case _ ~ List(e) ~ _ => e
      case tk1 ~ es ~ tk2 => TupleExpr(es)(Span(tk1.span.getStart, tk2.span.getEnd))
    }

  private def fieldAccessOp: Parser[Expr => Expr] = "." ~> ident ^^ { f => e => MemberAccess(e, f) }

  private def indexAccessOp: Parser[Expr => Expr] =
    "[" ~> expr ~ "]" ^^ { case ei ~ tk => e => IndexAccess(e, ei)(tk.span.getEnd) }

  private def applyOp: Parser[Expr => Expr] =
    "(" ~>! repsep(expr, ",") ~ ")" ^^ { case es ~ tk => ef => Apply(ef, es)(tk.span.getEnd) }

  private def sliceOp: Parser[Expr => Expr] =
    "[" ~> expr.? ~ (":" ~> expr.?) ~ "]" ^^ { case e1 ~ e2 ~ tk => e => Slice(e, e1, e2)(tk.span.getEnd) }

  private def size: Parser[Expr] =
    "|" ~ expr ~ "|" ^^ { case tk1 ~ e ~ tk2 => Size(e)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  // Parsing expressions
  private def pExpr: Parser[PExpr] = infixRight("|" ^^^ (PUnion(_, _)), pConcat)

  private def pConcat: Parser[PExpr] = infixRight(success(PConcat(_, _)), pTerm)

  private def pTerm: Parser[PExpr] =
    postfix(pStr | pRStr | pRef | pParen, pStarOp | pPlusOp | pOptOp | pRepOp)

  private def pStr: Parser[PStr] = STR ^^ { tk => PStr(unescapeStr(tk.value)) }

  private def pRStr: Parser[PExpr] =
    R_STR ^^ { tk => RegParser(uri, tk.span.getStart).parse(tk.value.drop(2).dropRight(1)) }

  private def pRef: Parser[PRef] = IDENTIFIER ^^ { tk => PRef(tk.value)(tk.span) }

  private def pParen: Parser[PExpr] = "(" ~>! pExpr <~ ")"

  private def pStarOp: Parser[PExpr => PExpr] = "*" ^^^ (PStar(_))

  private def pPlusOp: Parser[PExpr => PExpr] = "+" ^^^ (PPlus(_))

  private def pOptOp: Parser[PExpr => PExpr] = "?" ^^^ (POpt(_))

  private def pRepOp: Parser[PExpr => PExpr] =
    "{" ~! pInt ~ ("," ~> pInt.?).? ~ "}" ^^ {
      case _ ~ i ~ None ~ _ => PRep(_, i)
      case tk1 ~ i1 ~ Some(i2) ~ tk2 => PRep(_, IntRange(i1, i2)(Span(tk1.span.getStart, tk2.span.getEnd)))
    }

  private def pInt: Parser[BigInt] = INT ^^ { tk => parseInt(tk.value) }

  // Local statements
  private def stmt: Parser[Stmt] =
    pass | varStmt | assumeStmt | assertStmt | abort | ifStmt | returnStmt | whileStmt | breakStmt | continueStmt
      | forStmt | assign | augAssign | exprStmt

  private def pass: Parser[Stmt] = "pass" <~! NEWLINE ^^^ Pass()

  private def varStmt: Parser[VarStmt] =
    "var" ~>! ident ~ (":" ~> typ).? ~ ("=" ~> expr).? <~ NEWLINE ^^ { case id ~ t ~ e => VarStmt(id, t, e) }

  private def assumeStmt: Parser[Assume] = "assume" ~>! expr <~ NEWLINE ^^ (Assume(_))

  private def assertStmt: Parser[Assert] = "assert" ~>! expr <~ NEWLINE ^^ (Assert(_))

  private def abort: Parser[Abort] = "raise" ~>! expr <~ NEWLINE ^^ (Abort(_))

  private def ifStmt: Parser[If] =
    "if" ~>! expr ~ block(stmt.+) ~ elsePart ^^ { case e ~ b1 ~ b2 => If(e, b1, b2) }

  private def elsePart: Parser[List[Stmt]] = "else" ~>! (ifStmt ^^ (List(_)) | block(stmt.+)) | success(Nil)

  private def returnStmt: Parser[Return] = "return" ~! expr.? <~ NEWLINE ^^ { case t ~ e => Return(e)(t.span) }

  private def whileStmt: Parser[While] =
    "while" ~>! expr ~ block(loopSpec.* ~! stmt.+) ^^ { case e ~ (specs ~ body) => While(e, specs, body) }

  private def loopSpec: Parser[LoopSpec] = "invariant" ~>! expr <~ NEWLINE ^^ (InvariantSpec(_))

  private def breakStmt: Parser[Break] = "break" <~! NEWLINE ^^ { tk => Break()(tk.span) }

  private def continueStmt: Parser[Continue] = "continue" <~! NEWLINE ^^ { tk => Continue()(tk.span) }

  private def forStmt: Parser[For] =
    "for" ~>! ident ~ ("in" ~> expr) ~ block(loopSpec.* ~! stmt.+)
      ^^ { case x ~ e ~ (specs ~ body) => For(x, e, specs, body) }

  private def assign: Parser[Assign] =
    (lExpr <~ "=") ~! expr <~ NEWLINE ^^ { case left ~ right => Assign(left, right) }

  private def augAssign: Parser[AugAssign] =
    ident ~ ("+=" | "-=" | "*=" | "/=" | "%=") ~! expr <~ NEWLINE
      ^^ { case left ~ tk ~ right => AugAssign(left, Ident(tk.value)(tk.span), right) }

  private def exprStmt: Parser[ExprStmt] = expr <~ NEWLINE ^^ (ExprStmt(_))

  // Left-hand side expressions
  private def lExpr: Parser[LExpr] = lRef | lTuple | lList

  private def lRef: Parser[LRef] = IDENTIFIER ^^ { tk => LRef(tk.value)(tk.span) }

  private def lTuple: Parser[LTuple] =
    "(" ~! repsep(lExpr, ",") ~ ")" ^^ { case tk1 ~ es ~ tk2 => LTuple(es)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def lList: Parser[LList] =
    "[" ~! repsep(lExpr, ",") ~ "]" ^^ { case tk1 ~ es ~ tk2 => LList(es)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  def parse(): Module =
    val lexer = Lexer(uri, text)
    val tokens = lexer.lex()
    if reporter.hasError then
      return Module.empty

    val reader = TokenReader(tokens)
    phrase(module)(reader) match
      case Success(mod, _) => mod
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(next.first.span, msg))
        Module.empty
