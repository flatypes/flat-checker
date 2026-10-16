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

  def topLevel: Parser[List[TopStmt]] = topStmt.*

  private def ident: Parser[Ident] = IDENTIFIER ^^ { tk => Ident(tk.value)(tk.span) }

  // Top-level statements
  private def topStmt: Parser[TopStmt] = importStmt | typeAlias | valDef | langDef | funDef

  private def importStmt: Parser[Import] =
    "from" ~>! ident ~ ("import" ~> rep1sep(ident, ",")) <~ NEWLINE ^^ { case id ~ ids => Import(id, ids) }

  private def typeAlias: Parser[TypeAlias] =
    "type" ~>! ident ~ ("=" ~> typ) <~ NEWLINE ^^ { case id ~ t => TypeAlias(id, t) }

  private def valDef: Parser[ConstDef] =
    "val" ~>! ident ~ ("=" ~> expr) <~ NEWLINE ^^ { case id ~ e => ConstDef(id, e) }

  private def langDef: Parser[LangDef] =
    "lang" ~>! ident ~ ("=" ~> pExpr) <~ NEWLINE ^^ { case id ~ e => LangDef(id, e) }

  private def funDef: Parser[FunDef] =
    "def" ~>! ident ~ params ~ ("->" ~> typ).?
      ~ (NEWLINE ^^^ new~(new~(Nil, Nil), Nil) | block(requireSpec.* ~ ensureSpec.* ~ stmt.+))
      ^^ { case id ~ ps ~ t ~ (pre ~ post ~ body) => FunDef(id, ps, t, pre, post, body)(id.range) }

  private def params: Parser[List[Param]] = "(" ~> repsep(param, ",") <~ ")"

  private def param: Parser[Param] = ident ~ (":" ~> typ).? ^^ { case x ~ t => Param(x, t) }

  private def block[T](p: Parser[T]): Parser[T] = ":" ~> NEWLINE ~> INDENT ~> p <~ DEDENT

  private def requireSpec: Parser[Expr] = "requires" ~>! expr <~ NEWLINE

  private def ensureSpec: Parser[Expr] = "ensures" ~>! expr <~ NEWLINE

  // Types
  private def typ: Parser[Type] = infixR("->" ^^^ mkArrowType, optType)

  private def mkArrowType(left: Type, right: Type): Type = left match
    case TupleType(ts) => FunType(ts, right)(left.range.getStart)
    case _ => FunType(List(left), right)(left.range.getStart)

  private def optType: Parser[Type] =
    postfix(stdType | typeName | parenType | reftType, "?" ^^ { tk => t => OptType(t)(tk.span.getEnd) })

  private def stdType: Parser[Type] =
    "Int" ^^ { tk => IntType()(tk.span) }
      | "Bool" ^^ { tk => BoolType()(tk.span) }
      | "Str" ^^ { tk => StrType()(tk.span) }
      | "List" ~ ("[" ~> typ) ~ "]" ^^ { case tk1 ~ t ~ tk2 => ListType(t)(Span(tk1.span.getStart, tk2.span.getEnd)) }
      | "Set" ~ ("[" ~> typ) ~ "]" ^^ { case tk1 ~ t ~ tk2 => SetType(t)(Span(tk1.span.getStart, tk2.span.getEnd)) }
      | "Map" ~ ("[" ~> typ) ~ ("," ~> typ) ~ "]"
      ^^ { case tk1 ~ k ~ v ~ tk2 => MapType(k, v)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  private def typeName: Parser[TypeName] = IDENTIFIER ^^ { tk => TypeName(tk.value)(tk.span) }

  private def parenType: Parser[Type] =
    "(" ~ repsep(typ, ",") ~ ")" ^^ {
      case _ ~ List(t) ~ _ => t
      case tk1 ~ ts ~ tk2 => TupleType(ts)(Span(tk1.span.getStart, tk2.span.getEnd))
    }

  private def reftType: Parser[ReftType] =
    "{" ~ param ~ ("|" ~> expr) ~ "}"
      ^^ { case tk1 ~ p ~ e ~ tk2 => ReftType(p, e)(Span(tk1.span.getStart, tk2.span.getEnd)) }

  // Expressions
  private def expr: Parser[Expr] = lambda | ite | implies

  private def lambda: Parser[Expr] =
    "lambda" ~ lambdaParams ~ ("->" ~> expr)
      ^^ { case tk ~ ids ~ e => LambdaExpr(ids, e)(Span(tk.span.getStart, e.range.getEnd)) }

  private def lambdaParams: Parser[List[Param]] = ident ^^ { id => List(Param(id, None)) } | params

  private def ite: Parser[Expr] =
    implies ~ ("?" ~>! implies) ~ (":" ~> implies) ^^ { case e ~ e1 ~ e2 => Ite(e, e1, e2) }

  // Expressions: binary, unary
  private def binaryOp(op: Parser[Token]): Parser[(Expr, Expr) => Expr] =
    op ^^ { tk => (e1, e2) => BinaryExpr(e1, Ident(tk.value)(tk.span), e2) }

  private def implies: Parser[Expr] = infixR(binaryOp("==>"), or)

  private def or: Parser[Expr] = infixR(binaryOp("||"), and)

  private def and: Parser[Expr] = infixR(binaryOp("&&"), relational)

  private def relational: Parser[Expr] =
    chainedRelational("<=" | "<") | chainedRelational(">=" | ">")
      | infixNonAssoc(binaryOp("==" | "!=" | "in") | notIn, bitOr)

  private def chainedRelational(op: Parser[Token]): Parser[Expr] =
    bitOr ~ (op ~ bitOr).+ ^^ {
      case e ~ cmps =>
        val conds = for i <- cmps.indices yield
          val op = cmps(i)._1
          BinaryExpr(if i == 0 then e else cmps(i - 1)._2, Ident(op.value)(op.span), cmps(i)._2)
        val andOp = Ident("&&")(e.range.getStart)
        conds.reduceRight(BinaryExpr(_, andOp, _))
    }

  private def notIn: Parser[(Expr, Expr) => Expr] =
    "!" ~ "in" ^^ {
      case tk1 ~ tk2 =>
        val inOp = Ident("in")(tk2.span)
        val notOp = Ident("prefix_!")(tk1.span)
        (e1, e2) => Access(BinaryExpr(e1, inOp, e2), notOp)(Span(e1.range.getStart, e2.range.getEnd))
    }

  private def bitOr: Parser[Expr] = infixL(binaryOp("|"), bitXor)

  private def bitXor: Parser[Expr] = infixL(binaryOp("^"), bitAnd)

  private def bitAnd: Parser[Expr] = infixL(binaryOp("&"), bitShift)

  private def bitShift: Parser[Expr] = infixL(binaryOp("<<" | ">>"), additive)

  private def additive: Parser[Expr] = infixL(binaryOp("+" | "-"), multiplicative)

  private def multiplicative: Parser[Expr] = infixL(binaryOp("*" | "/" | "%"), unary)

  private def unaryOp(op: Parser[Token]): Parser[Expr => Expr] =
    op ^^ { tk => e => Access(e, Ident("prefix_" + tk.value)(tk.span))(Span(tk.span.getStart, e.range.getEnd)) }

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

  private def termRef: Parser[TermName] = IDENTIFIER ^^ { tk => TermName(tk.value)(tk.span) }

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

  private def fieldAccessOp: Parser[Expr => Expr] =
    "." ~> ident ^^ { f => e => Access(e, f)(Span(e.range.getStart, f.range.getEnd)) }

  private def indexAccessOp: Parser[Expr => Expr] =
    "[" ~ expr ~ "]" ^^ { case tk1 ~ ei ~ tk =>
      val member = Ident("select")(tk1.span)
      e => Apply(Access(e, member)(Span(e.range.getStart, tk1.span.getEnd)), List(ei))(tk.span.getEnd)
    }

  private def applyOp: Parser[Expr => Expr] =
    "(" ~>! repsep(expr, ",") ~ ")" ^^ { case es ~ tk => ef => Apply(ef, es)(tk.span.getEnd) }

  private def sliceOp: Parser[Expr => Expr] =
    "[" ~ expr.? ~ (":" ~> expr.?) ~ "]" ^^ {
      case tk1 ~ e1 ~ e2 ~ tk =>
        val member = Ident("slice")(tk1.span)
        val arg1 = e1.getOrElse(IntConst(0)(tk1.span))
        val args = if e2.isEmpty then List(arg1) else List(arg1, e2.get)
        e => Apply(Access(e, member)(Span(e.range.getStart, tk1.span.getEnd)), args)(tk.span.getEnd)
    }

  private def size: Parser[Expr] =
    "|" ~ expr ~ "|"
      ^^ { case tk1 ~ e ~ tk2 => Access(e, Ident("size")(tk1.span))(Span(tk1.span.getStart, tk2.span.getEnd)) }

  // Parsing expressions
  private def pExpr: Parser[PExpr] = infixR("|" ^^^ (PUnion(_, _)), pConcat)

  private def pConcat: Parser[PExpr] = infixR(success(PConcat(_, _)), pTerm)

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
    pass | varDecl | assumeStmt | assertStmt | abort | ifStmt | returnStmt | whileStmt | breakStmt | continueStmt
      | forStmt | assign | augAssign | exprStmt

  private def pass: Parser[Stmt] = "pass" <~! NEWLINE ^^^ Pass()

  private def varDecl: Parser[VarDecl] =
    "var" ~>! pattern ~ ("=" ~> expr) <~ NEWLINE ^^ { case l ~ r => VarDecl(l, r) }

  private def pattern: Parser[Pat] = patternVar | tuplePattern

  private def patternVar: Parser[PatVar] = ident ~ (":" ~> typ).? ^^ { case id ~ t => PatVar(id, t) }

  private def tuplePattern: Parser[TuplePat] =
    "(" ~! repsep(pattern, ",") ~ ")"
      ^^ { case tk1 ~ ps ~ tk2 => TuplePat(ps)(Span(tk1.span.getStart, tk2.span.getEnd)) }

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
    (ident <~ "=") ~! expr <~ NEWLINE ^^ { case left ~ right => Assign(left, right) }

  private def augAssign: Parser[Assign] =
    ident ~ ("+=" | "-=" | "*=" | "/=" | "%=") ~! expr <~ NEWLINE ^^ {
      case id ~ tk ~ e =>
        val op = Ident(tk.value.dropRight(1))(tk.span)
        Assign(id, BinaryExpr(TermName(id.name)(id.range), op, e))
    }

  private def exprStmt: Parser[ExprStmt] = expr <~ NEWLINE ^^ (ExprStmt(_))

  def parse(): Module =
    val lexer = Lexer(uri, text)
    val tokens = lexer.lex()
    if reporter.hasError then
      return Module(uri)

    val reader = TokenReader(tokens)
    phrase(topLevel)(reader) match
      case Success(bd, _) => Module(uri, bd)
      case NoSuccess(msg, next) =>
        reporter.report(uri, SyntaxError(next.first.span, msg))
        Module(uri)
