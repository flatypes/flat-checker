package flat.checker.py

import flat.checker.backend.core.Ident
import flat.checker.py.ast.*
import flat.checker.{Document, Issuer, Location, Position}

import scala.collection.mutable.ListBuffer
import scala.sys.process.{ProcessLogger, stringSeqToProcess}

class Unpickler(path: os.Path):
  private val doc = Document.fromPath(path)
  private val issuer = new Issuer

  def getTree: List[TopStmt] =
    val outLines = ListBuffer.empty[String]
    callScript(outLines)
    val jsonValue = ujson.read(outLines.mkString("\n"))
    val tree = jsonValue.arr.flatMap(topStmt).toList
    issuer.ensureNoError()
    tree

  private def callScript(outLines: ListBuffer[String]): Unit =
    val errLines = ListBuffer.empty[String]
    val logger = ProcessLogger(outLines += _, errLines += _)
    val returnCode = Seq("python3", "scripts/parse.py", path.toString) ! logger
    if returnCode != 0 then
      System.err.println("Python Syntax Error")
      for line <- errLines do System.err.println(line)
      System.exit(1)

  private type Parser[T] = ujson.Value => T

  private def list[T](p: Parser[T]): Parser[Seq[T]] = json => json.arr.map(p).toSeq

  extension (json: ujson.Value)
    private inline def |>[T](p: Parser[T]): T = p(json)
    private inline def int: Int = json.num.toInt
    private inline def opt: Option[ujson.Value] = if json.isNull then None else Some(json)

  private def location: Parser[Location] = json =>
    val m = json.obj
    val startLine = m("lineno").int
    val startOffset = m("col_offset").int
    val endLine = m("end_lineno").int
    val endOffset = m("end_col_offset").int
    Location(doc, Position(startLine - 1, startOffset), Position(endLine - 1, endOffset))

  private def topStmt: Parser[Option[TopStmt]] = json =>
    val m = json.obj
    val loc = m |> location
    m("_constr").str match
      case "TypeAlias" =>
        val name = (m("name") |> expr).asInstanceOf[Name]
        val ident = Ident(name.id).copyLocation(name)
        val value = m("value") |> expr
        Some(TypeAlias(ident, value).setLocation(loc))
      case "FunctionDef" =>
        val f = m("name").str
        val fOffset = m("name_col_offset").int
        val ident = Ident(f).setLocation(
          Location(doc, Position(loc.start.row, fOffset), Position(loc.start.row, fOffset + f.length)))
        val args = m("args") |> arguments
        val body = m("body") |> list(localStmt)
        val decorators = m("decorator_list").arr
        if decorators.nonEmpty then
          issuer.report(Unsupported("decorator", decorators.head |> location))
        val returns = m("returns").opt.map(expr)
        val typeParams = m("type_params").arr
        if typeParams.nonEmpty then
          issuer.report(Unsupported("type param", typeParams.head |> location))
        Some(FunctionDef(ident, args, body, returns).setLocation(loc))
      case "Import" | "ImportFrom" =>
        None
      case other =>
        issuer.report(Unsupported(other, loc))
        None

  private def arguments: Parser[Seq[Arg]] = json =>
    val m = json.obj
    assert(m("_constr").str == "arguments")
    val posOnlyArgs = m("posonlyargs").arr
    if posOnlyArgs.nonEmpty then
      issuer.report(Unsupported("pos only arg", posOnlyArgs.head |> location))
    val args = m("args") |> list(arg)
    val kwOnlyArgs = m("kwonlyargs").arr
    if kwOnlyArgs.nonEmpty then
      issuer.report(Unsupported("kw only arg", kwOnlyArgs.head |> location))
    val kwDefaults = m("kw_defaults").arr
    if kwDefaults.nonEmpty then
      issuer.report(Unsupported("kw default", kwDefaults.head |> location))
    val defaults = m("defaults").arr
    if defaults.nonEmpty then
      issuer.report(Unsupported("default", defaults.head |> location))
    args

  private def arg: Parser[Arg] = json =>
    val m = json.obj
    val loc = m |> location
    assert(m("_constr").str == "arg")
    val x = m("arg").str
    val ident = Ident(x).setLocation(Location(doc, loc.start, Position(loc.start.row, loc.start.offset + x.length)))
    val annot = m("annotation") |> expr
    Arg(ident, annot)

  private def localStmt: Parser[LocalStmt] = json =>
    val m = json.obj
    val loc = m |> location
    m("_constr").str match
      case "Assign" =>
        val targets = m("targets") |> list(expr)
        if targets.length > 1 then
          issuer.report(Unsupported("chaining targets", targets(1).loc))
        val value = m("value") |> expr
        Assign(targets.head, value).setLocation(loc)
      case "AnnAssign" =>
        val target = m("target") |> expr
        val ident = target match
          case name: Name => name.asIdent
          case _ =>
            issuer.report(Unsupported("not a name", target.loc))
            Ident("")
        val annot = m("annotation") |> expr
        val init = m("value").opt.map(expr)
        AnnAssign(ident, annot, init).setLocation(loc)
      case "AugAssign" =>
        val target = m("target") |> expr
        val attr = m("op") |> binOp
        val value = m("value") |> expr
        Assign(target, mkInfix(attr, target, value)).setLocation(loc)
      case "Raise" => // regarded as `assert False`
        Assert(Constant(false).setLocation(loc)).setLocation(loc)
      case "Assert" =>
        val test = m("test") |> expr
        Assert(test).setLocation(loc)
      case "Pass" =>
        Pass().setLocation(loc)
      case "If" =>
        val test = m("test") |> expr
        val body = m("body") |> list(localStmt)
        val orElse = m("orelse") |> list(localStmt)
        If(test, body, orElse).setLocation(loc)
      case "While" =>
        val test = m("test") |> expr
        val body = m("body") |> list(localStmt)
        val orElse = m("orelse") |> list(localStmt)
        if orElse.nonEmpty then
          issuer.report(Unsupported("else block in while-statement", orElse.head.loc))
        While(test, body).setLocation(loc)
      case "Break" =>
        Break().setLocation(loc)
      case "Return" =>
        val optValue = m("value").opt.map(expr)
        Return(optValue).setLocation(loc)
      case "Expr" =>
        val value = m("value") |> expr
        ExprStmt(value).setLocation(loc)
      case other =>
        issuer.report(Unsupported(other, loc))
        Pass()

  private def expr: Parser[Expr] = json =>
    val m = json.obj
    val loc = m |> location
    m("_constr").str match
      case "Constant" =>
        val value = m("value") match
          case ujson.Num(d) => d.toInt
          case ujson.Bool(b) => b
          case ujson.Str(s) => s
          case ujson.Null => null
          case other => throw IllegalStateException(s"illegal constant: $other")
        Constant(value).setLocation(loc)
      case "List" =>
        val values = m("elts") |> list(expr)
        ListExpr(values).setLocation(loc)
      case "Tuple" =>
        val values = m("elts") |> list(expr)
        TupleExpr(values).setLocation(loc)
      case "Name" =>
        val id = m("id").str
        Name(id).setLocation(loc)
      // unary, binary, and ternary expressions
      case "UnaryOp" =>
        val attr = m("op").obj("_constr").str match
          case "UAdd" => "__pos__"
          case "USub" => "__neg__"
          case "Not" => "__not__"
          case "Invert" => "__invert__"
          case _ => assert(false)
        val operand = m("operand") |> expr
        Call(Attribute(operand, attr), Seq.empty).setLocation(loc)
      case "BinOp" =>
        val attr = m("op") |> binOp
        val left = m("left") |> expr
        val right = m("right") |> expr
        mkInfix(attr, left, right)
      case "BoolOp" =>
        val attr = m("op").obj("_constr").str match
          case "And" => "__and__"
          case "Or" => "__or__"
        val values = m("values") |> list(expr)
        mkLeftAssoc(attr, values)
      case "Compare" =>
        val ops = m("ops").arr.map(_.obj("_constr").str)
        val left = m("left") |> expr
        val comparators = m("comparators") |> list(expr)
        val tests = for (op, (e1, e2)) <- ops zip ((left +: comparators) zip comparators) yield mkCmp(op, e1, e2)
        mkLeftAssoc("__and__", tests.toSeq)
      case "IfExp" =>
        val test = m("test") |> expr
        val body = m("body") |> expr
        val orElse = m("orelse") |> expr
        IfExp(test, body, orElse).setLocation(loc)
      // call and select
      case "Call" =>
        val func = m("func") |> expr
        val args = m("args") |> list(expr)
        val keywords = m("keywords").arr
        if keywords.nonEmpty then
          issuer.report(Unsupported("keyword", keywords.head |> location))
        Call(func, args).setLocation(loc)
      case "Attribute" =>
        val value = m("value") |> expr
        val attr = m("attr").str
        Attribute(value, attr).setLocation(loc)
      case "Subscript" =>
        val value = m("value") |> expr
        val m1 = m("slice").obj
        val slice: Slice | Expr =
          if m1("_constr").str == "Slice" then
            val loc1 = m1 |> location
            val lower = m1("lower").opt.map(expr)
            val upper = m1("upper").opt.map(expr)
            val optStep = m1("step").opt
            if optStep.isDefined then
              issuer.report(Unsupported("step", optStep.get |> location))
            Slice(lower, upper).setLocation(loc1)
          else m("slice") |> expr
        Subscript(value, slice).setLocation(loc)

  private def binOp: Parser[String] = json =>
    json.obj("_constr").str match
      case "Add" => "__add__"
      case "Sub" => "__sub__"
      case "Mult" => "__mul__"
      case "Div" => "__truediv__"
      case "FloorDiv" => "__floordiv__"
      case "Mod" => "__mod__"
      case "Pow" => "__pow__"
      case "LShift" => "__lshift__"
      case "RShift" => "__rshift__"
      case "BitOr" => "__or__"
      case "BitXor" => "__xor__"
      case "BitAnd" => "__and__"
      case "MatMult" => "__matmul__"
      case _ => assert(false)

  private def mkInfix(attr: String, left: Expr, right: Expr): Expr =
    Call(Attribute(left, attr), Seq(right))
      .setLocation(Location(doc, left.loc.start, right.loc.end))

  private def mkLeftAssoc(attr: String, tests: Seq[Expr]): Expr =
    require(tests.nonEmpty)
    tests.reduce(mkInfix(attr, _, _))

  private def mkCmp(cmpConstr: String, left: Expr, right: Expr): Expr =
    cmpConstr match
      case "Eq" => mkInfix("__eq__", left, right)
      case "NotEq" => mkInfix("__ne__", left, right)
      case "Lt" => mkInfix("__lt__", left, right)
      case "LtE" => mkInfix("__le__", left, right)
      case "Gt" => mkInfix("__gt__", left, right)
      case "GtE" => mkInfix("__ge__", left, right)
      case "Is" | "IsNot" =>
        val err = mkInfix("__is__", left, right)
        issuer.report(Unsupported("is", err.loc))
        err
      case "In" => mkInfix("__contains__", right, left)
      case "NotIn" =>
        val e = mkInfix("__contains__", right, left)
        Call(Attribute(e, "__not__"), Seq.empty).copyLocation(e)