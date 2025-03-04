package flat.checker.frontend.python

import flat.checker.frontend.python.ast.*
import flat.checker.{Document, Location, Position}

import scala.compiletime.uninitialized

object Parser:
  extension (obj: ujson.Obj)
    def kind: String = obj.value("_type").str

    def child(key: String): ujson.Obj = obj.value(key).asInstanceOf[ujson.Obj]

    def children(key: String): Seq[ujson.Obj] =
      obj.value(key).asInstanceOf[ujson.Arr].value.toSeq.map(_.asInstanceOf[ujson.Obj])

    def optionalChild(key: String): Option[ujson.Obj] =
      obj.value(key) match
        case o: ujson.Obj => Some(o)
        case ujson.Null => None
        case _ => throw UnsupportedOperationException()

  extension (value: ujson.Value)
    def int: Int = value.num.toInt

  def apply(json: String): Module =
    val obj = ujson.read(json).obj
    val source = obj.value("_source").str
    document = Document.fromPath(os.Path(source))
    parseModule(obj)

  private var document: Document = uninitialized

  private def parseModule(tree: ujson.Obj): Module =
    val b = tree.children("body").map(parseStmt)
    Module(b)

  private def locational(json: ujson.Obj)(node: Node): node.type =
    val r1 = json.value("lineno").int - 1
    val c1 = json.value("col_offset").int
    val r2 = json.value("end_lineno").int - 1
    val c2 = json.value("end_col_offset").int
    node.loc = Location(document, Position(r1, c1), Position(r2, c2))
    node

  private def parseStmt(tree: ujson.Obj): Stmt = locational(tree) {
    tree.kind match
      case "ImportFrom" =>
        val m = tree.value("module").str
        val as = tree.children("names").map(parseAlias)
        ImportFrom(m, as)
      case "FunctionDef" =>
        val f = tree.value("name").str
        val xs = tree.child("args").children("args").map(parseArg)
        val b = tree.children("body").map(parseStmt)
        val r = parseExpr(tree.child("returns"))
        FunctionDef(f, xs, b, r)
      case "TypeAlias" =>
        val t = parseExpr(tree.child("name"))
        val e = parseExpr(tree.child("value"))
        TypeAlias(t, e)
      case "Assign" =>
        val xs = tree.children("targets").map(parseExpr)
        if xs.length != 1 then throw UnsupportedOperationException("multiple left-values")
        val e = parseExpr(tree.child("value"))
        Assign(xs.head, e)
      case "AnnAssign" =>
        val x = parseExpr(tree.child("target"))
        val a = parseExpr(tree.child("annotation"))
        val e = parseExpr(tree.child("value"))
        AnnAssign(x, a, e)
      case "Assert" =>
        val e = parseExpr(tree.child("test"))
        Assert(e)
      case "Pass" => Pass()
      case "If" =>
        val e = parseExpr(tree.child("test"))
        val b1 = tree.children("body").map(parseStmt)
        val b2 = tree.children("orelse").map(parseStmt)
        If(e, b1, b2)
      case "While" =>
        val e = parseExpr(tree.child("test"))
        val b1 = tree.children("body").map(parseStmt)
        While(e, b1)
      case "Return" =>
        val oe = tree.optionalChild("value").map(parseExpr)
        Return(oe)
      case "Expr" =>
        val e = parseExpr(tree.child("value"))
        ExprStmt(e)
  }

  private def parseAlias(tree: ujson.Obj): Alias = locational(tree) {
    assert(tree.kind == "alias")
    val x = tree.value("name").str
    val oy = tree.value("asname") match
      case ujson.Null => None
      case ujson.Str(s) => Some(s)
      case _ => assert(false)
    Alias(x, oy)
  }

  private def parseArg(tree: ujson.Obj): Arg = locational(tree) {
    assert(tree.kind == "arg")
    val x = tree.value("arg").str
    val a = parseExpr(tree.child("annotation"))
    Arg(x, a)
  }

  private def parseExpr(tree: ujson.Obj): Expr = locational(tree) {
    tree.kind match
      case "Constant" =>
        val v = tree.value("value") match
          case ujson.Num(v) => v.toInt
          case ujson.Bool(b) => b
          case ujson.Str(s) => s
          case _ => throw UnsupportedOperationException()
        Constant(v)
      case "JoinedStr" =>
        val es = tree.children("values").map(parseExpr)
        JoinedStr(es)
      case "List" =>
        val es = tree.children("elts").map(parseExpr)
        ListExpr(es)
      case "Tuple" =>
        val es = tree.children("elts").map(parseExpr)
        TupleExpr(es)
      case "Name" => Name(tree.value("id").str)
      case "UnaryOp" =>
        val op = parseOpUnary(tree.child("op"))
        val e = parseExpr(tree.child("operand"))
        UnaryOp(op, e)
      case "BinOp" =>
        val e1 = parseExpr(tree.child("left"))
        val op = parseOpBin(tree.child("op"))
        val e2 = parseExpr(tree.child("right"))
        BinOp(e1, op, e2)
      case "BoolOp" =>
        val op = parseOpBool(tree.child("op"))
        val es = tree.children("values").map(parseExpr)
        BoolOp(op, es)
      case "CompareOp" =>
        val e = parseExpr(tree.child("left"))
        val ops = tree.children("ops").map(parseOpCompare)
        val es = tree.children("comparators").map(parseExpr)
        Compare(e, ops, es)
      case "Call" =>
        val e = parseExpr(tree.child("func"))
        val es = tree.children("args").map(parseExpr)
        Call(e, es)
      case "IfExp" =>
        val e = parseExpr(tree.child("test"))
        val e1 = parseExpr(tree.child("body"))
        val e2 = parseExpr(tree.child("orelse"))
        IfExp(e, e1, e2)
      case "Attribute" =>
        val e = parseExpr(tree.child("value"))
        val x = tree.value("attr").str
        Attribute(e, x)
      case "Subscript" =>
        val e = parseExpr(tree.child("value"))
        val e1 = parseExpr(tree.child("slice"))
        Subscript(e, e1)
      case "Slice" =>
        val oe1 = tree.optionalChild("lower").map(parseExpr)
        val oe2 = tree.optionalChild("upper").map(parseExpr)
        val oe3 = tree.optionalChild("step").map(parseExpr)
        Slice(oe1, oe2, oe3)
  }

  import OpUnary.*

  private def parseOpUnary(tree: ujson.Obj): OpUnary =
    tree.kind match
      case "UAdd" => UAdd
      case "USub" => USub
      case "Not" => Not
      case other => throw UnsupportedOperationException(s"unary op: $other")

  import OpBin.*

  private def parseOpBin(tree: ujson.Obj): OpBin =
    tree.kind match
      case "Add" => Add
      case "Sub" => Sub
      case other => throw UnsupportedOperationException(s"binary op: $other")

  import OpBool.*

  private def parseOpBool(tree: ujson.Obj): OpBool =
    tree.kind match
      case "And" => And
      case "Or" => Or
      case other => throw UnsupportedOperationException(s"Boolean op: $other")

  import OpCompare.*

  private def parseOpCompare(tree: ujson.Obj): OpCompare =
    tree.kind match
      case "Eq" => Eq
      case "NotEq" => NotEq
      case "Lt" => Lt
      case "LtE" => LtE
      case "Gt" => Gt
      case "GtE" => GtE
      case "In" => In
      case "NotIn" => NotIn
      case other => throw UnsupportedOperationException(s"compare op: $other")