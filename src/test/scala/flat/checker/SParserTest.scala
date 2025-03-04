//package flat.checker
//
//import flat.checker.Bound.{NegInf, PosInf}
//import flat.checker.ast.*
//import flat.checker.ast.Expr.*
//import flat.checker.ast.MetaChar.*
//import flat.checker.ast.Op.*
//import flat.checker.ast.ReExpr.*
//import flat.checker.ast.Stmt.*
//import flat.checker.ast.Type.*
//import org.scalatest.funsuite.AnyFunSuite
//
//class SParserTest extends AnyFunSuite:
//  // AST factory
//  given Conversion[Int | Boolean | String, Literal] = Literal.apply
//
//  given Conversion[Op, OpExpr] = OpExpr.apply
//
//  given Conversion[String, ReExpr] = ReString.apply
//
//  given Conversion[MetaChar, ReExpr] = ReMetaChar.apply
//
//  def apply(fun: Expr, args: Expr*): Expr = Apply(fun, args.toSeq)
//
//  def concat(rs: ReExpr*): ReExpr = ReConcat(rs.toList)
//
//  def union(rs: ReExpr*): ReExpr = ReUnion(rs.toList)
//
//  def accept(code: String): Program =
//    SParser.parse(code) match
//      case Left(msg) => fail(msg)
//      case Right(p) => p
//
//  test("parse type alias"):
//    val alias = accept("(type number int)").typeAliases.head
//    alias match
//      case TypeAlias(Ident("number"), _) =>
//      case _ => fail("unexpected")
//
//  test("parse function definition"):
//    val fun = accept(
//      """(def f () int
//        |   ()
//        |   skip)
//        |""".stripMargin).funDefs.head
//    fun match
//      case FunDef(Ident("f"), _, _, _, _) =>
//      case _ => fail("unexpected")
//
//  test("parse simple types"):
//    val fun = accept(
//      """(def f (bool str 1 true false "") int
//        |   ()
//        |   skip)
//        |""".stripMargin).funDefs.head
//    fun match
//      case FunDef(_, Seq(t1, t2, t3, t4, t5, t6), t, _, _) =>
//        assert(t == IntType)
//        assert(t1 == BoolType)
//        assert(t2 == StringType)
//        assert(t3 == LiteralType(1))
//        assert(t4 == LiteralType(true))
//        assert(t5 == LiteralType(false))
//        assert(t6 == LiteralType(""))
//      case _ => fail("unexpected")
//
//  test("parse compound types"):
//    val fun = accept(
//      """(def f ([-> (int (-> true bool)) (-> int bool)]) (array (-> int int))
//        |   ()
//        |   skip)
//        |""".stripMargin).funDefs.head
//    fun match
//      case FunDef(_, Seq(t1), t2, _, _) =>
//        assert(t1 == FunType(Seq(IntType, FunType(Seq(LiteralType(true)), BoolType)), FunType(Seq(IntType), BoolType)))
//        assert(t2 == ArrayType(FunType(Seq(IntType), IntType)))
//      case _ => fail("unexpected")
//
//  test("parse range types"):
//    val fun = accept(
//      """(def f ([range 10 20] [range -inf 0]) (range 0 inf)
//        |   ()
//        |   skip)
//        |""".stripMargin).funDefs.head
//    fun match
//      case FunDef(_, Seq(t1, t2), t3, _, _) =>
//        assert(t1 == RangeType(10, 20))
//        assert(t2 == RangeType(NegInf, 0))
//        assert(t3 == RangeType(0, PosInf))
//      case _ => fail("unexpected")
//
//  test("parse lang types"):
//    val fun = accept(
//      """(def f ([lang (re.++ "abc" (re.range 'd' 'f') re.all-char)]
//        |        [lang (re.* (re.union (re.+ re.digit) (re.? ",")))]
//        |        [lang (re.loop 2 inf (re.++ (re.^ 3 re.ascii) (re.loop 2 4 re.word)))])
//        |       (lang (re.comp (re.range 'a' 'f')))
//        |   ()
//        |   skip)
//        |""".stripMargin).funDefs.head
//    fun match
//      case FunDef(_, Seq(t1, t2, t3), t4, _, _) =>
//        assert(t1 == LangType(concat("abc", ReRange('d', 'f'), ReAllChar)))
//        assert(t2 == LangType(ReStar(union(RePlus(DIGIT), ReOpt(",")))))
//        assert(t3 == LangType(ReLoop(2, PosInf, concat(RePow(3, ASCII), ReLoop(2, 4, WORD)))))
//        assert(t4 == LangType(ReComp(ReRange('a', 'f'))))
//      case _ => fail("unexpected")
//
//  test("parse statements"):
//    val body = accept(
//      """(def f (int) int
//        |   ()
//        |   ([while (> (var 1) 5) (:= (var 1) (- (var 1) 1))]
//        |    [if (> (var 1) 1) (:= (var 1) (- (var 1) 1)) (return (var 1))]
//        |    [:= (var 1) (+ (var 1) 1)]
//        |    [return (var 1)]))
//        |""".stripMargin).funDefs.head.body
//    body match
//      case StmtSeq(While(_, Assign(_, _)), StmtSeq(IfStmt(_, Assign(_, _), Return(_)), StmtSeq(Assign(_, _),
//      Return(_)))) =>
//      case _ => fail("unexpected")
//
//  test("parse expressions"):
//    val body = accept(
//      """(def f (int) int
//        |   ()
//        |   (return (g (> (var 1) 5) (- (var 1) 1) (+ (var 1) 1))))
//        |""".stripMargin).funDefs.head.body
//    body match
//      case Return(Apply(e, Seq(e1, e2, e3))) =>
//        assert(e == GlobalRef("g"))
//        assert(e1 == apply(GT, LocalRef(1), 5))
//        assert(e2 == apply(SUB, LocalRef(1), 1))
//        assert(e3 == apply(ADD, LocalRef(1), 1))
//      case _ => fail("unexpected")