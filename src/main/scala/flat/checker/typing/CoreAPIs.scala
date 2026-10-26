package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.domain.{CharSet, RegEx, StrRE, given}
import flat.checker.flan.Show.show
import flat.checker.flan.sorting.{subsortOf, toSort}
import flat.checker.flan.tpd.*
import flat.checker.flan.untpd
import flat.checker.parsing.SyntaxError
import flat.checker.verif.Simplifier.simplify
import org.eclipse.lsp4j.Range as Span

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer

class CoreAPIs(using reporter: Reporter) extends LazyLogging:
  private val freshCounter = new AtomicInteger(0)

  def infer(node: untpd.Expr, ctx: TypingCtx): (Type, Expr, List[Stmt]) = node match
    // Constants
    case untpd.IntConst(v) => (IntType, IntConst(v)(node.range), Nil)
    case untpd.BoolConst(v) => (BoolType, BoolConst(v)(node.range), Nil)
    case untpd.CharConst(v) => (CharType, CharConst(v)(node.range), Nil)
    case untpd.StrConst(v) => (StrType, StrConst(v)(node.range), Nil)
    case untpd.NullConst() => (NullType, NullConst()(node.range), Nil)

    // Names
    case untpd.TermName(x) =>
      ctx.lookup(x) match
        case Some(ValInfo(_, typ)) => (narrow(x, typ, ctx), Var(x)(node.range), Nil)
        case Some(VarInfo(_, typ)) => (narrow(x, typ, ctx), Var(x)(node.range), Nil)
        case Some(ConstInfo(_, typ, value)) => (typ, value, Nil)
        case Some(_) =>
          reporter.report(ctx.uri, NotTerm(node.range))
          (NoType, Var(x)(node.range), Nil)
        case None =>
          reporter.report(ctx.uri, UndefinedName(node.range, x))
          (NoType, Var(x)(node.range), Nil)

    // Tuples
    case untpd.TupleExpr(es) =>
      val (types, elems, sides) = es.map(infer(_, ctx)).unzip3
      (TupleType(types), TupleExpr(elems)(node.range), sides.flatten)

    // Lambdas
    case untpd.LambdaExpr(params, body) =>
      throw new NotImplementedError("Lambda inference not implemented yet")

    // Operations: equality
    case untpd.BinaryExpr(e1, untpd.Ident("=="), e2) =>
      val (leftType, left, leftSide) = infer(e1, ctx)
      val (rightType, right, rightSide) = infer(e2, ctx)
      (BoolType, Eq(left, right)(node.range), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("!="), e2) =>
      val (leftType, left, leftSide) = infer(e1, ctx)
      val (rightType, right, rightSide) = infer(e2, ctx)
      (BoolType, Ne(left, right)(node.range), leftSide ++ rightSide)

    // Operations: logical that may introduce assumptions
    case untpd.BinaryExpr(e1, untpd.Ident("and"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, assume(left, ctx))
      (BoolType, And(left, right)(node.range), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("or"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, assume(Not(left), ctx))
      (BoolType, Or(left, right)(node.range), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("==>"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, assume(left, ctx))
      (BoolType, Implies(left, right)(node.range), leftSide ++ rightSide)
    case untpd.Ite(e, e1, e2) =>
      val (cond, condSide) = check(e, BoolType, ctx)
      val (typ, thenExpr, thenSide) = infer(e1, assume(cond, ctx))
      val (elseExpr, elseSide) = check(e2, typ, assume(Not(cond), ctx))
      (typ, Ite(cond, thenExpr, elseExpr)(node.range), condSide ++ thenSide ++ elseSide)

    // Operations: in
    case untpd.BinaryExpr(e, untpd.Ident("in"), untpd.TermName(x))
      if ctx.lookup(x).exists(_.isInstanceOf[LangInfo]) =>
      val (str, strSide) = check(e, StrType, ctx)
      val re = ctx.lookup(x).get.asInstanceOf[LangInfo].regEx
      (BoolType, StrIn(str, re)(node.range), strSide)
    case untpd.BinaryExpr(e1, op@untpd.Ident("in"), e2) =>
      inferOperation(e2, untpd.Ident("contains")(op.range), List(e1), node.range, ctx)

    // Operations: index
    case untpd.IndexAccess(e, ei) => inferOperation(e, untpd.Ident("__getitem__")(ei.range), List(ei), node.range, ctx)

    // Operations: string format
    case untpd.BinaryExpr(untpd.StrConst(fmt), untpd.Ident("%"), e) =>
      val args = e match
        case untpd.TupleExpr(es) => es
        case _ => List(e)
      (StrType, checkStrFormat(fmt, args, node.range, ctx), Nil)

    // Operation: list display
    case untpd.ListExpr(Nil) =>
      reporter.report(ctx.uri, MissingTypeAnnot(node.range))
      (NoType, NoExpr, Nil)
    case untpd.ListExpr(e :: es) =>
      val (elemType, first, firstSide) = infer(e, ctx)
      val elemSort = elemType.toSort
      val (rest, restSides) = es.map(check(_, elemSort, ctx)).unzip
      (ListType(elemSort), ListExpr(first :: rest)(elemSort, node.range), firstSide ++ restSides.flatten)

    case untpd.SetExpr(Nil) =>
      reporter.report(ctx.uri, MissingTypeAnnot(node.range))
      (NoType, NoExpr, Nil)
    case untpd.SetExpr(e :: es) =>
      val (elemType, first, firstSide) = infer(e, ctx)
      val elemSort = elemType.toSort
      val (rest, restSides) = es.map(check(_, elemSort, ctx)).unzip
      (SetType(elemSort), SetExpr(first :: rest)(elemSort, node.range), firstSide ++ restSides.flatten)

    case untpd.MapExpr(Nil) =>
      reporter.report(ctx.uri, MissingTypeAnnot(node.range))
      (NoType, NoExpr, Nil)
    case untpd.MapExpr((ek, ev) :: items) =>
      val (keyType, firstKey, firstKeySide) = infer(ek, ctx)
      val (valueType, firstValue, firstValueSide) = infer(ev, ctx)
      val keySort = keyType.toSort
      val valueSort = valueType.toSort
      val (restKeys, restKeySides) = items.map((k, v) => check(k, keySort, ctx)).unzip
      val (restValues, restValueSides) = items.map((k, v) => check(v, valueSort, ctx)).unzip
      (MapType(keySort, valueSort),
        MapExpr(firstKey :: restKeys, firstValue :: restValues)(keySort, valueSort, node.range),
        firstKeySide ++ firstValueSide ++ restKeySides.flatten ++ restValueSides.flatten)

    // Operations from the standard library
    case untpd.BinaryExpr(e1, op, e2) => inferOperation(e1, op, List(e2), node.range, ctx)
    case untpd.Access(e, op) => inferOperation(e, op, Nil, node.range, ctx)
    case untpd.Apply(untpd.Access(e, op), es) => inferOperation(e, op, es, node.range, ctx)

    // Function application: method call
    case untpd.Apply(untpd.TermName(m), es) if ctx.lookup(m).exists(_.isInstanceOf[MethodInfo]) =>
      val info = ctx.lookup(m).get.asInstanceOf[MethodInfo]
      val (args, argsSide) = checkArgs(es, info.paramTypes, node.range, ctx)
      val returnNames = info.returnType match
        case TupleType(ts) => ts.indices.map(_ => s"fresh_${freshCounter.getAndIncrement()}").toList
        case _ => List(s"fresh_${freshCounter.getAndIncrement()}")
      val returnVar = returnNames match
        case List(x) => Var(x)(node.range)
        case _ => TupleExpr(returnNames.map(Var(_)(node.range)))(node.range)
      (info.returnType, returnVar, argsSide ++ List(MethodCall(returnNames, m, args)(node.range)))

    // Built-in function application
    case untpd.Apply(id@untpd.TermName(f), es) if builtins.contains(f) =>
      f match
        case "len" => inferOperation(es.head, untpd.Ident("__len__")(id.range), es.tail, node.range, ctx)
        case "int" => es match
          case List(e) => inferOperation(e, untpd.Ident("__int__")(id.range), Nil, node.range, ctx)
          case List(e, eb) =>
            val (str, side) = check(e, StrType, ctx)
            val (base, baseSide) = check(eb, IntType, ctx)
            (IntType, StrToInt(str, base)(node.range), side ++ baseSide)
          case _ =>
            reporter.report(ctx.uri, ArityMismatch(node.range, 1, es.length))
            (NoType, NoExpr, Nil)
        case "str" => inferOperation(es.head, untpd.Ident("__str__")(id.range), es.tail, node.range, ctx)
        case "divmod" => es match
          case List(e1, e2) =>
            val (left, leftSide) = check(e1, IntType, ctx)
            val (right, rightSide) = check(e2, IntType, ctx)
            (TupleType(List(IntType, IntType)),
              TupleExpr(List(Div(left, right)(node.range), Mod(left, right)(node.range)))(node.range),
              leftSide ++ rightSide)
          case _ =>
            reporter.report(ctx.uri, ArityMismatch(node.range, 2, es.length))
            (NoType, NoExpr, Nil)
        case _ => throw NotImplementedError(s"Built-in function $f is not implemented")

    // Function application: others
    case untpd.Apply(e, es) =>
      val (typ, fun, funSide) = infer(e, ctx)
      typ match
        case funType: FunType =>
          val (args, argsSide) = checkArgs(es, funType.paramTypes, node.range, ctx)
          (funType.returnType, Apply(fun, args)(node.range), funSide ++ argsSide)
        case _ =>
          if typ != NoType then
            reporter.report(ctx.uri, TypeMismatch(node.range, "function type", typ.toString))
          (NoType, NoExpr, funSide)

    case _ =>
      throw NotImplementedError(s"Type inference for ${node.getClass.getSimpleName} is not implemented")

  private val builtins = Set("abs", "all", "any", "bool", "chr", "dict", "divmod", "filter",
    "hex", "int", "len", "list", "map", "max", "min", "oct", "ord", "reversed", "set", "str", "sum", "zip")

  private def narrow(x: String, typ: Type, ctx: TypingCtx): Type = typ match
    case OptType(t) =>
      val notNull = ctx.conds.exists:
        case Ne(Var(`x`), NullConst()) | Not(Eq(Var(`x`), NullConst())) => true
        case _ => false
      if notNull then t else typ
    case _ => typ

  private def checkStrFormat(fmt: String, args: List[untpd.Expr], span: Span, ctx: TypingCtx): Expr =
    val parts = ListBuffer.empty[Expr]
    var i = 0
    var k = 0
    while i < fmt.length do
      if fmt.drop(i).startsWith("%%") then
        parts += StrConst("%")
        i += 2
      else if fmt.drop(i).startsWith("%s") then
        val (part, side) = check(args(k), StrType, ctx)
        assert(side.isEmpty, "side effects not allowed in string format arguments")
        parts += part
        k += 1
        i += 2
      else if fmt(i) == '%' then
        parseFormatter(fmt.drop(i + 1), span, ctx) match
          case Some((f, n)) if k < args.length =>
            val (e, side) = check(args(k), IntType, ctx)
            assert(side.isEmpty, "side effects not allowed in string format arguments")
            parts += IntFormat(e, f)
            i += 1 + n
            k += 1
          case _ =>
            i = fmt.length
      else
        val s = fmt.drop(i).takeWhile(_ != '%')
        parts += StrConst(s)
        i += s.length

    if parts.isEmpty then StrConst("") else parts.reduce(StrConcat(_, _)(span))

  private def parseFormatter(f: String, span: Span, ctx: TypingCtx): Option[(NumStrFormat, Int)] =
    // flag: 0 for zero-padded
    val zeroPadded = f.startsWith("0")
    var i = 0
    if zeroPadded then
      i += 1
    // width
    val width = f.drop(i).takeWhile(_.isDigit)
    i += width.length
    if zeroPadded && width.isEmpty then
      reporter.report(ctx.uri, SyntaxError(span, "no width specified for zero-padded format"))
      return None

    // conversion
    if !Set('d', 'o', 'x', 'X').contains(f(i)) then
      reporter.report(ctx.uri, SyntaxError(span, s"invalid conversion specifier '${f(i)}'"))
      return None

    val conv = f(i)
    val formatter = NumStrFormat(zeroPadded = zeroPadded, width = if width.isEmpty then 0 else width.toInt,
      conv = conv)
    Some(formatter, i + 1)

  private def inferOperation(e: untpd.Expr, op: untpd.Ident, es: List[untpd.Expr],
                             span: Span, ctx: TypingCtx): (Type, Expr, List[Stmt]) =
    val (receiverType, receiver, receiverSide) = infer(e, ctx)
    if receiverType == NoType then
      return (NoType, NoExpr, receiverSide)

    val members = builtin.accessMember(receiverType, op.name)
    if members.isEmpty then
      reporter.report(ctx.uri, UndefinedName(op.range, op.name))
      return (NoType, NoExpr, receiverSide)

    members.filter(_.funType.arity == es.length) match
      case Nil =>
        reporter.report(ctx.uri, UndefinedName(op.range, op.name))
        (NoType, NoExpr, receiverSide)
      case List(m) =>
        val (args, argsSide) = checkArgs(es, m.funType.paramTypes, span, ctx)
        val expr = m.impl(receiver, args, span)
        (m.funType.returnType, expr, receiverSide ++ argsSide)
      case ms =>
        val (argTypes, args, argSides) = es.map(infer(_, ctx)).unzip3
        ms.filter(m => argTypes.zip(m.funType.paramTypes).forall(_.toSort.subsortOf(_))) match
          case Nil =>
            reporter.report(ctx.uri, TypeMismatch(span, "matching operation", s"${op.name} for type $receiverType"))
            (NoType, NoExpr, receiverSide ++ argSides.flatten)
          case List(m) =>
            val expr = m.impl(receiver, args, span)
            (m.funType.returnType, expr, receiverSide ++ argSides.flatten)
          case ms =>
            reporter.report(ctx.uri, TypeMismatch(span, "ambiguous overloaded operation", s"${op.name} for type $receiverType"))
            (NoType, NoExpr, receiverSide ++ argSides.flatten)

  def checkArgs(es: List[untpd.Expr], ts: List[Type], span: Span, ctx: TypingCtx): (List[Expr], List[Stmt]) =
    if es.length != ts.length then
      reporter.report(ctx.uri, ArityMismatch(span, es.length, ts.length))
    val (args, sides) = es.zip(ts).map(check(_, _, ctx)).unzip
    (args, sides.flatten)

  def check(node: untpd.Expr, expected: Type, ctx: TypingCtx): (Expr, List[Stmt]) =
    (node, expected.toSort) match
      case (untpd.TupleExpr(es), TupleType(ts)) if es.length == ts.length =>
        val (elems, sides) = es.zip(ts).map(check(_, _, ctx)).unzip
        (TupleExpr(elems)(node.range), sides.flatten)

      case (untpd.LambdaExpr(ps, e), FunType(ts, t)) =>
        if ps.length != ts.length then
          reporter.report(ctx.uri, ArityMismatch(node.range, ts.length, ps.length))
          return (NoExpr, Nil)

        val (params, localCtx) = resolveParams(ps, ts, ctx.pushed)
        val (body, side) = check(e, t, localCtx)
        assert(side.isEmpty, "side effects not allowed in lambda body")
        (LambdaExpr(params, body), Nil)

      case (untpd.ListExpr(es), ListType(t)) =>
        val (elems, sides) = es.map(check(_, t, ctx)).unzip
        (ListExpr(elems)(t, node.range), sides.flatten)
      case (untpd.SetExpr(es), SetType(t)) =>
        val (elems, sides) = es.map(check(_, t, ctx)).unzip
        (SetExpr(elems)(t, node.range), sides.flatten)
      case (untpd.MapExpr(items), MapType(tk, tv)) =>
        val (keys, keySides) = items.map((k, v) => check(k, tk, ctx)).unzip
        val (values, valueSides) = items.map((k, v) => check(v, tv, ctx)).unzip
        (MapExpr(keys, values)(tk, tv, node.range), keySides.flatten ++ valueSides.flatten)

      case (_, expectedSort) =>
        val (actual, expr, side) = infer(node, ctx)
        val actualSort = actual.toSort
        if actualSort.subsortOf(expectedSort) then
          (expr, side)
        else
          reporter.report(ctx.uri, TypeMismatch(node.range, expectedSort.show, actualSort.show))
          (expr, Nil)

  def expand(node: untpd.PExpr, ctx: TypingCtx): StrRE = node match
    case untpd.PChar(c) => RegEx.symbolSet(CharSet(c))
    case untpd.PStr(s) => RegEx.word(s.toList)
    case n@untpd.PRef(x) =>
      ctx.lookup(x) match
        case Some(LangInfo(_, r)) => r
        case Some(_) =>
          reporter.report(ctx.uri, TypeMismatch(n.range, "Lang", "not a language"))
          RegEx.Zero()
        case None =>
          reporter.report(ctx.uri, UndefinedName(n.range, x))
          RegEx.Zero()
    case untpd.PStar(l) => expand(l, ctx).star
    case untpd.PPlus(l) => expand(l, ctx).plus
    case untpd.POpt(l) => expand(l, ctx).opt
    case untpd.PRep(e, i: BigInt) => expand(e, ctx) ^ i.toInt
    case untpd.PRep(e, untpd.IntRange(i1, i2)) => expand(e, ctx).loop(i1.toInt, i2.map(_.toInt))
    case untpd.PConcat(l1, l2) => expand(l1, ctx) * expand(l2, ctx)
    case untpd.PUnion(l1, l2) => expand(l1, ctx) + expand(l2, ctx)
    case untpd.PCharSet(neg, items) =>
      val chars = items.flatMap:
        case c: Char => List(c)
        case untpd.CharRange(c1, c2) => (c1 to c2).toList
      RegEx.symbolSet(new CharSet(neg, chars.toSet))
    case untpd.PAllChar => RegEx.symbolSet(CharSet.full)

  def unfold(node: untpd.Type, ctx: TypingCtx): Type = node match
    case t@untpd.TypeName(x) =>
      ctx.lookup(x) match
        case Some(TypeInfo(_, typ)) => typ
        case Some(LangInfo(_, r)) => ReftType("s", StrType, StrIn(Var("s"), r))
        case Some(_) =>
          reporter.reportNotType(t.range, x)
          NoType
        case None => x match
          case "int" | "Int" => IntType
          case "bool" | "Bool" => BoolType
          case "char" | "Char" => CharType
          case "str" | "Str" => StrType
          case _ =>
            reporter.report(ctx.uri, UndefinedName(t.range, x))
            NoType

    case t@untpd.TypeApply(id, ts) =>
      id.name match
        case "list" | "List" =>
          if ts.length != 1 then
            reporter.report(ctx.uri, ArityMismatch(t.range, 1, ts.length))
          val elem = unfold(ts.head, ctx)
          ListType(elem)
        case "set" | "Set" =>
          if ts.length != 1 then
            reporter.report(ctx.uri, ArityMismatch(t.range, 1, ts.length))
          val elem = unfold(ts.head, ctx)
          SetType(elem)
        case "dict" | "Map" =>
          if ts.length != 2 then
            reporter.report(ctx.uri, ArityMismatch(t.range, 2, ts.length))
          val key = unfold(ts.head, ctx)
          val value = if ts.length >= 2 then unfold(ts(1), ctx) else NoType
          MapType(key, value)

    case untpd.TupleType(ts) => TupleType(ts.map(unfold(_, ctx)))
    case untpd.FunType(ts, t) => FunType(ts.map(unfold(_, ctx)), unfold(t, ctx))
    case untpd.OptType(t) => OptType(unfold(t, ctx))
    case untpd.ReftType(untpd.Param(id, Some(t)), e) =>
      val base = unfold(t, ctx)
      val info = ValInfo(id, base)
      val localCtx = ctx.pushed.declare(id.name, info)
      val (cond, side) = check(e, BoolType, localCtx)
      if side.nonEmpty then
        reporter.report(ctx.uri, NotConstant(e.range))
      ReftType(id.name, base, cond)
    case untpd.ReftType(untpd.Param(id, None), _) =>
      reporter.report(ctx.uri, MissingTypeAnnot(id.range))
      NoType

  def resolveParams(nodes: List[untpd.Param], expectedTypes: List[Type],
                    localCtx: TypingCtx): (List[(String, Type)], TypingCtx) =
    var ctx = localCtx
    val params = ListBuffer.empty[(String, Type)]
    for i <- nodes.indices do
      val id = nodes(i).ident
      val typ = nodes(i).typ match
        case Some(t) => unfold(t, ctx)
        case None =>
          expectedTypes.lift(i) match
            case Some(t) => t
            case None =>
              reporter.report(ctx.uri, MissingTypeAnnot(id.range))
              NoType
      val info = ValInfo(id, typ)
      ctx = declare(id, info, ctx)
      params += id.name -> typ
    (params.toList, ctx)

  def declare(id: untpd.Ident, info: Info, ctx: TypingCtx): TypingCtx =
    ctx.get(id.name) match
      case None =>
        ctx.declare(id.name, info)
      case Some(conflict) =>
        reporter.report(ctx.uri, RedefinedName(id.range, id.name))
        ctx

  def declare(items: List[(untpd.Ident, Info)], ctx: TypingCtx): TypingCtx =
    items.foldLeft(ctx) { case (c, (id, info)) => declare(id, info, c) }

  def assume(cond: Expr, ctx: TypingCtx): TypingCtx =
    cond.simplify match
      case Ne(Var(x), NullConst()) =>
        ctx.updatedWith(x):
          case ValInfo(id, OptType(t)) => ValInfo(id, t)
          case VarInfo(id, OptType(t)) => VarInfo(id, t)
      case And(e1, e2) =>
        assume(e2, assume(e1, ctx))
      case _ =>
        ctx