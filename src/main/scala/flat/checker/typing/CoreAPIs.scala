package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.domain.{CharSet, RegEx, StrRE, given}
import flat.checker.flan.Show.show
import flat.checker.flan.TypeOps.erase
import flat.checker.flan.tpd.{*, given}
import flat.checker.flan.untpd
import flat.checker.parsing.SyntaxError
import flat.checker.verif.{PreNotProved, ReftNotProved}
import org.eclipse.lsp4j.Range as Span

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer

class CoreAPIs(using reporter: Reporter) extends LazyLogging:
  private val freshCounter = new AtomicInteger(0)

  def infer(node: untpd.Expr, ctx: TypingCtx): (Type, Expr, List[Stmt]) = node match
    // Constants
    case untpd.IntConst(v) => (IntType, IntConst(v), Nil)
    case untpd.BoolConst(v) => (BoolType, BoolConst(v), Nil)
    case untpd.StrConst(v) => (StrType, StrConst(v), Nil)
    case untpd.NullConst() => (NullType, NullConst(), Nil)

    // Names
    case untpd.TermName(x) =>
      ctx.lookup(x) match
        case Some(ValInfo(_, typ)) => (narrow(x, typ, ctx), Var(x), Nil)
        case Some(VarInfo(_, typ)) => (narrow(x, typ, ctx), Var(x), Nil)
        case Some(ConstInfo(_, typ, value)) => (typ, value, Nil)
        case Some(_) =>
          reporter.report(ctx.uri, NotTerm(node.range))
          (NoType, Var(x), Nil)
        case None =>
          reporter.report(ctx.uri, UndefinedName(node.range, x))
          (NoType, Var(x), Nil)

    // Tuples
    case untpd.TupleExpr(es) =>
      val (types, elems, sides) = es.map(infer(_, ctx)).unzip3
      (TupleType(types), TupleExpr(elems), sides.flatten)

    // Lambdas
    case untpd.LambdaExpr(params, body) =>
      throw new NotImplementedError("Lambda inference not implemented yet")

    // Operations: equality
    case untpd.BinaryExpr(e1, untpd.Ident("=="), e2) =>
      val (leftType, left, leftSide) = infer(e1, ctx)
      val (rightType, right, rightSide) = infer(e2, ctx)
      (BoolType, Eq(left, right), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("!="), e2) =>
      val (leftType, left, leftSide) = infer(e1, ctx)
      val (rightType, right, rightSide) = infer(e2, ctx)
      (BoolType, Ne(left, right), leftSide ++ rightSide)

    // Operations: logical that may introduce assumptions
    case untpd.BinaryExpr(e1, untpd.Ident("&&"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, ctx.assume(left))
      (BoolType, And(left, right), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("||"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, ctx.assume(Not(left)))
      (BoolType, Or(left, right), leftSide ++ rightSide)
    case untpd.BinaryExpr(e1, untpd.Ident("==>"), e2) =>
      val (left, leftSide) = check(e1, BoolType, ctx)
      val (right, rightSide) = check(e2, BoolType, ctx.assume(left))
      (BoolType, Implies(left, right), leftSide ++ rightSide)
    case untpd.Ite(e, e1, e2) =>
      val (cond, condSide) = check(e, BoolType, ctx)
      val (typ, thenExpr, thenSide) = infer(e1, ctx.assume(cond))
      val (elseExpr, elseSide) = check(e2, typ, ctx.assume(Not(cond)))
      (typ, Ite(cond, thenExpr, elseExpr), condSide ++ thenSide ++ elseSide)

    // Operations: string in lang
    case untpd.BinaryExpr(e, untpd.Ident("in"), untpd.TermName(x))
      if ctx.lookup(x).exists(_.isInstanceOf[LangInfo]) =>
      val (str, strSide) = check(e, StrType, ctx)
      val re = ctx.lookup(x).get.asInstanceOf[LangInfo].regEx
      (BoolType, StrIn(str, re), strSide)

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
      val elemSort = elemType.erase
      val (rest, restSides) = es.map(check(_, elemSort, ctx)).unzip
      (ListType(elemSort), ListExpr(first :: rest)(elemSort, noRange), firstSide ++ restSides.flatten)

    case untpd.SetExpr(Nil) =>
      reporter.report(ctx.uri, MissingTypeAnnot(node.range))
      (NoType, NoExpr, Nil)
    case untpd.SetExpr(e :: es) =>
      val (elemType, first, firstSide) = infer(e, ctx)
      val elemSort = elemType.erase
      val (rest, restSides) = es.map(check(_, elemSort, ctx)).unzip
      (SetType(elemSort), SetExpr(first :: rest)(elemSort, noRange), firstSide ++ restSides.flatten)

    case untpd.MapExpr(Nil) =>
      reporter.report(ctx.uri, MissingTypeAnnot(node.range))
      (NoType, NoExpr, Nil)
    case untpd.MapExpr((ek, ev) :: items) =>
      val (keyType, firstKey, firstKeySide) = infer(ek, ctx)
      val (valueType, firstValue, firstValueSide) = infer(ev, ctx)
      val keySort = keyType.erase
      val valueSort = valueType.erase
      val (restKeys, restKeySides) = items.map((k, v) => check(k, keySort, ctx)).unzip
      val (restValues, restValueSides) = items.map((k, v) => check(v, valueSort, ctx)).unzip
      (MapType(keySort, valueSort),
        MapExpr(firstKey :: restKeys, firstValue :: restValues)(keySort, valueSort, noRange),
        firstKeySide ++ firstValueSide ++ restKeySides.flatten ++ restValueSides.flatten)

    // Operations from the standard library
    case untpd.BinaryExpr(e1, op, e2) => inferOperation(e1, op, List(e2), node.range, ctx)
    case untpd.Access(e, op) => inferOperation(e, op, Nil, node.range, ctx)
    case untpd.Apply(untpd.Access(e, op), es) => inferOperation(e, op, es, node.range, ctx)

    // Function application: method call
    case untpd.Apply(untpd.TermName(f), es)
      if ctx.lookup(f).exists(_.isInstanceOf[MethodInfo]) =>
      val info = ctx.lookup(f).get.asInstanceOf[MethodInfo]
      val (args, argsSide) = checkArgs(es, info.paramTypes, node.range, ctx)
      val pre = for p <- info.requires yield
        Assert(p.subst(info.paramNames.zip(args).toMap), () => PreNotProved(node.range))
      val fresh = "fresh$" + freshCounter.getAndIncrement()
      val post = for p <- info.ensures yield
        Assume(Apply(p.subst(info.paramNames.zip(args).toMap), List(Var(fresh))))
      (info.returnType, Var(fresh), argsSide ++ pre ++ List(Declare(fresh, info.returnType), Havoc(fresh)) ++ post)

    // Function application: others
    case untpd.Apply(e, es) =>
      val (typ, fun, funSide) = infer(e, ctx)
      typ match
        case funType: FunType =>
          val (args, argsSide) = checkArgs(es, funType.paramTypes, node.range, ctx)
          (funType.returnType, Apply(fun, args), funSide ++ argsSide)
        case _ =>
          if typ != NoType then
            reporter.report(ctx.uri, TypeMismatch(node.range, "function type", typ.toString))
          (NoType, NoExpr, funSide)

    case _ =>
      throw NotImplementedError(s"Type inference for ${node.getClass.getSimpleName} is not implemented")

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
      case List(m) =>
        val (args, argsSide) = checkArgs(es, m.funType.paramTypes, span, ctx)
        val expr = m.impl(receiver, args)
        val opSide = m.side(receiver, for (arg, e) <- args.zip(es) yield (arg, e.range))
        (m.funType.returnType, expr, receiverSide ++ argsSide ++ opSide)
      case Nil =>
        reporter.report(ctx.uri, UndefinedName(op.range, op.name))
        (NoType, NoExpr, receiverSide)
      case _ =>
        throw UnsupportedOperationException(s"Ambiguous operation: ${op.name} for type $receiverType")

  private def checkArgs(es: List[untpd.Expr], ts: List[Type], span: Span, ctx: TypingCtx): (List[Expr], List[Stmt]) =
    if es.length != ts.length then
      reporter.report(ctx.uri, ArityMismatch(span, es.length, ts.length))
    val (args, sides) = es.zip(ts).map(check(_, _, ctx)).unzip
    (args, sides.flatten)

  def check(node: untpd.Expr, expected: Type, ctx: TypingCtx): (Expr, List[Stmt]) =
    (node, expected) match
      case (untpd.TupleExpr(es), TupleType(ts)) if es.length == ts.length =>
        val (elems, sides) = es.zip(ts).map(check(_, _, ctx)).unzip
        (TupleExpr(elems), sides.flatten)

      case (untpd.LambdaExpr(ps, e), FunType(ts, t)) =>
        ts match
          case _ if ts.length == ps.length =>
            val (params, localCtx) = resolveParams(ps, ts, ctx.pushed)
            val (body, side) = check(e, t, localCtx)
            assert(side.isEmpty, "side effects not allowed in lambda body")
            (Lambda(params, body), Nil)
          case List(TupleType(ts)) if ts.length == ps.length => // auto untupling
            val (params, localCtx) = resolveParams(ps, ts, ctx.pushed)
            val (body, side) = check(e, t, localCtx)
            assert(side.isEmpty, "side effects not allowed in lambda body")
            val param = "tuple" -> TupleType(ts.map(_.erase))
            val substMap = (for i <- ps.indices yield params(i)._1 -> TupleSelect(i, Var("tuple"))()).toMap
            (Lambda(List(param), body.subst(substMap)), Nil)
          case _ =>
            reporter.report(ctx.uri, ArityMismatch(node.range, ts.length, ps.length))
            (NoExpr, Nil)

      case _ =>
        val (actual, expr, side) = infer(node, ctx)
        if isSubtype(actual, expected) then // success
          (expr, side)
        else if isSubtype(actual, expected.erase) then // needs refinement checking
          (expr, side :+ Assert(hasType(expr, expected), () => ReftNotProved(node.range)))
        else // failure
          reporter.report(ctx.uri, TypeMismatch(node.range, expected.show, actual.show))
          (expr, Nil)

  private def isSubtype(left: Type, right: Type): Boolean =
    if left == right || left == NoType || right == NoType then true
    else (left, right) match
      case (ListType(t1), ListType(t2)) => isSubtype(t1, t2)
      case (SetType(t1), SetType(t2)) => isSubtype(t1, t2)
      case (MapType(k1, v1), MapType(k2, v2)) => isSubtype(k1, k2) && isSubtype(k2, k1) && isSubtype(v1, v2)
      case (RefinedType(_, t1, _), t2) => isSubtype(t1, t2)
      case (TupleType(ts1), TupleType(ts2)) => ts1.length == ts2.length && (ts1 zip ts2).forall(isSubtype)
      case (FunType(ps1, r1), FunType(ps2, r2)) =>
        ps1.length == ps2.length && (ps2 zip ps1).forall(isSubtype) && isSubtype(r1, r2)
      case (OptType(t1), OptType(t2)) => isSubtype(t1, t2)
      case (NullType, OptType(_)) => true
      case (t1, OptType(t2)) => isSubtype(t1, t2)
      case _ => false

  def hasType(expr: Expr, typ: Type): Expr = typ match
    case IntType | BoolType | StrType => BoolConst(true)
    case ListType(t) => ListForall(expr, Lambda(List("x" -> t), hasType(Var("x"), t)))
    case TupleType(ts) => mkAnd(for i <- ts.indices.toList yield hasType(TupleSelect(i, expr), ts(i)))
    case OptType(t) => Implies(Ne(expr, NullConst()), hasType(expr, t))
    case RefinedType(x, _, e) => e.subst(Map(x -> expr))
    case _ => throw new IllegalArgumentException(typ.show)

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
        case Some(LangInfo(_, r)) => RefinedType("s", StrType, StrIn(Var("s"), r))
        case Some(_) =>
          reporter.reportNotType(t.range, x)
          NoType
        case None =>
          x match
            case "int" | "Int" => IntType
            case "bool" | "Bool" => BoolType
            case "str" | "Str" => StrType
            case _ =>
              reporter.report(ctx.uri, UndefinedName(t.range, x))
              NoType

    case untpd.GenericType(untpd.Ident("List"), List(t)) => ListType(unfold(t, ctx))
    case untpd.GenericType(untpd.Ident("Set"), List(t)) => SetType(unfold(t, ctx))
    case untpd.GenericType(untpd.Ident("Map"), List(tk, tv)) => MapType(unfold(tk, ctx), unfold(tv, ctx))
    case t@untpd.GenericType(_, _) =>
      reporter.reportNameUndefined(t.range)
      NoType

    case untpd.TupleType(ts) => TupleType(ts.map(unfold(_, ctx)))
    case untpd.FunType(ts, t) => FunType(ts.map(unfold(_, ctx)), unfold(t, ctx))
    case untpd.OptType(t) => OptType(unfold(t, ctx))
    case untpd.RefinedType(untpd.Param(id, Some(t)), e) =>
      val base = unfold(t, ctx)
      val info = ValInfo(id, base)
      val localCtx = ctx.pushed.define(id.name, info)
      val (cond, side) = check(e, BoolType, localCtx)
      if side.nonEmpty then
        reporter.report(ctx.uri, NotConstant(e.range))
      RefinedType(id.name, base, cond)

  def resolveParams(nodes: List[untpd.Param], expectedTypes: List[Type],
                    localCtx: TypingCtx): (List[(String, Type)], TypingCtx) =
    var ctx = localCtx
    val params = ListBuffer.empty[(String, Type)]
    for i <- nodes.indices do
      val id = nodes(i).ident
      val typ = nodes(i).typ match
        case Some(t) => unfold(t, ctx)
        case None =>
          if expectedTypes.nonEmpty then expectedTypes(i)
          else
            reporter.report(ctx.uri, MissingTypeAnnot(id.range))
            NoType
      val info = ValInfo(id, typ)
      ctx = define(id, info, ctx)
      params += id.name -> typ
    (params.toList, ctx)

  def define(id: untpd.Ident, info: Info, ctx: TypingCtx): TypingCtx =
    ctx.get(id.name) match
      case None =>
        ctx.define(id.name, info)
      case Some(conflict) =>
        reporter.report(ctx.uri, RedefinedName(id.range, id.name))
        ctx