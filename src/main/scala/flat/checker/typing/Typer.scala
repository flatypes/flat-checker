package flat.checker.typing

import com.typesafe.scalalogging.LazyLogging
import flat.checker.Reporter
import flat.checker.domain.StrREOps.NumStrFormat
import flat.checker.domain.{RegEx, StrRE, given}
import flat.checker.flan.*
import flat.checker.flan.Show.*
import flat.checker.flan.TypeOps.*
import flat.checker.flan.tpd.*
import flat.checker.verif.ReftNotProvedError
import org.eclipse.lsp4j.{Position, Range}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Typer(body: ListBuffer[Stmt])(using reporter: Reporter) extends LazyLogging:
  def normalize(node: untpd.Type, ctx: Ctx): Type = node match
    case t@untpd.TypeName(x) =>
      ctx.lookup(x) match
        case Some(TypeInfo(typ)) => typ
        case Some(LangInfo(r)) => RefinedType(strType, StringInLang(Var("_"), r))
        case Some(_) =>
          reporter.reportNotType(t.range, x)
          NoType
        case None =>
          x match
            case "int" | "Int" => IntType
            case "bool" | "Bool" => BoolType
            case "char" | "Char" => CharType
            case "str" | "String" => strType
            case _ =>
              reporter.reportNameUndefined(t.range)
              NoType

    // Generic types
    case untpd.GenericType("list" | "Seq", List(t)) => ListType(normalize(t, ctx))
    case untpd.GenericType("set" | "Set", List(t)) => SetType(normalize(t, ctx))
    case untpd.GenericType("dict" | "Map", List(tk, tv)) => DictType(normalize(tk, ctx), normalize(tv, ctx))
    case t@untpd.GenericType(_, _) =>
      reporter.reportNameUndefined(t.range)
      NoType

    // Compound types
    case untpd.TupleType(ts) => TupleType(ts.map(normalize(_, ctx)))
    case untpd.FunType(ts, t) => FunType(ts.map(normalize(_, ctx)), normalize(t, ctx))
    case untpd.NullableType(t) => NullableType(normalize(t, ctx))
    case _ => throw UnsupportedOperationException(s"normalize ${node.getClass.getSimpleName}")

  def translate(node: untpd.Lang, ctx: Ctx): StrRE = node match
    case untpd.LangConst(s) => RegEx.word(s.toList)
    case n@untpd.LangName(x) =>
      ctx.lookup(x) match
        case Some(LangInfo(r)) => r
        case Some(_) =>
          reporter.reportNotLang(n.range, x)
          RegEx.Zero()
        case None =>
          reporter.reportNameUndefined(n.range)
          RegEx.Zero()
    case untpd.RegEx(r) => r
    case untpd.LangStar(l) => translate(l, ctx).star
    case untpd.LangPlus(l) => translate(l, ctx).plus
    case untpd.LangOpt(l) => translate(l, ctx).opt
    case untpd.LangPower(l, n) => translate(l, ctx) ^ n
    case untpd.LangLoop(l, n1, Some(n2)) => translate(l, ctx).loop(n1, Some(n2))
    case untpd.LangLoop(l, n1, None) => translate(l, ctx).loop(n1, None)
    case untpd.LangConcat(l1, l2) => translate(l1, ctx) * translate(l2, ctx)
    case untpd.LangUnion(l1, l2) => translate(l1, ctx) + translate(l2, ctx)

  def infer(expr: untpd.Expr, ctx: Ctx): (Expr, Type) = expr match
    // Literals
    case untpd.Const(null) => (NullLit()(expr.range), NullType)
    case untpd.Const(v: Int) => (IntLit(v)(expr.range), IntType)
    case untpd.Const(v: Boolean) => (BoolLit(v)(expr.range), BoolType)
    case untpd.Const(v: Char) => (CharLit(v)(expr.range), CharType)
    case untpd.Const(v: String) => (StrLit(v)(expr.range), strType)

    // Variable
    case untpd.TermName(x) =>
      ctx.lookup(x) match
        case Some(ValInfo(t)) => (Var(x)(expr.range), ctx.getNarrowedType(x).getOrElse(t))
        case Some(VarInfo(t)) => (Var(x)(expr.range), ctx.getNarrowedType(x).getOrElse(t))
        case Some(ConstInfo(sort, value)) => (value, sort)
        case Some(m: MethodInfo) => (MethodRef(x)(expr.range), m.funType)
        case Some(_) =>
          reporter.reportNotTerm(expr.range, x)
          (NoExpr, NoType)
        case None =>
          reporter.reportNameUndefined(expr.range)
          (NoExpr, NoType)

    // Equality
    case untpd.Eq(e1, e2) =>
      val (left, leftType) = infer(e1, ctx)
      val (right, rightType) = infer(e2, ctx)
      (Eq(left, right)(expr.range), BoolType)
    case untpd.Ne(e1, e2) =>
      val (left, leftType) = infer(e1, ctx)
      val (right, rightType) = infer(e2, ctx)
      (Ne(left, right)(expr.range), BoolType)

    // Boolean Operators
    case untpd.And(e1, e2) =>
      val left = check(e1, BoolType, ctx)
      val right = check(e2, BoolType, assume(left, ctx))
      (And(left, right)(expr.range), BoolType)
    case untpd.Or(e1, e2) =>
      val left = check(e1, BoolType, ctx)
      val right = check(e2, BoolType, assume(Not(left), ctx))
      (Or(left, right)(expr.range), BoolType)
    case untpd.Not(e) =>
      val cond = check(e, BoolType, ctx)
      (Not(cond)(expr.range), BoolType)
    case untpd.Implies(e1, e2) =>
      val left = check(e1, BoolType, ctx)
      val right = check(e2, BoolType, assume(left, ctx))
      (Implies(left, right)(expr.range), BoolType)
    case untpd.Ite(e, e1, e2) =>
      val cond = check(e, BoolType, ctx)
      val thenExpr = check(e1, BoolType, assume(cond, ctx))
      val elseExpr = check(e2, BoolType, assume(Not(cond), ctx))
      (Ite(cond, thenExpr, elseExpr)(expr.range), BoolType)

    // General Operators
    case untpd.Access(e, m) =>
      val (receiver, receiverType) = infer(e, ctx)
      if receiverType == NoType then
        return (NoExpr, NoType)

      builtin.accessMember(receiverType, m.name) match
        case Nil => // not found
          reporter.reportMemberNotFound(m.range, m.name, receiverType.erase)
          (NoExpr, NoType)
        case List(m) => // unique
          if m.funType.arity == 0 then
            (m.apply(receiver, Nil, expr.range), m.funType.returnType)
          else
            throw UnsupportedOperationException("lambda expression")
        case ms => // ambiguous
          reporter.reportAmbiguousOverload(expr.range, m.name, receiverType.erase, ms.map(_.funType))
          (NoExpr, NoType)

    case untpd.Apply(untpd.Access(e, m), es) =>
      val (receiver, receiverType) = infer(e, ctx)
      if receiverType == NoType then
        return (NoExpr, NoType)

      builtin.accessMember(receiverType, m.name) match
        case Nil => // not found
          reporter.reportMemberNotFound(m.range, m.name, receiverType)
          (NoExpr, NoType)
        case List(m) => // unique
          val args = checkApply(es, m.funType, expr.range, 0, ctx)
          (m.apply(receiver, args, expr.range), m.funType.returnType)
        case ms => // overloaded
          ms.filter(_.funType.arity == es.length) match
            case List(info) => // unique with matching arity
              val args = checkApply(es, info.funType, expr.range, 0, ctx)
              (info.apply(receiver, args, expr.range), info.funType.returnType)
            case candidates => // try candidates to see if any match the argument types
              val (args, argTypes) = es.map(infer(_, ctx)).unzip
              candidates.find(info => (argTypes.map(_.erase) zip info.funType.paramTypes).forall(_ :<: _)) match
                case Some(info) =>
                  (info.apply(receiver, args, expr.range), info.funType.returnType)
                case None =>
                  reporter.reportNoMatchingOverload(m.range, m.name, receiverType.erase, ms.map(_.funType), argTypes)
                  (NoExpr, NoType)

    case untpd.Apply(untpd.TermName("format"), untpd.Const(s: String) :: es) =>
      val e = checkStrFormat(s, es, expr.range, ctx)
      (e, strType)

    case untpd.Apply(e, es) =>
      val (fun, typ) = infer(e, ctx)
      typ match
        case funType: FunType =>
          val args = checkApply(es, funType, expr.range, 0, ctx)
          (Apply(fun, args)(expr.range), funType.returnType)
        case _ =>
          if typ != NoType then
            reporter.reportNotCallable(e.range, typ)
          (NoExpr, NoType)

    // Constructors
    case untpd.SeqExpr(Nil) =>
      reporter.reportMissingTypeAnnot(expr.range)
      (NoExpr, NoType)
    case untpd.SeqExpr(e :: es) =>
      val (firstElem, elemType) = infer(e, ctx)
      val elemSort = elemType.erase
      val otherElems = es.map(check(_, elemSort, ctx))
      (SeqLit(firstElem :: otherElems)(elemSort, expr.range), ListType(elemSort))
    case untpd.SetExpr(Nil) =>
      reporter.reportMissingTypeAnnot(expr.range)
      (NoExpr, NoType)
    case untpd.SetExpr(e :: es) =>
      val (firstElem, elemType) = infer(e, ctx)
      val elemSort = elemType.erase
      val otherElems = es.map(check(_, elemSort, ctx))
      (SetLit(firstElem :: otherElems)(elemSort, expr.range), SetType(elemSort))
    case untpd.MapExpr(Nil, Nil) =>
      reporter.reportMissingTypeAnnot(expr.range)
      (NoExpr, NoType)
    case untpd.MapExpr(ek :: eks, ev :: evs) =>
      val (firstKey, keyType) = infer(ek, ctx)
      val otherKeys = eks.map(check(_, keyType, ctx))
      val (firstVal, valType) = infer(ev, ctx)
      val otherVals = evs.map(check(_, valType, ctx))
      (MapLit(firstKey :: otherKeys, firstVal :: otherVals)(keyType, valType, expr.range),
        DictType(keyType, valType))

    case untpd.TupleExpr(es) =>
      val (elems, elemTypes) = es.map(infer(_, ctx)).unzip
      (TupleExpr(elems)(expr.range), TupleType(elemTypes))

    case untpd.InLang(e, l) =>
      val str = check(e, strType, ctx)
      val re = translate(l, ctx)
      (StringInLang(str, re)(expr.range), BoolType)

    case _ =>
      throw NotImplementedError(s"Type inference for ${expr.getClass.getSimpleName} is not implemented")

  def assume(cond: Expr, ctx: Ctx): Ctx = simplify(cond) match
    case Ne(Var(x), NullLit()) =>
      ctx.lookup(x) match
        case Some(ValInfo(NullableType(t))) => ctx.narrow(x, t)
        case Some(VarInfo(NullableType(t))) => ctx.narrow(x, t)
        case _ => ctx
    case And(e1, e2) =>
      val ctx1 = assume(e1, ctx)
      assume(e2, ctx1)
    case _ => ctx

  def simplify(cond: Expr): Expr = cond match
    case Not(e) =>
      simplify(e) match
        case Not(e1) => e1
        case Eq(e1, e2) => Ne(e1, e2)
        case Ne(e1, e2) => Eq(e1, e2)
        case And(e1, e2) => Or(simplify(Not(e1)), simplify(Not(e2)))
        case Or(e1, e2) => And(simplify(Not(e1)), simplify(Not(e2)))
        case e => Not(e)
    case And(e1, e2) => And(simplify(e1), simplify(e2))
    case Or(e1, e2) => Or(simplify(e1), simplify(e2))
    case _ => cond

  private def checkApply(argNodes: List[untpd.Expr], funType: FunType, range: Range, x: Int,
                         ctx: Ctx): List[Expr] =
    if argNodes.length < funType.arity then
      val lastRange = Range(Position(range.getEnd.getLine, range.getEnd.getCharacter - 1), range.getEnd)
      reporter.reportMissingArgs(lastRange, funType, argNodes.length)
    else if argNodes.length > funType.arity then
      reporter.reportTooManyArgs(range, funType)
    argNodes.zip(funType.paramTypes).map(check(_, _, ctx))

  private def checkStrFormat(fmt: String, args: List[untpd.Expr], range: Range, ctx: Ctx): Expr =
    val parts = ListBuffer.empty[Expr]
    var i = 0
    var k = 0
    while i < fmt.length do
      if fmt.drop(i).startsWith("%%") then
        parts += StrLit("%")
        i += 2
      else if fmt.drop(i).startsWith("%s") then
        parts += check(args(k), strType, ctx)
        k += 1
        i += 2
      else if fmt(i) == '%' then
        parseFormatter(fmt.drop(i + 1), range) match
          case Some((f, n)) if k < args.length =>
            val e = check(args(k), IntType, ctx)
            parts += StrFromInt(e, f)
            i += 1 + n
            k += 1
          case _ =>
            i = fmt.length
      else
        val s = fmt.drop(i).takeWhile(_ != '%')
        parts += StrLit(s)
        i += s.length

    if parts.isEmpty then StrLit("") else parts.reduce(SeqConcat(_, _)(range))

  private def parseFormatter(f: String, range: Range): Option[(NumStrFormat, Int)] =
    // flag: 0 for zero-padded
    val zeroPadded = f.startsWith("0")
    var i = 0
    if zeroPadded then
      i += 1
    // width
    val width = f.drop(i).takeWhile(_.isDigit)
    i += width.length
    if zeroPadded && width.isEmpty then
      reporter.reportSyntaxError("no width specified for zero-padded format", range)
      return None
    // conversion
    if !Set('d', 'o', 'x', 'X').contains(f(i)) then
      reporter.reportSyntaxError(s"invalid conversion specifier '${f(i)}'", range)
      return None
    val conv = f(i)
    val formatter = NumStrFormat(zeroPadded = zeroPadded, width = if width.isEmpty then 0 else width.toInt,
      conv = conv)
    Some(formatter, i + 1)

  def check(node: untpd.Expr, expectedType: Type, ctx: Ctx): Expr =
    (node, expectedType) match
      case (untpd.Ite(e, e1, e2), _) =>
        val cond = check(e, BoolType, ctx)
        val thenExpr = check(e1, expectedType, assume(cond, ctx))
        val elseExpr = check(e2, expectedType, assume(Not(cond), ctx))
        Ite(cond, thenExpr, elseExpr)(node.range)

      case (untpd.SeqExpr(es), ListType(t)) =>
        val elems = es.map(check(_, t, ctx))
        SeqLit(elems)(t, node.range)
      case (untpd.SetExpr(es), SetType(t)) =>
        val elems = es.map(check(_, t, ctx))
        SetLit(elems)(t, node.range)
      case (untpd.MapExpr(eks, evs), DictType(tk, tv)) =>
        val keys = eks.map(check(_, tk, ctx))
        val values = evs.map(check(_, tv, ctx))
        MapLit(keys, values)(tk, tv, node.range)

      case (untpd.TupleExpr(es), TupleType(ts)) if es.length == ts.length =>
        val elems = es.zip(ts).map((e, t) => check(e, t, ctx))
        TupleExpr(elems)(node.range)

      case (_, expected) =>
        val (expr, actual) = infer(node, ctx)
        if isSubtype(actual, expected) then
          return expr

        if !isSubtype(actual, expected.erase) then
          reporter.reportTypeMismatch(node.range, expected, actual)
          return expr

        expected match
          case RefinedType(_, reft) =>
            body += GAssert(reft.subst(Map("_" -> expr)), ReftNotProvedError(node.range))
          case _ =>
            throw NotImplementedError(s"Type checking for $node: ${actual.show} <: ${expected.show} is not implemented")
        expr

  private def isSubtype(left: Type, right: Type): Boolean =
    if left == right || left == NoType || right == NoType then true
    else (left, right) match
      case (ListType(t1), ListType(t2)) => isSubtype(t1, t2)
      case (SetType(t1), SetType(t2)) => isSubtype(t1, t2)
      case (DictType(k1, v1), DictType(k2, v2)) => isSubtype(k1, k2) && isSubtype(k2, k1) && isSubtype(v1, v2)
      case (RefinedType(t1, _), t2) => isSubtype(t1, t2)
      case (TupleType(ts1), TupleType(ts2)) => ts1.length == ts2.length && (ts1 zip ts2).forall(isSubtype)
      case (FunType(ps1, r1), FunType(ps2, r2)) =>
        ps1.length == ps2.length && (ps2 zip ps1).forall(isSubtype) && isSubtype(r1, r2)
      case (NullableType(t1), NullableType(t2)) => isSubtype(t1, t2)
      case (NullType, NullableType(_)) => true
      case (t1, NullableType(t2)) => isSubtype(t1, t2)
      case _ => false

  def inferParamList(nodes: List[untpd.Param], ctx: Ctx): List[(untpd.Ident, Type)] =
    val scope = mutable.Map.empty[String, Range]
    for node <- nodes yield
      val typ = normalize(node.typ, ctx)
      if scope.contains(node.ident.name) then
        reporter.reportNameRedefined(node.ident.range, scope(node.ident.name))
      else
        scope(node.ident.name) = node.ident.range
      (node.ident, typ)
