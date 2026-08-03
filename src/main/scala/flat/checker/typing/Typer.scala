package flat.checker.typing

import flat.checker.Reporter
import flat.checker.domain.{RegEx, StrRE, given}
import flat.checker.flan.*
import flat.checker.flan.SortOps.*
import flat.checker.flan.tpd.*
import org.eclipse.lsp4j.{Position, Range}

import scala.collection.mutable

class Typer(using reporter: Reporter):
  def normalize(node: untpd.Type)(using ctx: Ctx): NormType = node match
    case untpd.NullType => NullSort
    case untpd.BoolType => BoolSort
    case untpd.IntType => IntSort
    case untpd.CharType => CharSort
    case t@untpd.TypeName(x) =>
      ctx.lookup(x) match
        case Some(TypeInfo(typ)) => typ
        case Some(LangInfo(r)) => NormType(stringSort, Some(StringInLang(Var("_"), r)))
        case Some(_) =>
          reporter.reportNotType(t.range, x)
          NoSort
        case None =>
          reporter.reportNameUndefined(t.range)
          NoSort

    // Collection types
    case untpd.SeqType(t) =>
      val typ = normalize(t)
      val sort = SeqSort(typ.sort)
      NormType(sort, typ.reft.map(p => mkLambda(sort, SeqForall(_, p)()))) // _.forall(p)
    case untpd.SetType(t) =>
      val typ = normalize(t)
      val sort = SetSort(typ.sort)
      NormType(sort, typ.reft.map(p => mkLambda(sort, SetForall(_, p)()))) // _.forall(p)
    case untpd.MapType(tk, tv) =>
      val keyType = normalize(tk)
      val valueType = normalize(tv)
      val sort = MapSort(keyType.sort, valueType.sort)
      val pairPred = extractTuplePred(mkTupleSort(keyType.sort, valueType.sort), List(keyType.reft, valueType.reft))
      NormType(sort, pairPred.map(p => mkLambda(sort, m => SetForall(MapItems(m)(), p)()))) // _.items.forall(p)

    // Compound types
    case untpd.TupleType(ts) =>
      val elemTypes = ts.map(normalize)
      val sort = TupleSort(elemTypes.map(_.sort))
      NormType(sort, extractTuplePred(sort, elemTypes.map(_.reft)))
    case untpd.FunType(ts, t) =>
      val paramTypes = ts.map(normalize)
      val returnType = normalize(t)
      if paramTypes.exists(_.reft.isDefined) || returnType.reft.isDefined then
        throw IllegalArgumentException("Function refinement types are not supported")
      FunSort(paramTypes.map(_.sort), returnType.sort)
    case untpd.UnionType(t1, t2) =>
      val leftType = normalize(t1)
      val rightType = normalize(t2)
      if leftType.reft.isDefined || rightType.reft.isDefined then
        throw IllegalArgumentException("Union refinement types are not supported")
      UnionSort(leftType.sort, rightType.sort)
    case _ => throw UnsupportedOperationException(s"normalize ${node.getClass.getSimpleName}")

  private def extractTuplePred(sort: TupleSort, preds: List[Option[Expr]]): Option[Expr] =
    val conjuncts = preds.zipWithIndex.collect { case (Some(p), i) => (p, i) }
    if conjuncts.isEmpty then None
    else Some(mkLambda(sort, tup => mkAnd(conjuncts.map { (p, i) => mkApply(p, TupleSelect(i, tup)()) })))

  def translate(node: untpd.Lang)(using ctx: Ctx): StrRE = node match
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
    case untpd.LangStar(l) => translate(l).star
    case untpd.LangPlus(l) => translate(l).plus
    case untpd.LangOpt(l) => translate(l).opt
    case untpd.LangPower(l, n) => translate(l) ^ n
    case untpd.LangLoop(l, n1, Some(n2)) => ???
    case untpd.LangLoop(l, n1, None) => ???
    case untpd.LangConcat(l1, l2) => translate(l1) * translate(l2)
    case untpd.LangUnion(l1, l2) => translate(l1) + translate(l2)

  def infer(node: untpd.Expr)(using ctx: Ctx, vs: VarStore): (Sort, Expr) = node match
    case untpd.Const(null) => (NullSort, Const(null)(node.range))
    case untpd.Const(v: Boolean) => (BoolSort, Const(v)(node.range))
    case untpd.Const(v: Int) => (IntSort, Const(v)(node.range))
    case untpd.Const(v: Char) => (CharSort, Const(v)(node.range))
    case untpd.Const(v: String) => (stringSort, Const(v)(node.range))

    case untpd.TermName(x) =>
      ctx.lookup(x) match
        case Some(VarInfo(typ, index)) => (typ.sort, Var(vs.getName(index))(node.range))
        case Some(ConstInfo(sort, value)) => (sort, value)
        case Some(m: MethodInfo) => (m.funSort, MethodRef(x)(node.range))
        case Some(_) =>
          reporter.reportNotTerm(node.range, x)
          (NoSort, Var(x)(node.range))
        case None =>
          reporter.reportNameUndefined(node.range)
          (NoSort, Var(x)(node.range))

    case untpd.SeqExpr(es) =>
      val (argSorts, args) = es.map(infer).unzip
      if argSorts.isEmpty then
        reporter.reportMissingTypeAnnot(node.range)
        (SeqSort(NoSort), SeqLit(args)(NoSort, node.range))
      else
        val elemSort = argSorts.reduce(_ lub _)
        (SeqSort(elemSort), SeqLit(args)(elemSort, node.range))

    case untpd.SetExpr(es) =>
      val (argSorts, args) = es.map(infer).unzip
      if argSorts.isEmpty then
        reporter.reportMissingTypeAnnot(node.range)
        (SetSort(NoSort), SetLit(args)(NoSort, node.range))
      else
        val elemSort = argSorts.reduce(_ lub _)
        (SetSort(elemSort), SetLit(args)(elemSort, node.range))

    case untpd.MapExpr(eks, evs) =>
      val (keySorts, keys) = eks.map(infer).unzip
      val (valSorts, vals) = evs.map(infer).unzip
      if keySorts.isEmpty then
        reporter.reportMissingTypeAnnot(node.range)
        (MapSort(NoSort, NoSort), MapLit(keys, vals)(NoSort, NoSort, node.range))
      else
        val keySort = keySorts.reduce(_ lub _)
        val valSort = valSorts.reduce(_ lub _)
        (MapSort(keySort, valSort), MapLit(keys, vals)(keySort, valSort, node.range))

    case untpd.TupleExpr(es) =>
      val (elemSorts, elems) = es.map(infer).unzip
      (TupleSort(elemSorts), TupleExpr(elems)(node.range))

    case untpd.Eq(e1, e2) =>
      val (leftType, left) = infer(e1)
      val (rightType, right) = infer(e2)
      if leftType :<: rightType || rightType :<: leftType then
        (BoolSort, Eq(left, right)(node.range))
      else
        reporter.reportTypesUnrelated(node.range, leftType, rightType)
        (BoolSort, Const(false)(node.range))

    case untpd.Ne(e1, e2) =>
      val (leftType, left) = infer(e1)
      val (rightType, right) = infer(e2)
      if leftType :<: rightType || rightType :<: leftType then
        (BoolSort, Ne(left, right)(node.range))
      else
        reporter.reportTypesUnrelated(node.range, leftType, rightType)
        (BoolSort, Const(true)(node.range))

    case untpd.Ite(e, e1, e2) =>
      val cond = check(e, BoolSort)
      val (thenType, thenValue) = infer(e1)
      val (elseType, elseValue) = infer(e2)
      (thenType lub elseType, Ite(cond, thenValue, elseValue)(node.range))

    case untpd.Access(e, m) =>
      val (receiverSort, receiver) = infer(e)
      if receiverSort == NoSort then
        return (NoSort, NoExpr)

      builtin.accessMember(receiverSort, m.name) match
        case Nil => // not found
          reporter.reportMemberNotFound(m.range, m.name, receiverSort)
          (NoSort, NoExpr)
        case List(m) => // unique
          if m.funSort.arity == 0 then
            (m.funSort.returnSort, m.apply(receiver, Nil, node.range))
          else
            throw UnsupportedOperationException("lambda expression")
        case ms => // ambiguous
          reporter.reportAmbiguousOverload(node.range, m.name, receiverSort, ms.map(_.funSort))
          (NoSort, NoExpr)

    case untpd.Apply(untpd.Access(e, m), es) =>
      val (receiverSort, receiver) = infer(e)
      if receiverSort == NoSort then
        return (NoSort, NoExpr)

      builtin.accessMember(receiverSort, m.name) match
        case Nil => // not found
          reporter.reportMemberNotFound(m.range, m.name, receiverSort)
          (NoSort, NoExpr)
        case List(m) => // unique
          val args = checkApply(es, m.funSort, node.range, 0)
          (m.funSort.returnSort, m.apply(receiver, args, node.range))
        case ms => // overloaded
          ms.filter(_.funSort.arity == es.length) match
            case List(info) => // unique with matching arity
              val args = checkApply(es, info.funSort, node.range, 0)
              (info.funSort.returnSort, info.apply(receiver, args, node.range))
            case candidates => // try candidates to see if any match the argument types
              val (argSorts, args) = es.map(infer).unzip
              candidates.find(info => argSorts.zip(info.funSort.argSorts).forall(_ :<: _)) match
                case Some(info) =>
                  (info.funSort.returnSort, info.apply(receiver, args, node.range))
                case None =>
                  reporter.reportNoMatchingOverload(m.range, m.name, receiverSort, ms.map(_.funSort), argSorts)
                  (NoSort, NoExpr)

    case untpd.Apply(e, es) =>
      val (sort, fun) = infer(e)
      sort match
        case funSort: FunSort =>
          val args = checkApply(es, funSort, node.range, 0)
          (funSort.returnSort, Apply(fun, args)(node.range))
        case _ =>
          if sort != NoSort then
            reporter.reportNotCallable(e.range, sort)
          (NoSort, NoExpr)

    case _ =>
      throw NotImplementedError(s"Type inference for ${node.getClass.getSimpleName} is not implemented")

  private def checkApply(argNodes: List[untpd.Expr], funSort: FunSort, range: Range, x: Int)
                        (using ctx: Ctx, vs: VarStore): List[Expr] =
    if argNodes.length < funSort.arity then
      val lastRange = Range(Position(range.getEnd.getLine, range.getEnd.getCharacter - 1), range.getEnd)
      reporter.reportMissingArgs(lastRange, funSort, argNodes.length)
    else if argNodes.length > funSort.arity then
      reporter.reportTooManyArgs(range, funSort)
    argNodes.zip(funSort.argSorts).map(check)

  def check(node: untpd.Expr, expected: Sort)(using ctx: Ctx, vs: VarStore): Expr =
    (node, expected) match
      case (untpd.TupleExpr(es), TupleSort(ts)) if es.length == ts.length =>
        val elems = es.zip(ts).map(check)
        TupleExpr(elems)(node.range)

      case (untpd.SeqExpr(es), SeqSort(s)) =>
        val elems = es.map(check(_, s))
        SeqLit(elems)(s, node.range)

      case (untpd.SetExpr(es), SetSort(s)) =>
        val elems = es.map(check(_, s))
        SetLit(elems)(s, node.range)

      case (untpd.MapExpr(eks, evs), MapSort(sk, sv)) =>
        val keys = eks.map(check(_, sk))
        val values = evs.map(check(_, sv))
        MapLit(keys, values)(sk, sv, node.range)

      case (untpd.Ite(e, e1, e2), _) =>
        val cond = check(e, BoolSort)
        val thenValue = check(e1, expected)
        val elseValue = check(e2, expected)
        Ite(cond, thenValue, elseValue)(node.range)

      case _ =>
        val (actual, expr) = infer(node)
        if !(actual :<: expected) then
          reporter.reportTypeMismatch(node.range, expected, actual)
        expr

  def inferParamList(nodes: List[untpd.Param])(using ctx: Ctx): List[VarDecl] =
    val scope = mutable.Map.empty[String, Range]
    for node <- nodes yield
      val typ = normalize(node.typ)
      if scope.contains(node.ident.name) then
        reporter.reportNameRedefined(node.ident.range, scope(node.ident.name))
      else
        scope(node.ident.name) = node.ident.range
      VarDecl(node.ident.name, typ)
