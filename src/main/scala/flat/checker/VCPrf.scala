package flat.checker

import com.typesafe.scalalogging.LazyLogging
import flat.Issuer
import flat.checker.core.{Expr, TypeTest}

import scala.collection.mutable.ListBuffer

trait VCPrf(path: os.Path)(using types: Types) extends LazyLogging:

  import Formula.*

  val issuer = new Issuer

  private val records = ListBuffer.empty[ujson.Obj]

  def prove(goal: Formula)(using ctx: PrfCtx = PrfCtx.empty): Unit = goal match
    case True =>
    case g: HasType => proveHasType(g)
    case g: Goal => proveSubGoal(g)
    case LAnd(goal1, goal2) => prove(goal1); prove(goal2)
    case LImp(e, goal) => prove(goal)(using ctx + e)

  def proveHasType(goal: HasType)(using ctx: PrfCtx): Unit =
    val HasType(e, t, _) = goal
    logger.info("")
    logger.info(s"Goal: $ctx ⇒ $e : $t")
    val (elapsed, result) = flat.util.time(process(TypeTest(e, t)))
    result match
      case Right(setting) =>
        logger.info(s"Proved $setting (time: $elapsed ms)")
        records += ujson.Obj(
          "goal" -> ujson.Num(records.size + 1),
          "success" -> ujson.Bool(true),
          "time (ms)" -> ujson.Num(elapsed),
          "use SMT" -> ujson.Bool(setting.withSMT),
          "use lemmas" -> ujson.Bool(setting.withHints),
        )
      case Left(actual) =>
        issuer.report(TypeMayMismatch(t.toString, actual, e.loc))
        logger.info(s"Failed (time: $elapsed ms)")
        records += ujson.Obj(
          "goal" -> ujson.Num(records.size + 1),
          "success" -> ujson.Bool(false),
          "time (ms)" -> ujson.Num(elapsed),
        )

  def proveSubGoal(goal: Goal)(using ctx: PrfCtx): Unit =
    val Goal(e, err) = goal
    logger.info("")
    logger.info(s"Goal: $ctx ⇒ $e")
    val (elapsed, result) = flat.util.time(process(e))
    result match
      case Right(setting) =>
        logger.info(s"Proved $setting (time: $elapsed ms)")
        records += ujson.Obj(
          "goal" -> ujson.Num(records.size + 1),
          "success" -> ujson.Bool(true),
          "time (ms)" -> ujson.Num(elapsed),
          "use SMT" -> ujson.Bool(setting.withSMT),
          "use lemmas" -> ujson.Bool(setting.withHints),
        )
      case Left(_) =>
        issuer.report(err)
        logger.info(s"Failed (time: $elapsed ms)")
        records += ujson.Obj(
          "goal" -> ujson.Num(records.size + 1),
          "success" -> ujson.Bool(false),
          "time (ms)" -> ujson.Num(elapsed),
        )

  final case class Setting(withSMT: Boolean = false, withHints: Boolean = false):
    def |(that: Setting): Setting = Setting(withSMT || that.withSMT, withHints || that.withHints)

    override def toString: String =
      (withSMT, withHints) match
        case (true, true) => "with SMT + lemmas"
        case (true, false) => "with SMT"
        case (false, true) => "with lemmas"
        case (false, false) => "trivial"

  protected def process(conclusion: Expr)(using ctx: PrfCtx): Either[String, Setting]

  def getRecord: ujson.Obj = ujson.Obj(
    "file" -> ujson.Str(path.toString),
    "success" -> ujson.Bool(records.forall(_("success").bool)),
    "goals" -> ujson.Arr.from(records)
  )
