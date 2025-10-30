package flat.checker.py

import flat.checker.ast.*

extension (typ: Type)
  def show: String = typ match
    case TopType => "Any"
    case NoType => "?"
    case IntSort => "int"
    case BoolSort => "bool"
    case StrSort => "str"
    case TupleSort(ss) => s"tuple[${ss.map(_.show).mkString(", ")}]"
    case ArraySort(s) => s"list[${s.show}]"
    case FunSort(ss, s) => s"Callable[[${ss.map(_.show).mkString(", ")}], ${s.show}]"
    case _ => typ.base.show