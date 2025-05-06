package flat.checker.py

import flat.checker.{Sort, core}

extension (sort: Sort)
  def show: String = sort match
    case Sort.Top => "Any"
    case Sort.Bot => "?"
    case Sort.Int => "int"
    case Sort.Bool => "bool"
    case Sort.String => "str"
    case Sort.Tuple(ss) => s"tuple[${ss.map(_.show).mkString(", ")}]"
    case Sort.Array(s) => s"list[${s.show}]"
    case Sort.Fun(ss, s) => s"Callable[[${ss.map(_.show).mkString(", ")}], ${s.show}]"

extension (typ: core.Type)
  def show: String = typ.toSort.show