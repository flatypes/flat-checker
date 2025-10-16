package flat.checker.py

import flat.checker.{Sort, ast}

extension (sort: Sort)
  def show: String = sort match
    case Sort.Top => "Any"
    case Sort.Bot => "?"
    case Sort.I => "int"
    case Sort.B => "bool"
    case Sort.S => "str"
    case Sort.Tuple(ss) => s"tuple[${ss.map(_.show).mkString(", ")}]"
    case Sort.Array(s) => s"list[${s.show}]"
    case Sort.Fun(ss, s) => s"Callable[[${ss.map(_.show).mkString(", ")}], ${s.show}]"

extension (typ: ast.Type)
  def show: String = typ.toSort.show