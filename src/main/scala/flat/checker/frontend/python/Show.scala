package flat.checker.frontend.python

import flat.checker.{Sort, ast}

extension (sort: Sort)
  def show: String = sort match
    case Sort.Top => "Any"
    case Sort.Bot => "?"
    case Sort.Int => "int"
    case Sort.Bool => "bool"
    case Sort.Char => "char"
    case Sort.String => "str"
    case Sort.Array(s) => s"list[${s.show}]"
    case Sort.Fun(ss, s) => s"Callable[[${ss.map(_.show).mkString(", ")}], ${s.show}]"

extension (typ: ast.Type)
  def show: String = typ.toSort.show