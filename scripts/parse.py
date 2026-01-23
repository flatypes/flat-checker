import ast
import json
import sys
from typing import Any


class Completer(ast.NodeVisitor):
  """Complete the positions of function identifiers."""

  def __init__(self, code: str) -> None:
    super().__init__()
    self.lines = code.splitlines()
    self.tree = ast.parse(code)
    self.visit(self.tree)

  def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
    name_col_offset = self.lines[node.lineno - 1].index(node.name, 3)
    setattr(node, 'name_col_offset', name_col_offset)
    for stmt in node.body:
      self.visit(stmt)


def serialize_node(node: ast.AST) -> dict:
  obj = {}
  obj['_constr'] = node.__class__.__name__
  for attr in dir(node):
    if not attr.startswith('_'):
      obj[attr] = serialize_value(getattr(node, attr))
  return obj


def serialize_value(value: Any) -> Any:
  match value:
    case ast.AST():
      return serialize_node(value)
    case list():
      return [serialize_value(v) for v in value]
    case _:
      return value


if len(sys.argv) < 2:
  print('No input file', file=sys.stderr)
  sys.exit(1)
with open(sys.argv[1], 'r') as f:
  code = f.read()
  completer = Completer(code)
  json_value = serialize_value(completer.tree.body)
  print(json.dumps(json_value, indent=2))
