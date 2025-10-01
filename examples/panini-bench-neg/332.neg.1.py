type Input = lang(r'[^a].*|a(.(b..*|([^b]b*)*))?')


def f332(s: Input):
  if len(s) > 0:
    a, x, b = s
    assert a == "a"
    assert b == "b"
