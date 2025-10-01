type Input = lang(r'a(.(b..*|([^b]b*)*))?|([^a]a*)*')


def f323(s: Input):
  a, x = s[0:2]
  y, b = s[1:]
  assert a == "a"
  assert b == "b"
