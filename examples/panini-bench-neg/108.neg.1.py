type Input = lang(r'a(a..*|([^a]a*)*)|([^a]a*)*')


def f108(s: Input):
  a, b = s
  assert a == "a"
  assert b == "a"
