type Input = lang(r'a(b..*|([^b]b*)*)|([^a]a*)*')


def f212(s: Input):
  assert s.index("a") == 0
  assert s.index("b") == 1
  assert len(s) == 2
