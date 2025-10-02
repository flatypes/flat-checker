type Input = lang(r'a?([^a]a*)*')


def f524(s: Input):
  a1, a2 = s[0:2]
  assert a1 == "a"
  assert a2 == "a"
