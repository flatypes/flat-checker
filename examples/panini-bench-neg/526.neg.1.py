type Input = lang(r'a?([^a]a*)*')


def f526(s: Input):
  a1, a2 = s[0:2]
  assert a1 == a2
  assert a1 == "a"
