type Input = lang(r'a..*|([^a]a*)*')


def f081(s: Input):
  assert s[0] == "a"
  assert len(s) == 1
