type Input = lang(r'a..*|([^a]a*)*')


def f082(s: Input):
  if s[0] == "a":
    assert len(s) == 1
  else:
    assert False
