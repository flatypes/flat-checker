type Input = lang(r'([^a]a*)*')


def f125(s: Input):
  if s == "":
    raise Exception
  else:
    assert s[0] == "a"
