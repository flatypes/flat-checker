type Input = lang(r'[^a](a..*|([^a]a*)*)|(a[^a]*)*')


def f182(s: Input):
  if s[0] == "a":
    raise Exception
  elif s[1] == "a":
    assert len(s) == 2
  else:
    raise Exception
