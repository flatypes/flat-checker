type Input = lang(r'[a-c]..*|([^a-c][a-c]*)*')


def f422(s: Input):
  c = s[0]
  if c != "a":
    if c != "b":
      if c != "c":
        raise Exception
  assert len(s) == 1
