type Input = lang(r'([^ab]|[ab].).*')


def f392(s: Input):
  assert len(s) == 0 or (len(s) == 1 and (s.find("a") >= 0 or s.find("b") >= 0))
