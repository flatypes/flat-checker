type Input = lang(r'(.(...*)?)?')


def f043(s: Input):
  n = len(s) - 2
  c1 = s[n]
  c2 = s[n + 1]
  assert len(s) == 2
