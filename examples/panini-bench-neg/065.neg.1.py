type Input = lang(r'(.(.(...*)?)?)?')


def f065(s: Input):
  i = len(s) - 1
  assert i == 2
  while i >= 0:
    c = s[i]
    i -= 1
