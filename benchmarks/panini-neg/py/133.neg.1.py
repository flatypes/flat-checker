type Input = lang(r'(.*[^a])?')


def f133(s: Input):
  i = 0
  while i < len(s) - 1:
    i += 1
  assert s[i] == "a"
