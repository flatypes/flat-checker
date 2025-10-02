type Input = lang(r'(a|[^a]..*)*')


def f202(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s))
    if s[i] != "a":
      break
    i += 1
  assert i == len(s) - 1
