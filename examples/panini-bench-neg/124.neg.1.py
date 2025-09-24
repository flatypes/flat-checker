type Input = lang(r'([^a]a*)*')


def f124(s: Input):
  i = 0
  assert s[i] == "a"
  while i < len(s):
    c = s[i]
    i += 1
