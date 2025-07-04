type Input = lang(r'([^a]a*)*')

def f126(s: Input):
  i = len(s)
  while i > 0:
    i -= 1
  assert s[i] == "a"
