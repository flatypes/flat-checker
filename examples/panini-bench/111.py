type Input = lang(r'a*')

def f111(s: Input):
  i = len(s)
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
