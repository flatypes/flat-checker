type Input = lang(r'[^a]a')

def f181(s: Input):
  x,y = s
  assert x != "a"
  assert y == "a"
