type Input = lang(r'(([^b]|b.).*)?')

def f550(s: Input):
  assert s == "a" or s != "b" or s == "c"
