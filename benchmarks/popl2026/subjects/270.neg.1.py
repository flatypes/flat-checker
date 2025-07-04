type Input = lang(r'a([^b]|b.).*|([^a]a*)*')

def f270(s: Input):
  assert s == "a" or s == "ab"
