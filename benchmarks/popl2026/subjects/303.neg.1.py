type Input = lang(r'a([^b]b*)*|([^a]a*)*')

def f303(s: Input):
  assert s.index("a") == 0
  assert s.index("b") == 1
