type Input = lang(r'a.*b')

def f363(s: Input):
  assert s.index("a") == 0
  assert s.index("b", len(s)-1) == len(s)-1
