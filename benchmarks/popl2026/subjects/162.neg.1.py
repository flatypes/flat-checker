type Input = lang(r'(a|[^a].).*')

def f162(s: Input):
  assert len(s) <= 1
  assert s.find("a") == -1
