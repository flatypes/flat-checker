type Input = lang(r'[^a]..*|(a[^a]*)*')

def f152(s: Input):
  assert len(s) == 1
  assert s.find("a") == -1
