type Input = lang(r'[^a]..*|(a[^a]*)*')

def f154(s: Input):
  if s[0] == "a":
    raise Exception
  else:
    assert len(s) == 1
