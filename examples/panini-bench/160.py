type Input = lang(r'[^a]?')

def f160(s: Input):
  if len(s) == 1:
    assert s[0] != "a"
  else:
    assert len(s) == 0
