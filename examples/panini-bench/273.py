type Input = lang(r'ab?')

def f273(s: Input):
  c = s[len(s)-1]
  if c == "b":
    assert len(s) == 2
    assert s[0] == "a"
  else:
    assert c == "a"
    assert len(s) == 1
