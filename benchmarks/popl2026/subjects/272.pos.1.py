type Input = lang(r'ab?')

def f272(s: Input):
  bi = s.find("b")
  if bi == 1:
    assert s[0] == "a"
    assert len(s) == 2
  else:
    assert s == "a"
