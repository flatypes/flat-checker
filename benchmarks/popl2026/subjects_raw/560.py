
def f560(s: Input):
  bi = s.index("b")
  if bi == 1:
    assert s[0] == "a"
  else:
    assert bi == 0
