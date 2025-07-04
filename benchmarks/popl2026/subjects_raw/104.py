
def f104(s: Input):
  i = len(s)
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
  assert len(s) == 2
