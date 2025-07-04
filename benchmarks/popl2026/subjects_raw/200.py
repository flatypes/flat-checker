
def f200(s: Input):
  i = 0
  while i < len(s)-1:
    assert s[i] == "a"
    i += 1
  assert s[i] != "a"
