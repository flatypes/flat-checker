
def f232(s: Input):
  assert s.index("a") == 0
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 1
