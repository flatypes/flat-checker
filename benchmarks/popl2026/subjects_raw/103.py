
def f103(s: Input):
  i = 0
  j = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(j == i)
    assert s[i] == "a"
    i += 1
    j += 1
  assert j == 2
