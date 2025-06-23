type Input = lang(r'aa')

def f102(s: Input):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    i += 1
  assert i == 2
