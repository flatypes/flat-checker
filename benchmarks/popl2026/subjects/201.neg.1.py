type Input = lang(r'(a|[^a]..*)*')

def f201(s: Input):
  assert s[len(s)-1] != "a"
  i = 0
  while i < len(s)-1:
    assert s[i] == "a"
    i += 1
