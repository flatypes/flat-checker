type Input = lang(r'ab*[^b].*|([^a]a*)*')

def f230(s: Input):
  assert s[0] == "a"
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 1
