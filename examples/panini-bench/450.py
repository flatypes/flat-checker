type Input = lang(r'(a[^ab]b)*')

def f450(s: Input):
  i = 0
  while i < len(s):
    assert s[i+0] == "a"
    assert s[i+1] != "a"
    assert s[i+1] != "b"
    assert s[i+2] == "b"
    i += 1
