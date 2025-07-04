type Input = lang(r'a([^b]b*)*|([^a]a*)*')

def f300(s: Input):
  assert s[0] == "a"
  assert s[1] == "b"
