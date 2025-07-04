type Input = lang(r'a([^b]b*)*|([^a]a*)*')

def f301(s: Input):
  assert s[0:2] == "ab"
