type Input = lang(r'a?([^a]a*)*')

def f520(s: Input):
  assert s[0] == "a"
  assert s[1] == "a"
