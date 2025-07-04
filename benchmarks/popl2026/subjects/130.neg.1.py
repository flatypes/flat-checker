type Input = lang(r'(.*[^a])?')

def f130(s: Input):
  assert s[len(s)-1] == "a"
