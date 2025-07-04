type Input = lang(r'a..*|([^a]a*)*')

def f084(s: Input):
  t = s[0:1]
  assert t == "a"
  assert len(s) == 1
