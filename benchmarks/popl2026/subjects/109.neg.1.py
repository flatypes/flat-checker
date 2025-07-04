type Input = lang(r'a(a..*|([^a]a*)*)|([^a]a*)*')

def f109(s: Input):
  a,b = s
  t = a + b
  assert t == "aa"
