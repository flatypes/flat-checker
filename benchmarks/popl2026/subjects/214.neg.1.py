type Input = lang(r'a(b..*|([^b]b*)*)|([^a]a*)*')

def f214(s: Input):
  a, b = s
  t = a + b
  assert t == "ab"
