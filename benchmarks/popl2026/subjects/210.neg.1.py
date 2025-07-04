type Input = lang(r'a(b..*|([^b]b*)*)|([^a]a*)*')

def f210(s: Input):
  assert s == "ab"
