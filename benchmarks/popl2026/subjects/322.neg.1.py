type Input = lang(r'a(.(b..*|([^b]b*)*))?|([^a]a*)*')

def f322(s: Input):
  a,x,b = s
  assert a == "a"
  assert b == "b"
