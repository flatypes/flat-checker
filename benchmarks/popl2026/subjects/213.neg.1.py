type Input = lang(r'a(b..*|([^b]b*)*)|([^a]a*)*')

def f213(s: Input):
  a, b = s
  if a != "a":
    raise Exception
  if b != "b":
    raise Exception
