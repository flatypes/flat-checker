type Input = lang(r'ab')

def f213(s: Input):
  a, b = s
  if a != "a":
    raise Exception
  if b != "b":
    raise Exception
