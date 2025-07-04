type Input = lang(r'[ab]..*|([^ab][ab]*)*')

def f372(s: Input) -> bool:
  if s == "a":
    return True
  elif s == "b":
    return False
  else:
    raise Exception
