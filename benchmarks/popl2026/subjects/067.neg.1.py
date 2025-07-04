type Input = lang(r'(.(.(...*)?)?)?')

def f067(s: Input) -> Input:
  assert len(s) <= 3
  return s[0:3]
