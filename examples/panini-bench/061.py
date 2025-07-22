type Input = lang(r'...')

def f061(s: Input) -> str:
  assert len(s) <= 3
  c0 = s[0]
  c1 = s[1]
  c2 = s[2]
  return c0 + c1 + c2
