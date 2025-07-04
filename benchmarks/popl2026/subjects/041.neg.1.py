type Input = lang(r'(.(...*)?)?')

def f041(s: Input) -> Input:
  assert len(s) <= 2
  return s[0] + s[1]
