type Input = lang(r'.?')

def f021(s: Input) -> Char:
  if len(s) == 1:
    return s[0]
  elif len(s) == 0:
    return 'a'
  else:
    raise Exception
