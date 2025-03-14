type Input = lang(r'.*')

def f032(s: Input) -> Char:
  if len(s) == 0:
    return 'a'
  else:
    return s[0]
