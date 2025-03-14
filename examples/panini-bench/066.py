type Input = lang(r'...')

def f066(s: Input):
  if len(s) > 3:
    raise Exception
  else:
    return s[0:3]
