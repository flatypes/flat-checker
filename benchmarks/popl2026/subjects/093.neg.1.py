type Input = lang(r'([^a]|a.).*')

def f093(s: Input):
  if len(s) == 0:
    return
  elif s == "a":
    return
  else:
    raise Exception
