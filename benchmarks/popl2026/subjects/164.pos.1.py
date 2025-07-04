type Input = lang(r'[^a]?')

def f164(s: Input):
  if s == "":
    return
  elif s[0] == "a":
    raise Exception
  else:
    assert len(s) == 1
