type Input = lang(r'a')

def f083(s: Input):
  if s[0] == "a":
    assert len(s) == 1
  else:
    raise Exception
