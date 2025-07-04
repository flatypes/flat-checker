type Input = lang(r'[^a]*a.*')

def f172(s: Input):
  i = 0
  while i < len(s):
    if s[i] == "a":
      raise Exception
    i += 1
