type Input = lang(r'.*a.*')

def f143(s: Input):
  i = len(s)
  while i > 0:
    if s[i-1] == "a":
      return
    i -= 1
  raise Exception
