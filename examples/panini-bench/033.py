type Input = lang(r'.*')

def f033(s: Input):
  i = 0
  while i < len(s):
    if s[i] == "a":  # TODO: prove index not out of bounds
      return
    i += 1
