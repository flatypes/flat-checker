type Input = lang(r'(.(.(...*)?)?)?')

def f063(s: Input):
  i = 0
  while i < 3:
    c = s[i]
    i += 1
  assert len(s) == i
