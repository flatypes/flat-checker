type Input = lang(r'a*')

def f110(s: Input):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    i += 1
