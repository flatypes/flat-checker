type Input = lang(r'aa.*')

def f525(s: Input):
  a1,a2 = s[0:2]
  assert a1 == a2
  assert a2 == "a"
