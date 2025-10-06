type Input = lang(r'a.?b')

def f352(s: Input):
  a = ""
  b = ""
  if len(s) == 2:
    a,b = s
  if len(s) == 3:
    a,x,b = s
  assert a == "a"
  assert b == "b"
