type Input = lang(r'a([^b]b*)*|([^a]a*)*')

def f302(s: Input):
  a,b = s[0:2]
  assert a == "a"
  assert b == "b"
