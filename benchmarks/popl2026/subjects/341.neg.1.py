type Input = lang(r'[^a].*|a(.(ba.)*(b(a|[^a].*)|([^b]b*)*))?')

def f341(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s) + 3)
    inv(isinstance(s[i:], Input))
    a,x,b = s[i:i+3]
    assert a == "a"
    assert b == "b"
    i += 3  # NOTE: original program is problematic
