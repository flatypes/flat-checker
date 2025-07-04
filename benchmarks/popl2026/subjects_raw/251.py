
def f251(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s) + 2)
    inv(isinstance(s[i:], Input))
    a,b = s[i:i+2]
    assert a == "a"
    assert b == "b"
    i += 2
