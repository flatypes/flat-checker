
def f530(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(isinstance(s[i:], Input))
    if s[i] == "a":
      assert s[i+1] == "b"
      i += 2
    else:
      i += 1
