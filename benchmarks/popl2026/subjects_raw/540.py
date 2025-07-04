
def f540(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(isinstance(s[i:], Input))
    if s[i] == "a":
      assert s[i+1] == "b"
      assert s[i+2] == "b"
      i += 3
    else:
      i += 1
