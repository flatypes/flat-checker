
def f481(s: Input):
  if s == "":
    return
  i = 0
  while i < len(s)-1:
    inv(0 <= i <= len(s) - 1)
    inv(isinstance(s[i:], Input))
    if s[i] == "1":
      assert s[i+1] != "1"
    else:
      assert s[i] == "0"
    i += 1
  if len(s) > 1:
    if s[i-1] == "1":
      assert s[i] == "0"
    else:
      assert s[i] == "0" or s[i] == "1"
