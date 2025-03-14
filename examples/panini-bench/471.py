type Input = lang(r'(0|01)*')

def f471(s: Input):
  if s == "":
    return
  assert s[0] == "0"
  i = 0
  while i < len(s)-1:
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
