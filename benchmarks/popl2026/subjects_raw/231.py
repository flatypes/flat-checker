
def f231(s: Input):
  i = len(s)
  while i > 0:
    i -= 1    
    if s[i] != "b":
      break
  assert s[i] == "a"
