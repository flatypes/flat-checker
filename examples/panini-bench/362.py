type Input = lang(r'a.*b')

def f362(s: Input):
  i = 0  
  assert s[i] == "a"
  while i < len(s)-1:
    i += 1
  assert s[i] == "b"
