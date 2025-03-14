type Input = lang(r'.*a.*')

def f145(s: Input):
  i = 0
  f = False
  while i < len(s):
    f = f or s[i] == "a"      
    i += 1
  assert f
