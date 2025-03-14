type Input = lang(r'[01]*011')

def f490(s: Input):
  i = 0
  while i < len(s):
    assert s[i] == "0" or s[i] == "1"
    i += 1  
  assert s[-3:] == "011"
