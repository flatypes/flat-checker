type Input = lang(r'[^a]*a.*')

def f171(s: Input):
  i = 0
  while i < len(s):
    assert s[i] != "a"
    i += 1
