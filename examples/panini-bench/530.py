type Input = lang(r'([^a]|ab)*')

def f530(s: Input):
  i = 0
  while i < len(s):
    if s[i] == "a":
      assert s[i+1] == "b"
      i += 2
    else:
      i += 1
