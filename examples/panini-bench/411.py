type Input = lang(r'(a|b)*')

def f411(s: Input):
  i = 0
  while i < len(s):
    if s[i] == "a":
      i += 1
    else:
      j = 0
      while j < len(s)-i:
        if s[i+j] == "b":
          j += 1
        break
      assert j > 0
      i += j
