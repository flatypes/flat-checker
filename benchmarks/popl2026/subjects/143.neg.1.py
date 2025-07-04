type Input = lang(r'[^a]*')

def f143(s: Input):
  i = len(s)
  while i > 0:
    inv(0 <= i <= len(s))
    inv('a' not in s[i:])
    if s[i-1] == "a":
      return
    i -= 1
  raise Exception
